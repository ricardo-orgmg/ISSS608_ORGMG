library(shiny)
library(shinyWidgets)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)
library(maps)
library(randomForest)

# Load dataset
fintech <- read.csv("customer_data.csv")

# Data preparation
fintech_clean <- fintech %>%
  mutate(
    gender = case_when(
      gender == 1 ~ "Male",
      gender == 2 ~ "Female",
      gender == 3 ~ "Other",
      TRUE ~ as.character(gender)
    ),
    clv_millions = customer_lifetime_value / 1000000,
    age_bucket = cut(
      age,
      breaks = c(18, 25, 35, 45, 55, 65, Inf),
      labels = c("18–25", "26–35", "36–45", "46–55", "56–65", "65+"),
      right = TRUE
    ),
    gender = factor(gender),
    income_bracket = factor(income_bracket),
    occupation = factor(occupation),
    customer_segment = factor(customer_segment),
    age_bucket = factor(age_bucket)
  ) %>%
  filter(
    !is.na(clv_millions),
    !is.na(churn_probability),
    !is.na(transaction_frequency)
  )

ui <- fluidPage(
  titlePanel("Exploratory Modelling: Retention Risk & Customer Value"),
  
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        "gender",
        "Gender",
        choices = c("All", levels(fintech_clean$gender)),
        selected = "All",
        multiple = FALSE
      ),
      pickerInput(
        "income",
        "Income Bracket",
        choices = c("All", levels(fintech_clean$income_bracket)),
        selected = "All",
        multiple = FALSE
      ),
      pickerInput(
        "occupation",
        "Occupation",
        choices = c("All", levels(fintech_clean$occupation)),
        selected = "All",
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      pickerInput(
        "age_bucket",
        "Age Group",
        choices = c("All", levels(fintech_clean$age_bucket)),
        selected = "All",
        multiple = FALSE
      ),
      br(),
      checkboxInput(
        "run_rf",
        "Run Predictive Model (Random Forest)",
        value = FALSE
      )
    ),
    
    mainPanel(
      fluidRow(
        column(
          6,
          h4("Customer Lifetime Value Distribution"),
          plotOutput("clv_plot", height = "320px")
        ),
        column(
          6,
          h4("Average Churn Risk"),
          plotOutput("churn_plot", height = "320px")
        )
      ),
      
      fluidRow(
        column(
          6,
          h4("Engagement vs Customer Value"),
          plotOutput("engagement_plot", height = "320px")
        ),
        column(
          6,
          h4("Geographic Customer Value"),
          plotOutput("map_plot", height = "320px")
        )
      ),
      
      conditionalPanel(
        condition = "input.run_rf == true",
        fluidRow(
          column(
            12,
            h4("Random Forest: Predicted vs Actual CLV"),
            plotOutput("rf_plot", height = "400px")
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    df <- fintech_clean
    
    if (input$gender != "All") {
      df <- df %>% filter(gender == input$gender)
    }
    
    if (input$income != "All") {
      df <- df %>% filter(income_bracket == input$income)
    }
    
    if (input$occupation != "All") {
      df <- df %>% filter(occupation == input$occupation)
    }
    
    if (input$age_bucket != "All") {
      df <- df %>% filter(age_bucket == input$age_bucket)
    }
    
    df
  })
  
  output$clv_plot <- renderPlot({
    ggplot(
      filtered_data(),
      aes(x = customer_segment, y = clv_millions, fill = customer_segment)
    ) +
      geom_boxplot() +
      scale_fill_brewer(
        palette = "Blues",
        guide = "none"
      ) +
      scale_y_log10(labels = comma) +
      labs(
        x = "Customer Segment",
        y = "CLV (Millions)"
      ) +
      theme_minimal()
  })
  
  output$churn_plot <- renderPlot({
    churn_by_segment <- filtered_data() %>%
      group_by(customer_segment) %>%
      summarise(
        avg_churn = mean(churn_probability, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(avg_churn))
    
    ggplot(
      churn_by_segment,
      aes(x = reorder(customer_segment, avg_churn), y = avg_churn, fill = avg_churn)
    ) +
      geom_col(width = 0.7) +
      geom_text(
        aes(label = scales::label_percent(accuracy = 0.1)(avg_churn)),
        vjust = -0.4,
        size = 4,
        color = "#08306b",
        fontface = "bold"
      ) +
      scale_fill_gradient(
        low = "#c6dbef",
        high = "#08306b",
        guide = "none"
      ) +
      scale_y_continuous(
        labels = scales::label_percent(),
        breaks = seq(0.30, 0.35, 0.01),
        expand = expansion(mult = c(0, 0.08))
      ) +
      coord_cartesian(ylim = c(0.30, 0.35)) +
      labs(
        x = "Customer Segment",
        y = "Average Churn Probability"
      ) +
      theme_minimal()
  })
  
  output$engagement_plot <- renderPlot({
    df <- filtered_data()
    
    x_limit <- quantile(df$transaction_frequency, 0.95, na.rm = TRUE)
    y_limit <- quantile(df$clv_millions, 0.95, na.rm = TRUE)
    
    ggplot(
      df,
      aes(
        x = transaction_frequency,
        y = clv_millions,
        color = customer_segment
      )
    ) +
      geom_point(alpha = 0.4) +
      geom_smooth(
        method = "lm",
        formula = y ~ x,
        se = FALSE
      ) +
      scale_color_brewer(palette = "Blues") +
      coord_cartesian(
        xlim = c(0, x_limit),
        ylim = c(0, y_limit)
      ) +
      labs(
        x = "Transaction Frequency",
        y = "CLV (Millions)",
        color = "Customer Segment"
      ) +
      theme_minimal()
  })
  
  output$map_plot <- renderPlot({
    colombia_map <- map_data("world", "Colombia")
    
    clv_location <- filtered_data() %>%
      filter(
        longitude >= -80, longitude <= -67,
        latitude >= -5, latitude <= 13
      ) %>%
      group_by(city, department, latitude, longitude) %>%
      summarise(
        avg_clv = mean(clv_millions, na.rm = TRUE),
        customers = n(),
        .groups = "drop"
      )
    
    top_cities <- clv_location %>%
      slice_max(avg_clv, n = 3)
    
    ggplot() +
      geom_polygon(
        data = colombia_map,
        aes(x = long, y = lat, group = group),
        fill = "#f7fbff",
        color = "gray80"
      ) +
      geom_point(
        data = clv_location,
        aes(
          x = longitude,
          y = latitude,
          color = avg_clv,
          size = customers
        ),
        alpha = 0.7
      ) +
      geom_text(
        data = top_cities,
        aes(
          x = longitude,
          y = latitude,
          label = city
        ),
        size = 3
      ) +
      scale_color_gradient(
        low = "#9ecae1",
        high = "#08306b"
      ) +
      labs(
        color = "Average CLV",
        size = "Customers"
      ) +
      theme_minimal()
  })
  
  output$rf_plot <- renderPlot({
    req(input$run_rf)
    
    rf_data <- filtered_data() %>%
      select(
        clv_millions,
        transaction_frequency,
        active_products,
        average_transaction_value,
        satisfaction_score,
        nps_score,
        customer_segment,
        age_bucket,
        income_bracket,
        gender,
        occupation
      ) %>%
      na.omit()
    
    validate(
      need(nrow(rf_data) >= 20, "Not enough complete observations to run the Random Forest model for the selected filters.")
    )
    
    set.seed(123)
    
    train_index <- sample(
      1:nrow(rf_data),
      0.7 * nrow(rf_data)
    )
    
    train_data <- rf_data[train_index, ]
    test_data  <- rf_data[-train_index, ]
    
    validate(
      need(nrow(test_data) >= 5, "Not enough test observations after filtering.")
    )
    
    rf_model <- randomForest(
      clv_millions ~ .,
      data = train_data,
      ntree = 200
    )
    
    test_data$predicted_clv <- predict(
      rf_model,
      newdata = test_data
    )
    
    ggplot(
      test_data,
      aes(x = predicted_clv, y = clv_millions)
    ) +
      geom_point(alpha = 0.3, color = "#3182bd") +
      geom_abline(
        slope = 1,
        intercept = 0,
        linetype = "dashed",
        color = "#08306b"
      ) +
      labs(
        x = "Predicted CLV",
        y = "Actual CLV"
      ) +
      theme_minimal()
  })
}

shinyApp(ui, server)