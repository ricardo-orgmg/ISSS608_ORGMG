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
    clv_millions = customer_lifetime_value / 1000000,
    age_bucket = cut(
      age,
      breaks = c(18, 25, 35, 45, 55, 65, Inf),
      labels = c("18–25", "26–35", "36–45", "46–55", "56–65", "65+"),
      right = TRUE
    ),
    gender = as.factor(gender),
    income_bracket = as.factor(income_bracket),
    occupation = as.factor(occupation),
    customer_segment = as.factor(customer_segment),
    age_bucket = as.factor(age_bucket)
  ) %>%
  filter(
    !is.na(clv_millions),
    !is.na(churn_probability),
    !is.na(transaction_frequency)
  )

# UI
ui <- fluidPage(
  titlePanel("Exploratory Modelling: Retention Risk & Customer Value"),
  
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        inputId = "gender",
        label = "Gender",
        choices = sort(unique(fintech_clean$gender)),
        selected = sort(unique(fintech_clean$gender)),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE
        )
      ),
      
      pickerInput(
        inputId = "income",
        label = "Income Bracket",
        choices = sort(unique(fintech_clean$income_bracket)),
        selected = sort(unique(fintech_clean$income_bracket)),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE
        )
      ),
      
      pickerInput(
        inputId = "occupation",
        label = "Occupation",
        choices = sort(unique(fintech_clean$occupation)),
        selected = sort(unique(fintech_clean$occupation)),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE,
          `selected-text-format` = "count > 3"
        )
      ),
      
      pickerInput(
        inputId = "age_bucket",
        label = "Age Group",
        choices = levels(fintech_clean$age_bucket),
        selected = levels(fintech_clean$age_bucket),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("CLV Distribution", plotOutput("clv_plot", height = "500px")),
        tabPanel("Churn Risk", plotOutput("churn_plot", height = "500px")),
        tabPanel("Engagement vs CLV", plotOutput("engagement_plot", height = "500px")),
        tabPanel("Geographic CLV Map", plotOutput("map_plot", height = "550px")),
        tabPanel("Random Forest Model", plotOutput("rf_plot", height = "550px"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    fintech_clean %>%
      filter(
        gender %in% input$gender,
        income_bracket %in% input$income,
        occupation %in% input$occupation,
        age_bucket %in% input$age_bucket
      )
  })
  
  output$clv_plot <- renderPlot({
    validate(
      need(nrow(filtered_data()) > 0, "No data available for the selected filters.")
    )
    
    ggplot(
      filtered_data(),
      aes(x = customer_segment, y = clv_millions, fill = customer_segment)
    ) +
      geom_boxplot(alpha = 0.8, outlier.alpha = 0.3, width = 0.6) +
      stat_summary(
        fun = mean,
        geom = "point",
        shape = 18,
        size = 3,
        color = "#08306b"
      ) +
      scale_fill_brewer(palette = "Blues", guide = "none") +
      scale_y_log10(labels = comma) +
      labs(
        title = "Customer Lifetime Value by Customer Segment",
        subtitle = "Distribution shown on logarithmic scale",
        x = "Customer Segment",
        y = "Customer Lifetime Value (Millions, Log Scale)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "gray40"),
        axis.text.x = element_text(angle = 25, hjust = 1),
        panel.grid.minor = element_blank()
      )
  })
  
  output$churn_plot <- renderPlot({
    validate(
      need(nrow(filtered_data()) > 0, "No data available for the selected filters.")
    )
    
    churn_by_segment <- filtered_data() %>%
      group_by(customer_segment) %>%
      summarise(avg_churn = mean(churn_probability, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(avg_churn)) %>%
      mutate(customer_segment = factor(customer_segment, levels = customer_segment))
    
    ggplot(churn_by_segment, aes(x = customer_segment, y = avg_churn, fill = avg_churn)) +
      geom_col(width = 0.7) +
      geom_text(
        aes(label = label_percent(accuracy = 0.1)(avg_churn)),
        vjust = -0.4,
        size = 4,
        fontface = "bold",
        color = "#08306b"
      ) +
      scale_fill_gradient(
        low = "#c6dbef",
        high = "#08306b",
        guide = "none"
      ) +
      scale_y_continuous(
        labels = label_percent(),
        breaks = seq(0.30, 0.35, 0.01),
        expand = expansion(mult = c(0, 0.08))
      ) +
      coord_cartesian(ylim = c(0.30, 0.35)) +
      labs(
        title = "Average Churn Probability by Customer Segment",
        subtitle = "Segments ranked from highest to lowest churn risk",
        x = "Customer Segment",
        y = "Average Churn Probability"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "gray40"),
        axis.text.x = element_text(angle = 25, hjust = 1),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
      )
  })
  
  output$engagement_plot <- renderPlot({
    validate(
      need(nrow(filtered_data()) > 0, "No data available for the selected filters.")
    )
    
    x_limit <- quantile(filtered_data()$transaction_frequency, 0.95, na.rm = TRUE)
    y_limit <- quantile(filtered_data()$clv_millions, 0.95, na.rm = TRUE)
    
    ggplot(
      filtered_data(),
      aes(x = transaction_frequency, y = clv_millions, color = customer_segment)
    ) +
      geom_point(alpha = 0.4, size = 2) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1, alpha = 0.6) +
      scale_color_brewer(palette = "Blues") +
      scale_y_continuous(labels = comma) +
      coord_cartesian(
        xlim = c(0, x_limit),
        ylim = c(0, y_limit)
      ) +
      labs(
        title = "Transaction Frequency vs Customer Lifetime Value",
        subtitle = "Relationship between customer engagement and financial value",
        x = "Transaction Frequency",
        y = "Customer Lifetime Value (Millions)",
        color = "Customer Segment"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "gray40"),
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      )
  })
  
  output$map_plot <- renderPlot({
    validate(
      need(nrow(filtered_data()) > 0, "No data available for the selected filters.")
    )
    
    colombia_map <- map_data("world", region = "Colombia")
    
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
      ) %>%
      mutate(label = city)
    
    top_cities <- clv_location %>%
      slice_max(avg_clv, n = 3)
    
    ggplot() +
      geom_polygon(
        data = colombia_map,
        aes(x = long, y = lat, group = group),
        fill = "#f7fbff",
        color = "gray80",
        linewidth = 0.3
      ) +
      geom_point(
        data = clv_location,
        aes(x = longitude, y = latitude, color = avg_clv, size = customers),
        alpha = 0.75
      ) +
      geom_text(
        data = top_cities,
        aes(x = longitude, y = latitude, label = label),
        size = 3,
        fontface = "bold",
        color = "#08306b",
        vjust = -0.8
      ) +
      scale_color_gradient(
        low = "#9ecae1",
        high = "#08306b",
        labels = comma
      ) +
      scale_size_continuous(range = c(2, 8)) +
      coord_quickmap(
        xlim = c(-80, -67),
        ylim = c(-5, 13)
      ) +
      labs(
        title = "Average Customer Lifetime Value by Location",
        subtitle = "Bubble size represents number of customers; labels show top 3 cities by average CLV",
        color = "Average CLV\n(Millions)",
        size = "Number of\nCustomers"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "gray40"),
        legend.position = "right",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()
      )
  })
  
  output$rf_plot <- renderPlot({
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
      need(nrow(rf_data) > 50, "Not enough complete observations to run the Random Forest model for the selected filters.")
    )
    
    rf_data <- rf_data %>%
      mutate(
        customer_segment = as.factor(customer_segment),
        age_bucket = as.factor(age_bucket),
        income_bracket = as.factor(income_bracket),
        gender = as.factor(gender),
        occupation = as.factor(occupation)
      )
    
    set.seed(123)
    train_index <- sample(1:nrow(rf_data), 0.7 * nrow(rf_data))
    train_data <- rf_data[train_index, ]
    test_data  <- rf_data[-train_index, ]
    
    validate(
      need(nrow(test_data) > 10, "Not enough test observations after filtering.")
    )
    
    rf_model <- randomForest(
      clv_millions ~
        transaction_frequency +
        active_products +
        average_transaction_value +
        satisfaction_score +
        nps_score +
        customer_segment +
        age_bucket +
        income_bracket +
        gender +
        occupation,
      data = train_data,
      ntree = 200,
      importance = TRUE
    )
    
    test_data$predicted_clv <- predict(rf_model, newdata = test_data)
    
    x_limit <- quantile(test_data$predicted_clv, 0.95, na.rm = TRUE)
    y_limit <- quantile(test_data$clv_millions, 0.95, na.rm = TRUE)
    
    ggplot(
      test_data,
      aes(x = predicted_clv, y = clv_millions, color = clv_millions)
    ) +
      geom_point(alpha = 0.35, size = 2) +
      geom_abline(
        slope = 1,
        intercept = 0,
        color = "#08306b",
        linewidth = 1,
        linetype = "dashed"
      ) +
      scale_color_gradient(
        low = "#9ecae1",
        high = "#08306b",
        labels = comma
      ) +
      coord_cartesian(
        xlim = c(0, x_limit),
        ylim = c(0, y_limit)
      ) +
      labs(
        title = "Random Forest: Predicted vs Actual Customer Lifetime Value",
        subtitle = "Dashed line represents perfect prediction",
        x = "Predicted CLV (Millions)",
        y = "Actual CLV (Millions)",
        color = "Actual CLV\n(Millions)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "gray40"),
        panel.grid.minor = element_blank()
      )
  })
}

# Run app
shinyApp(ui = ui, server = server)
