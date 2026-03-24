library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)
library(maps)

# Load dataset
fintech <- read.csv("customer_data.csv")

# Data preparation
fintech_clean <- fintech %>%
  mutate(
    
    # Convert CLV to millions
    clv_millions = customer_lifetime_value / 1000000,
    
    # Create age buckets
    age_bucket = cut(
      age,
      breaks = c(18, 25, 35, 45, 55, 65, Inf),
      labels = c("18–25", "26–35", "36–45", "46–55", "56–65", "65+"),
      right = TRUE
    )
    
  ) %>%
  filter(
    !is.na(clv_millions),
    !is.na(churn_probability),
    !is.na(transaction_frequency)
  )

# Define UI
ui <- fluidPage(
  
  titlePanel("Exploratory Modelling: Retention Risk & Customer Value"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("gender",
                  "Gender",
                  choices = unique(fintech$gender)),
      
      selectInput("income",
                  "Income Bracket",
                  choices = unique(fintech$income_bracket)),
      
      selectInput("occupation",
                  "Occupation",
                  choices = unique(fintech$occupation))
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel("CLV Distribution",
                 plotOutput("clv_plot")),
        
        tabPanel("Churn Risk",
                 plotOutput("churn_plot")),
        
        tabPanel("Engagement vs CLV",
                 plotOutput("engagement_plot")),
        
        tabPanel("Geographic CLV Map",
                 plotOutput("map_plot"))
        
      )
      
    )
    
  )
)

# Server logic
server <- function(input, output) {
  
  filtered_data <- reactive({
    
    fintech_clean %>%
      filter(
        gender %in% input$gender,
        income_bracket %in% input$income,
        occupation %in% input$occupation
      )
    
  })
  
  output$clv_plot <- renderPlot({
    
    ggplot(filtered_data(),
           aes(x = customer_segment, y = clv_millions)) +
      geom_boxplot() +
      labs(
        title = "Customer Lifetime Value by Customer Segment",
        x = "Customer Segment",
        y = "CLV (Millions)"
      ) +
      theme_minimal()
    
  })
  
  output$churn_plot <- renderPlot({
    
    churn_by_segment <- filtered_data() %>%
      group_by(customer_segment) %>%
      summarise(avg_churn = mean(churn_probability, na.rm = TRUE))
    
    ggplot(churn_by_segment,
           aes(x = reorder(customer_segment, avg_churn),
               y = avg_churn,
               fill = avg_churn)) +
      geom_col() +
      geom_text(
        aes(label = scales::percent(avg_churn, accuracy = 0.1)),
        vjust = -0.5
      ) +
      scale_fill_gradient(
        low = "lightblue",
        high = "darkblue",
        guide = "none"
      ) +
      scale_y_continuous(labels = scales::percent) +
      labs(
        title = "Average Churn Probability by Customer Segment",
        x = "Customer Segment",
        y = "Average Churn Probability"
      ) +
      theme_minimal()
    
  })
  
  output$engagement_plot <- renderPlot({
    
    ggplot(filtered_data(),
           aes(x = transaction_frequency,
               y = clv_millions,
               color = customer_segment)) +
      geom_point(alpha = 0.5) +
      labs(
        title = "Transaction Frequency vs Customer Lifetime Value",
        x = "Transaction Frequency",
        y = "CLV (Millions)",
        color = "Customer Segment"
      ) +
      theme_minimal()
    
  })
  output$map_plot <- renderPlot({
    
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
      slice_max(avg_clv, n = 5)
    
    ggplot() +
      geom_polygon(
        data = colombia_map,
        aes(x = long, y = lat, group = group),
        fill = "gray95",
        color = "white"
      ) +
      
      geom_point(
        data = clv_location,
        aes(x = longitude, y = latitude,
            color = avg_clv,
            size = customers),
        alpha = 0.7
      ) +
      
      geom_text(
        data = top_cities,
        aes(x = longitude, y = latitude, label = label),
        size = 3,
        vjust = -0.8
      ) +
      
      scale_color_gradient(
        low = "#9ecae1",
        high = "#08306b"
      ) +
      
      coord_quickmap(
        xlim = c(-80, -67),
        ylim = c(-5, 13)
      ) +
      
      labs(
        title = "Average Customer Lifetime Value by Location",
        color = "Average CLV",
        size = "Customers"
      ) +
      
      theme_minimal()
    
  })
}

# Run the app
shinyApp(ui = ui, server = server)


