pacman::p_load(shiny, shinydashboard, shinythemes, 
plotly, tidyverse, ggstatsplot, tools)

exam <- read_csv("data/Exam_data.csv")

ui <- navbarPage(
  title = "ShinyIDEA: Interactive Data Exploration and Analysis",
  fluid = TRUE,
  theme=shinytheme("flatly"),
  id = "navbarID",
  tabPanel("Introduction"),
  navbarMenu("Univariate"),
  navbarMenu("Bivariate",
             tabPanel("Between group"),
             tabPanel("Within group"),
             tabPanel("ANOVA",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     selectInput(inputId = "xvariable",
                                                 label = "Select x-variable:",
                                                 choices = c("Class" = "CLASS",
                                                             "Gender" = "GENDER",
                                                             "Race" = "RACE"),
                                                 selected = "GENDER"),
                                     selectInput(inputId = "yvariable",
                                                 label = "Select y-variable:",
                                                 choices = c("English" = "ENGLISH",
                                                              "Maths" = "MATHS",
                                                              "Science" = "SCIENCE"),
                                                 selected = "ENGLISH"),
                                     selectInput(inputId = "test",
                                                 label = "Type of statistical test:",
                                                 choices = c("parametric" = "p",
                                                             "nonparametric" = "np",
                                                             "robust" = "r",
                                                             "Bayes Factor" = "bf"),
                                                 selected = "p"),
                                     selectInput(inputId = "plotType",
                                                 label = "Type of plot:",
                                                 choices = c("boxviolin" = "boxviolin",
                                                             "box" = "box",
                                                             "violin" = "violin"),
                                                 selected = "boxviolin"),
                                     textInput(inputId = "plotTitle",
                                               label = "Plot title",
                                               placeholder = "Enter text to be used as plot title"),
                                     actionButton(inputId = "goButton", 
                                                  "Go!")
                        ),
                        mainPanel(width = 9,
                                  box(
                                    plotOutput("boxplot",
                                               height = "500px",
                                               width = "500px"))
                        )
                      )),
             tabPanel("Correlation",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     selectInput(inputId = "xvariable1",
                                                 label = "Select x-variable:",
                                                 choices = c("English" = "ENGLISH",
                                                             "Maths" = "MATHS",
                                                             "Science" = "SCIENCE"),
                                                 selected = "ENGLISH"),
                                     selectInput(inputId = "yvariable1",
                                                 label = "Select y-variable:",
                                                 choices = c("English" = "ENGLISH",
                                                             "Maths" = "MATHS",
                                                             "Science" = "SCIENCE"),
                                                 selected = "MATHS"),
                                     selectInput(inputId = "test1",
                                                 label = "Type of statistical test:",
                                                 choices = c("parametric" = "p",
                                                             "nonparametric" = "np",
                                                             "robust" = "r",
                                                             "Bayes Factor" = "bf"),
                                                 selected = "p"),
                                     checkboxInput(inputId = "marginal", 
                                                   label = "Display marginal graphs", 
                                                   value = TRUE),
                                     textInput(inputId = "plotTitle1",
                                               label = "Plot title",
                                               placeholder = "Enter text to be used as plot title"),
                                     actionButton(inputId = "goButton1", 
                                                  "Go!")
                        ),
                        mainPanel(width = 9,
                                  box(
                                    plotOutput("corrPlot",
                                               height = "500px",
                                               width = "500px"))
                        )
                      ))
  ),
  navbarMenu("Multivariate",
             tabPanel("Principal Component Analysis"),
             tabPanel("Hierarchical Custering"),
             tabPanel("kmeans Clustering"),
             tabPanel("Multiple Linear Regression"))
)

#========================#
###### Shiny Server ######
#========================#

server <- function(input, output){
  
##### Shiny Server: Between Group Analysis ##### 
  output$boxplot <- renderPlot({
    input$goButton
    set.seed(1234)
    
    ggbetweenstats(
      data = exam,
      x = !!input$xvariable, 
      y = !!input$yvariable,
      type = input$test,
      title = isolate({
        toTitleCase(input$plotTitle)
      }),
      plot.type = input$plotType,
      mean.ci = TRUE, 
      pairwise.comparisons = TRUE, 
      pairwise.display = "s",
      p.adjust.method = "fdr",
      messages = FALSE)
  })

##### Shiny Server: Correlation Analysis #####   
  output$corrPlot <- renderPlot({
    input$goButton1
    set.seed(1234)
    
  ggscatterstats(
    data = exam,
    x = !!input$xvariable1, 
    y = !!input$yvariable1,
    marginal = input$marginal,
    type = input$test1,
    title = isolate({
      toTitleCase(input$plotTitle1)
    }),
    conf.level = 0.95,
    bf.prior = 0.707)
    })
}

shinyApp(ui = ui, server = server)
