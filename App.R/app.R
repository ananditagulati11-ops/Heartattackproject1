
file.exists("app_new.R")
readLines("app_new.R", n = 5)
library(shiny)
library(dplyr)
library(ggplot2)
data <- read.csv("cleaned_heart_attack_data.csv")
ui <- fluidPage(
  titlePanel("Heart Attack Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("age", "Age range:",
                  min = min(data$age),
                  max = max(data$age),
                  value = c(min(data$age), max(data$age))),
      checkboxGroupInput("gender", "Gender:",
                         choices = unique(data$gender),
                         selected = unique(data$gender)),
      checkboxGroupInput("attack", "Heart attack status:",
                         choices = unique(data$heart_attack),
                         selected = unique(data$heart_attack))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Troponin histogram", plotOutput("hist_trop")),
        tabPanel("CK-MB histogram", plotOutput("hist_ckmb")),
        tabPanel("Troponin vs CK-MB", plotOutput("scatter_trop_ckmb")),
        tabPanel("Summary table", tableOutput("summary_table"))
      )
    )
  )
)

# 3) Empty server for now (no plots yet)
server <- function(input, output, session) {
  filtered <- reactive({
    data %>%
      filter(
        age >= input$age[1],
        age <= input$age[2],
        gender %in% input$gender,
        heart_attack %in% input$attack
      )
  })
  output$hist_trop <- renderPlot({
    ggplot(filtered(), aes(x = troponin_ngl)) +
      geom_histogram(bins = 30) +
      labs(x = "Troponin (ng/L)", y = "Count")
  })
}

# 4) Start the app
shinyApp(ui, server)