library(shiny)
library(ggplot2)
library(dplyr)

# Load cleaned data
data <- read.csv("cleaned_heart_attack_data.csv")

# UI ----
ui <- fluidPage(
  titlePanel("Heart Attack Data Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("age", "Select Age Range:",
                  min = min(data$age),
                  max = max(data$age),
                  value = c(min(data$age), max(data$age))),
      
      checkboxGroupInput("gender", "Select Gender:",
                         choices = levels(data$gender),
                         selected = levels(data$gender)),
      
      sliderInput("troponin", "Troponin (ng/L):",
                  min = min(data$troponin_ngl),
                  max = max(data$troponin_ngl),
                  value = c(min(data$troponin_ngl), max(data$troponin_ngl))),
      
      sliderInput("ckmb", "CK-MB:",
                  min = min(data$ck_mb),
                  max = max(data$ck_mb),
                  value = c(min(data$ck_mb), max(data$ck_mb))),
      
      checkboxGroupInput("attack", "Heart Attack Status:",
                         choices = levels(data$heart_attack),
                         selected = levels(data$heart_attack))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Troponin Histogram", plotOutput("plotTroponin")),
        tabPanel("CK-MB Histogram", plotOutput("plotCKMB")),
        tabPanel("Troponin Boxplot", plotOutput("boxTroponin")),
        tabPanel("CK-MB Boxplot", plotOutput("boxCKMB")),
        tabPanel("Summary Table", tableOutput("summary"))
      )
    )
  )
)

# SERVER ----
server <- function(input, output) {
  # Empty for now — no error
}

# RUN APP ----
shinyApp(ui = ui, server = server)
