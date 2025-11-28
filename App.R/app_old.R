library(shiny)

ui <- fluidPage(
  titlePanel("Test app")
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)