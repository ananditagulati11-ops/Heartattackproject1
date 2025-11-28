library(shiny)
library(dplyr)

# ---------------------------------
# LOAD & PREPARE DATA
# ---------------------------------
data <- read.csv("cleaned_heart_attack_data.csv")

data$heart_attack <- factor(data$heart_attack,
                            levels = c(0, 1),
                            labels = c("No Attack", "Heart Attack"))
data$gender <- as.factor(data$gender)

# ---------------------------------
# USER INTERFACE
# ---------------------------------
ui <- fluidPage(
  titlePanel("Heart Attack Risk Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Select Patient Characteristics"),
      
      sliderInput("age", "Age range:",
                  min = min(data$age, na.rm = TRUE),
                  max = max(data$age, na.rm = TRUE),
                  value = c(min(data$age, na.rm = TRUE),
                            max(data$age, na.rm = TRUE))),
      
      checkboxGroupInput("gender", "Gender:",
                         choices = levels(data$gender),
                         selected = levels(data$gender)),
      
      selectInput("factor", "Biomarker / Vital Sign:",
                  choices = c("Troponin" = "troponin_ngl",
                              "CK-MB" = "ck_mb",
                              "Heart Rate" = "heart_rate",
                              "Blood Pressure" = "blood_pressure"),
                  selected = "troponin_ngl")
    ),
    
    mainPanel(
      h3("Personalized Heart Attack Risk Assessment"),
      
      uiOutput("riskCard"),
      br(),
      uiOutput("biomarkerCard"),
      br(),
      tableOutput("summaryTable")
    )
  )
)

# ---------------------------------
# SERVER
# ---------------------------------
server <- function(input, output, session) {
  
  filtered <- reactive({
    data %>%
      filter(age >= input$age[1],
             age <= input$age[2],
             gender %in% input$gender)
  })
  
  # ==========================================
  # 1) RISK CARD (PROBABILITY DISPLAY)
  # ==========================================
  output$riskCard <- renderUI({
    df <- filtered()
    req(nrow(df) > 0)
    
    total <- nrow(df)
    attacks <- sum(df$heart_attack == "Heart Attack")
    prob <- round((attacks / total) * 100, 1)
    
    div(
      style = "padding:20px; background:#f5f5f5; border-radius:10px;",
      h4("📊 Estimated Heart Attack Probability"),
      p("Based on people matching your selected criteria:"),
      h2(paste0(prob, "% likelihood")),
      if (prob > 50)
        p("⚠️ This represents a HIGH-RISK category.", style="color:#b30000; font-weight:bold;")
      else
        p("🟢 This represents a LOWER-RISK category.", style="color:#008000; font-weight:bold;")
    )
  })
  
  # ==========================================
  # 2) BIOMARKER INTERPRETATION CARD
  # ==========================================
  output$biomarkerCard <- renderUI({
    df <- filtered()
    req(nrow(df) > 0)
    
    mean_group <- round(mean(df[[input$factor]], na.rm = TRUE), 2)
    mean_global <- round(mean(data[[input$factor]], na.rm = TRUE), 2)
    ratio <- round(mean_group / mean_global, 2)
    
    biomarker_name <- names(which(c(
      "troponin_ngl" = "Troponin",
      "ck_mb" = "CK-MB",
      "heart_rate" = "Heart Rate",
      "blood_pressure" = "Blood Pressure"
    ) == input$factor))
    
    div(
      style = "padding:20px; background:#e8f1ff; border-radius:10px;",
      h4("🧪 Biomarker Insight: ", biomarker_name),
      
      p("Average value in your selected group:",
        strong(mean_group)),
      p("Overall dataset average:",
        strong(mean_global)),
      
      p(if (ratio > 1)
        paste0("➡️ This group shows **", ratio,
               "× HIGHER** ", biomarker_name,
               " levels compared to the general population.")
        else
          paste0("➡️ This group shows **", ratio,
                 "× LOWER** ", biomarker_name,
                 " levels compared to the general population."))
    )
  })
  
  # ==========================================
  # 3) SUMMARY TABLE FOR GROUP COMPARISON
  # ==========================================
  output$summaryTable <- renderTable({
    df <- filtered()
    req(nrow(df) > 0)
    
    df %>%
      group_by(heart_attack) %>%
      summarise(
        Count = n(),
        Mean_Selected_Factor = round(mean(.data[[input$factor]], na.rm = TRUE), 2),
        Median_Selected_Factor = round(median(.data[[input$factor]], na.rm = TRUE), 2)
      )
  })
}

# ---------------------------------
# RUN APP
# ---------------------------------
shinyApp(ui, server)
