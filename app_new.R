# library(shiny)
# library(dplyr)
# 
# # ---------------------------------
# # LOAD & PREPARE DATA
# # ---------------------------------
# data <- read.csv("cleaned_heart_attack_data.csv")
# 
# data$heart_attack <- factor(data$heart_attack,
#                             levels = c(0, 1),
#                             labels = c("No Attack", "Heart Attack"))
# data$gender <- as.factor(data$gender)
# 
# # ---------------------------------
# # USER INTERFACE
# # ---------------------------------
# ui <- fluidPage(
#   titlePanel("Heart Attack Risk Explorer"),
#   
#   sidebarLayout(
#     sidebarPanel(
#       h4("Select Patient Characteristics"),
#       
#       sliderInput("age", "Age range:",
#                   min = min(data$age, na.rm = TRUE),
#                   max = max(data$age, na.rm = TRUE),
#                   value = c(min(data$age, na.rm = TRUE),
#                             max(data$age, na.rm = TRUE))),
#       
#       checkboxGroupInput("gender", "Gender:",
#                          choices = levels(data$gender),
#                          selected = levels(data$gender)),
#       
#       selectInput("factor", "Biomarker / Vital Sign:",
#                   choices = c("Troponin" = "troponin_ngl",
#                               "CK-MB" = "ck_mb",
#                               "Heart Rate" = "heart_rate",
#                               "Blood Pressure" = "blood_pressure"),
#                   selected = "troponin_ngl")
#     ),
#     
#     mainPanel(
#       h3("Personalized Heart Attack Risk Assessment"),
#       
#       uiOutput("riskCard"),
#       br(),
#       uiOutput("biomarkerCard"),
#       br(),
#       tableOutput("summaryTable")
#     )
#   )
# )
# 
# # ---------------------------------
# # SERVER
# # ---------------------------------
# server <- function(input, output, session) {
#   
#   filtered <- reactive({
#     data %>%
#       filter(age >= input$age[1],
#              age <= input$age[2],
#              gender %in% input$gender)
#   })
#   
#   # ==========================================
#   # 1) RISK CARD (PROBABILITY DISPLAY)
#   # ==========================================
#   output$riskCard <- renderUI({
#     df <- filtered()
#     req(nrow(df) > 0)
#     
#     total <- nrow(df)
#     attacks <- sum(df$heart_attack == "Heart Attack")
#     prob <- round((attacks / total) * 100, 1)
#     
#     div(
#       style = "padding:20px; background:#f5f5f5; border-radius:10px;",
#       h4("📊 Estimated Heart Attack Probability"),
#       p("Based on people matching your selected criteria:"),
#       h2(paste0(prob, "% likelihood")),
#       if (prob > 50)
#         p("⚠️ This represents a HIGH-RISK category.", style="color:#b30000; font-weight:bold;")
#       else
#         p("🟢 This represents a LOWER-RISK category.", style="color:#008000; font-weight:bold;")
#     )
#   })
#   
#   # ==========================================
#   # 2) BIOMARKER INTERPRETATION CARD
#   # ==========================================
#   output$biomarkerCard <- renderUI({
#     df <- filtered()
#     req(nrow(df) > 0)
#     
#     mean_group <- round(mean(df[[input$factor]], na.rm = TRUE), 2)
#     mean_global <- round(mean(data[[input$factor]], na.rm = TRUE), 2)
#     ratio <- round(mean_group / mean_global, 2)
#     
#     biomarker_name <- names(which(c(
#       "troponin_ngl" = "Troponin",
#       "ck_mb" = "CK-MB",
#       "heart_rate" = "Heart Rate",
#       "blood_pressure" = "Blood Pressure"
#     ) == input$factor))
#     
#     div(
#       style = "padding:20px; background:#e8f1ff; border-radius:10px;",
#       h4("🧪 Biomarker Insight: ", biomarker_name),
#       
#       p("Average value in your selected group:",
#         strong(mean_group)),
#       p("Overall dataset average:",
#         strong(mean_global)),
#       
#       p(if (ratio > 1)
#         paste0("➡️ This group shows **", ratio,
#                "× HIGHER** ", biomarker_name,
#                " levels compared to the general population.")
#         else
#           paste0("➡️ This group shows **", ratio,
#                  "× LOWER** ", biomarker_name,
#                  " levels compared to the general population."))
#     )
#   })
#   
#   # ==========================================
#   # 3) SUMMARY TABLE FOR GROUP COMPARISON
#   # ==========================================
#   output$summaryTable <- renderTable({
#     df <- filtered()
#     req(nrow(df) > 0)
#     
#     df %>%
#       group_by(heart_attack) %>%
#       summarise(
#         Count = n(),
#         Mean_Selected_Factor = round(mean(.data[[input$factor]], na.rm = TRUE), 2),
#         Median_Selected_Factor = round(median(.data[[input$factor]], na.rm = TRUE), 2)
#       )
#   })
# }
# 
# # ---------------------------------
# # RUN APP
# # ---------------------------------
# shinyApp(ui, server)
library(shiny)
library(dplyr)
library(ggplot2)

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
ui <- navbarPage("Heart Attack Risk Explorer",
                 
                 # ================= MAIN PAGE =================
                 tabPanel("Dashboard",
                          fluidPage(
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
                                                        "Systolic BP" = "systolic_blood_pressure",
                                                        "Diastolic BP" = "diastolic_blood_pressure"),
                                            selected = "troponin_ngl"),
                                
                                br()
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
                 ),
                 
                 # ================= PLOT PAGE =================
                 tabPanel("Boxplot Analysis",
                          fluidPage(
                            h3("Biomarker Distribution Overview"),
                            p("These boxplots display how key biomarkers differ between patients who had a heart attack and those who did not."),
                            p("Higher biomarker values—such as Troponin or CK-MB—often indicate increased cardiac muscle injury."),
                            br(),
                            
                            plotOutput("box1", height = "250px"),
                            plotOutput("box2", height = "250px"),
                            plotOutput("box3", height = "250px"),
                            
                            br()
                          )
                 )
)

# ---------------------------------
# SERVER LOGIC
# ---------------------------------
server <- function(input, output, session) {
  
  # Navigation between tabs
  observeEvent(input$toPlots, {
    updateTabsetPanel(session, "tabs", selected = "Boxplot Analysis")
  }, ignoreInit = TRUE)
  
  observeEvent(input$goBack, {
    updateTabsetPanel(session, "tabs", selected = "Dashboard")
  }, ignoreInit = TRUE)
  
  # Fix tab panel switching logic
  observe({
    updateNavbarPage(session, inputId = "tabs", selected = ifelse(input$toPlots > 0, "Boxplot Analysis", "Dashboard"))
  })
  
  # Filtered dataset
  filtered <- reactive({
    data %>%
      filter(age >= input$age[1],
             age <= input$age[2],
             gender %in% input$gender)
  })
  
  # ----------------------------------------
  # RISK CARD
  # ----------------------------------------
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
  
  # ----------------------------------------
  # BIOMARKER CARD
  # ----------------------------------------
  output$biomarkerCard <- renderUI({
    df <- filtered()
    req(nrow(df) > 0)
    
    mean_group <- round(mean(df[[input$factor]], na.rm = TRUE), 2)
    print(strong(mean_group))
    mean_global <- round(mean(data[[input$factor]], na.rm = TRUE), 2)
    print(strong(mean_global))
    ratio <- round(mean_group / mean_global, 2)
    print(ratio)
    
    factor_names <- c("troponin_ngl" = "Troponin",
                      "ck_mb" = "CK-MB",
                      "heart_rate" = "Heart Rate",
                      "Systolic BP" = "systolic_blood_pressure",
                      "Diastolic BP" = "diastolic_blood_pressure")
    
    biomarker <- factor_names[[input$factor]]
    
    div(
      style = "padding:20px; background:#e8f1ff; border-radius:10px;",
      h4("🧪 Biomarker Insight: ", biomarker),
      p("Average value in your selected group:", strong(mean_group)),
      p("Overall dataset average:", strong(mean_global)),
      p(if (ratio > 1)
        paste0("➡️ This group shows **", ratio, "× HIGHER** ", biomarker, " levels.")
        else
          paste0("➡️ This group shows **", ratio, "× LOWER** ", biomarker, " levels."))
    )
  })
  
  # ----------------------------------------
  # SUMMARY TABLE
  # ----------------------------------------
  output$summaryTable <- renderTable({
    filtered() %>%
      group_by(heart_attack) %>%
      summarise(
        Count = n(),
        Mean_Selected_Factor = round(mean(.data[[input$factor]], na.rm = TRUE), 2),
        Median_Selected_Factor = round(median(.data[[input$factor]], na.rm = TRUE), 2)
      )
  })
  
  # ----------------------------------------
  # BOX PLOTS PAGE
  # ----------------------------------------
  output$box1 <- renderPlot({
    data %>%
      mutate(
        troponin_cat = ifelse(troponin_ngl >= 14,
                              "High Troponin (Possible Heart Injury)",
                              "Normal Troponin"),
        troponin_cat = factor(troponin_cat, levels = c("Normal Troponin", "High Troponin (Possible Heart Injury)"))
      ) %>%
      count(heart_attack, troponin_cat) %>%
      ggplot(aes(x = troponin_cat, y = n, fill = heart_attack)) +
      geom_col(position = "dodge") +
      labs(
        title = "Troponin Levels and Heart Attack Risk",
        subtitle = "High Troponin usually indicates heart muscle damage",
        x = "Troponin Category",
        y = "Number of People",
        fill = "Outcome"
      ) +
      theme_minimal(base_size = 14)
  })
  
  # =======================
  # 2) CK-MB Risk Plot
  # =======================
  output$box2 <- renderPlot({
    data %>%
      mutate(
        ckmb_cat = ifelse(ck_mb >= 25,
                          "High CK-MB (Muscle Damage)",
                          "Normal CK-MB"),
        ckmb_cat = factor(ckmb_cat, levels = c("Normal CK-MB", "High CK-MB (Muscle Damage)"))
      ) %>%
      count(heart_attack, ckmb_cat) %>%
      ggplot(aes(x = ckmb_cat, y = n, fill = heart_attack)) +
      geom_col(position = "dodge") +
      labs(
        title = "CK-MB Levels and Heart Attack Risk",
        subtitle = "Elevated CK-MB suggests damage to heart or skeletal muscle",
        x = "CK-MB Category",
        y = "Number of People",
        fill = "Outcome"
      ) +
      theme_minimal(base_size = 14)
  })
  
  # =======================
  # 3) Heart Rate Risk Plot
  # =======================
  # 3) Systolic Blood Pressure Risk Plot
  # =======================
  output$box3 <- renderPlot({
    data %>%
      mutate(
        sbp_cat = ifelse(systolic_blood_pressure >= 140,
                         "High Systolic BP (Hypertension)",
                         "Normal Systolic BP"),
        sbp_cat = factor(sbp_cat,
                         levels = c("Normal Systolic BP",
                                    "High Systolic BP (Hypertension)"))
      ) %>%
      count(heart_attack, sbp_cat) %>%
      ggplot(aes(x = sbp_cat, y = n, fill = heart_attack)) +
      geom_col(position = "dodge") +
      labs(
        title = "Systolic Blood Pressure and Heart Attack Risk",
        subtitle = "BP ≥140 mmHg increases the strain on the arteries and heart",
        x = "Systolic Blood Pressure Category",
        y = "Number of People",
        fill = "Outcome"
      ) +
      theme_minimal(base_size = 14)
  })
}

# ---------------------------------
# RUN APP
# ---------------------------------
shinyApp(ui, server)
