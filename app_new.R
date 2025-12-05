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
#         p("⚠This represents a HIGH-RISK category.", style="color:#b30000; font-weight:bold;")
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
library(tidyr)

# ---------------------------------
# LOAD & PREPARE DATA
# ---------------------------------
data <- read.csv("cleaned_heart_attack_data.csv")

data$heart_attack <- factor(
  data$heart_attack,
  levels = c(0, 1),
  labels = c("No Attack", "Heart Attack")
)

data$gender <- as.factor(data$gender)

# ---------------------------------
# USER INTERFACE
# ---------------------------------
ui <- navbarPage("Heart Attack Risk Explorer",
                 
                 # ================= MAIN PAGE =================
                 tabPanel("Dashboard",
                          fluidPage(
                            
                            # SLIDER COLOR THEME (optional, looks nicer)
                            tags$style(HTML("
        .irs-bar, .irs-bar-edge, .irs-single { background-color:#2C64C6; border-color:#2C64C6; }
        .irs-slider { border-color:#2C64C6; }
      ")),
                            
                            sidebarLayout(
                              sidebarPanel(
                                h4("Select Patient Characteristics"),
                                
                                # >>> CLEAN AGE SLIDER (NO TICKS) <<<
                                sliderInput(
                                  "age", "Age range:",
                                  min = min(data$age, na.rm = TRUE),
                                  max = max(data$age, na.rm = TRUE),
                                  value = c(min(data$age, na.rm = TRUE),
                                            max(data$age, na.rm = TRUE)),
                                  step = 1,
                                  ticks = FALSE  # removes clutter under slider
                                ),
                                
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
                 tabPanel("Biomarker Patterns",
                          fluidPage(
                            h3("Biomarker Patterns in Heart Attack vs No Attack"),
                            p("These charts compare biomarker levels between patients who suffered a heart attack and those who did not."),
                            p("Higher levels of Troponin or CK-MB often indicate increased cardiac muscle injury."),
                            br(),
                            
                            plotOutput("box1", height = "250px"),
                            plotOutput("box2", height = "250px"),
                            
                            selectInput("curve_bio",
                                        "Select biomarker to visualize:",
                                        choices = c("Troponin", "CK-MB", "Both"),
                                        selected = "Both"),
                            
                            plotOutput("box3"),
                            uiOutput("biomarker_comp_msg"),
                            br(),
                            
                            h3("Continuous Biomarker Trends Across Age"),
                            plotOutput("box4", height = "350px"),
                            uiOutput("age_bio_msg"),
                            br()
                          )
                 )
)

# ---------------------------------
# SERVER LOGIC
# ---------------------------------
server <- function(input, output, session) {
  
  # Filtered dataset using only dashboard selections
  filtered <- reactive({
    data %>%
      filter(age >= input$age[1],
             age <= input$age[2],
             gender %in% input$gender)
  })
  
  # -------- RISK CARD --------
  output$riskCard <- renderUI({
    df <- filtered(); req(nrow(df) > 0)
    prob <- round(sum(df$heart_attack == "Heart Attack") / nrow(df) * 100, 1)
    
    div(style="padding:20px; background:#f5f5f5; border-radius:10px;",
        h4("📊 Estimated Heart Attack Probability"),
        p("Based on people matching your selected criteria:"),
        h2(paste0(prob, "% likelihood")),
        ifelse(
          prob > 50,
          p("⚠️ High Risk Category", style="color:#b30000; font-weight:bold;"),
          p("🟢 Lower Risk Category", style="color:#008000; font-weight:bold;")
        )
    )
  })
  
  # -------- BIOMARKER CARD --------
  output$biomarkerCard <- renderUI({
    df <- filtered(); req(nrow(df) > 0)
    mean_group <- round(mean(df[[input$factor]], na.rm = TRUE), 2)
    mean_total <- round(mean(data[[input$factor]], na.rm = TRUE), 2)
    ratio <- round(mean_group / mean_total, 2)
    
    labs <- c("troponin_ngl"="Troponin","ck_mb"="CK-MB",
              "heart_rate"="Heart Rate","systolic_blood_pressure"="Systolic BP",
              "diastolic_blood_pressure"="Diastolic BP")
    
    biomarker <- labs[[input$factor]]
    
    div(style="padding:20px; background:#e8f1ff; border-radius:10px;",
        h4("🧪 Biomarker Insight:", biomarker),
        p("Average value in selected group:", strong(mean_group)),
        p("Overall dataset average:", strong(mean_total)),
        p(ifelse(ratio > 1,
                 paste0("➡️ **", ratio, "× higher** ", biomarker, " levels"),
                 paste0("➡️ **", ratio, "× lower** ", biomarker, " levels")
        ))
    )
  })
  
  # -------- SUMMARY TABLE --------
  output$summaryTable <- renderTable({
    filtered() %>%
      group_by(heart_attack) %>%
      summarise(
        Count = n(),
        Mean = round(mean(.data[[input$factor]], na.rm = TRUE), 2),
        Median = round(median(.data[[input$factor]], na.rm = TRUE), 2)
      )
  })
  
  # -------- PLOT 1: Troponin --------
  output$box1 <- renderPlot({
    data %>%
      mutate(cat = ifelse(troponin_ngl >= 14, "High Troponin", "Normal Troponin")) %>%
      count(heart_attack, cat) %>%
      ggplot(aes(cat, n, fill=heart_attack)) +
      geom_col(position="dodge") +
      labs(title="Troponin Levels vs Heart Attack",
           x="Troponin Category", y="Count") +
      theme_minimal()
  })
  
  # -------- PLOT 2: CK-MB --------
  output$box2 <- renderPlot({
    data %>%
      mutate(cat = ifelse(ck_mb >= 25, "High CK-MB", "Normal CK-MB")) %>%
      count(heart_attack, cat) %>%
      ggplot(aes(cat, n, fill=heart_attack)) +
      geom_col(position="dodge") +
      labs(title="CK-MB Levels vs Heart Attack",
           x="CK-MB Category", y="Count") +
      theme_minimal()
  })
  
  # -------- PLOT 3: Sensitivity Curve --------
  output$box3 <- renderPlot({
    tm <- glm(heart_attack ~ troponin_ngl, data=data, family=binomial)
    cm <- glm(heart_attack ~ ck_mb, data=data, family=binomial)
    
    td <- data.frame(troponin_ngl=seq(min(data$troponin_ngl),
                                      max(data$troponin_ngl),100))
    td$prob <- predict(tm, td, type="response")
    
    cd <- data.frame(ck_mb=seq(min(data$ck_mb),
                               max(data$ck_mb),100))
    cd$prob <- predict(cm, cd, type="response")
    
    ggplot() +
      geom_line(data=td, aes(troponin_ngl, prob), size=1.2, color="#0072B2") +
      geom_line(data=cd, aes(ck_mb, prob), size=1.2, color="#D55E00", linetype="dashed") +
      labs(title="Biomarker Sensitivity Curve",
           x="Biomarker Level", y="Probability of Heart Attack") +
      theme_minimal()
  })
  
  output$biomarker_comp_msg <- renderUI({
    HTML("
      <b>How to interpret this:</b><br>
      • Steeper slope = stronger association with heart attack.<br>
      • Troponin rises earlier → more sensitive.<br>
      • CK-MB rises later → confirms damage.
    ")
  })
  
  # -------- PLOT 4: CONTINUOUS AGE-BIOMARKER TREND --------
  output$box4 <- renderPlot({
    df <- filtered(); req(nrow(df) > 0)
    
    df <- df %>%
      select(age, heart_attack, troponin_ngl, ck_mb) %>%
      pivot_longer(cols=c(troponin_ngl, ck_mb),
                   names_to="biomarker", values_to="value") %>%
      mutate(biomarker=recode(biomarker,
                              "troponin_ngl"="Troponin",
                              "ck_mb"="CK-MB"))
    
    ggplot(df, aes(age, value, color=biomarker)) +
      geom_smooth(method="loess", se=FALSE, size=1.4) +
      facet_wrap(~heart_attack) +
      labs(
        title="Biomarker Levels Across Age",
        subtitle=paste("Age range selected:", input$age[1], "to", input$age[2]),
        x="Age (years)", y="Biomarker Value",
        color="Biomarker"
      ) +
      theme_minimal()
  })
  
  output$age_bio_msg <- renderUI({
    HTML("
      <b>Interpretation:</b><br>
      • This continuous plot shows how biomarkers change with age.<br>
      • Use the <b>Age Range</b> slider on the Dashboard to explore trends.<br>
      • Troponin increases earlier → sensitive early marker.<br>
      • CK-MB rises later → indicates established myocardial injury.<br>
      • When both are elevated, heart attack likelihood rises sharply.
    ")
  })
}

# ---------------------------------
# RUN THE APP
# ---------------------------------
shinyApp(ui, server)
