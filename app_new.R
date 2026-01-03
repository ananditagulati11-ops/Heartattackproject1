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

data$heart_attack <- as.logical(data$heart_attack) 

data$heart_attack <- factor(
  data$heart_attack,
  levels = c(FALSE, TRUE),
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
                                
                                sliderInput(
                                  "age", "Age range:",
                                  min = min(data$age, na.rm = TRUE),
                                  max = max(data$age, na.rm = TRUE),
                                  value = c(min(data$age, na.rm = TRUE),
                                            max(data$age, na.rm = TRUE)),
                                  step = 1,
                                  ticks = FALSE
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
                                
                                tableOutput("summaryTable"),
                                br(),
                                hr(),
                                
                                
                                h3("Combined Biomarker Trends Across Age"),
                                
                                
                                checkboxGroupInput(
                                  "age_biomarkers",
                                  "Choose biomarker(s) to display:",
                                  choices = c("Troponin" = "troponin_ngl",
                                              "CK-MB" = "ck_mb"),
                                  selected = c("troponin_ngl", "ck_mb"),
                                  inline = TRUE
                                ),
                                
                                plotOutput("box4", height = "350px"),
                                uiOutput("age_bio_msg")
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
                 paste0("➡️ ", ratio, "× higher ", biomarker, " levels"),
                 paste0("➡️ ", ratio, "× lower ", biomarker, " levels")
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
  
  # -------- PLOT 3: Sensitivity Curve (FIXED: responds to dropdown) --------
  output$box3 <- renderPlot({
    req(input$curve_bio)
    
    tm <- glm(heart_attack ~ troponin_ngl, data = data, family = binomial)
    cm <- glm(heart_attack ~ ck_mb, data = data, family = binomial)
    
    td <- data.frame(
      troponin_ngl = seq(min(data$troponin_ngl, na.rm = TRUE),
                         max(data$troponin_ngl, na.rm = TRUE),
                         length.out = 200)
    )
    td$prob <- predict(tm, newdata = td, type = "response")
    
    cd <- data.frame(
      ck_mb = seq(min(data$ck_mb, na.rm = TRUE),
                  max(data$ck_mb, na.rm = TRUE),
                  length.out = 200)
    )
    cd$prob <- predict(cm, newdata = cd, type = "response")
    
    if (input$curve_bio == "Troponin") {
      ggplot(td, aes(x = troponin_ngl, y = prob)) +
        geom_line(linewidth = 1.2, color = "#0072B2") +
        labs(title = "Biomarker Sensitivity Curve: Troponin",
             x = "Troponin Level", y = "Probability of Heart Attack") +
        theme_minimal()
      
    } else if (input$curve_bio == "CK-MB") {
      ggplot(cd, aes(x = ck_mb, y = prob)) +
        geom_line(linewidth = 1.2, color = "#D55E00") +
        labs(title = "Biomarker Sensitivity Curve: CK-MB",
             x = "CK-MB Level", y = "Probability of Heart Attack") +
        theme_minimal()
      
    } else {
      ggplot() +
        geom_line(data = td, aes(x = troponin_ngl, y = prob),
                  linewidth = 1.2, color = "#0072B2") +
        geom_line(data = cd, aes(x = ck_mb, y = prob),
                  linewidth = 1.2, color = "#D55E00", linetype = "dashed") +
        labs(title = "Biomarker Sensitivity Curve: Troponin vs CK-MB",
             x = "Biomarker Level", y = "Probability of Heart Attack") +
        theme_minimal()
    }
  })
  
  output$biomarker_comp_msg <- renderUI({
    HTML("
      <b>How to interpret this:</b><br>
      • Steeper slope = stronger association with heart attack.<br>
      • Troponin rises earlier → more sensitive.<br>
      • CK-MB rises later → confirms damage.
    ")
  })
  
  # -------- PLOT 4: Combined Biomarker Trends Across Age (Interactive) --------
  output$box4 <- renderPlot({
    df <- filtered(); req(nrow(df) > 0)
    req(!is.null(input$age_biomarkers))
    req(length(input$age_biomarkers) >= 1)
    
    chosen <- input$age_biomarkers
    
    df_long <- df %>%
      select(age, heart_attack, all_of(chosen)) %>%
      pivot_longer(cols = all_of(chosen),
                   names_to = "biomarker",
                   values_to = "value") %>%
      mutate(biomarker = recode(biomarker,
                                "troponin_ngl" = "Troponin",
                                "ck_mb" = "CK-MB"))
    
    ggplot(df_long, aes(
      x = age,
      y = value,
      color = biomarker,
      linetype = heart_attack,
      group = interaction(biomarker, heart_attack)
    )) +
      geom_smooth(method = "loess", se = FALSE, linewidth = 1.3) +
      labs(
        title = "Combined Biomarker Trends Across Age",
        subtitle = "Solid vs dashed lines indicate Heart Attack status",
        x = "Age (years)",
        y = "Biomarker Value",
        color = "Biomarker",
        linetype = "Heart Attack Status"
      ) +
      theme_minimal()
  })
  
  output$age_bio_msg <- renderUI({
    if (is.null(input$age_biomarkers) || length(input$age_biomarkers) == 0) {
      HTML("<b>Please select at least one biomarker to display.</b>")
    } else {
      HTML("
        <b>Interpretation:</b><br>
        • This combined line chart updates based on your biomarker selection.<br>
        • Solid vs dashed lines indicate Heart Attack status.
      ")
    }
  })
}

# ---------------------------------
# RUN THE APP
# ---------------------------------
shinyApp(ui, server)