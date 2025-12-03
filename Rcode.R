# library(readxl)
# #loadrawdata----
# data_raw <- read_xlsx("D:/R studio/Shiny/heartattackproject/Heartattackproject/Heartattackproject1/Medicaldataset.xlsx")
# #workingcopy for cleaning-----
# data <- data_raw    # working copy
# library(dplyr)
# library(janitor)
# #Cleaning--------
# data <- data %>% clean_names()
# names(data)
# data <- data %>%
#   rename(heart_attack = result)
# names(data)
# summary(data$troponin)
# table(data$troponin > 14)
# data$troponin_ngL <- data$troponin * 1000
# data$heart_attack <- data_raw$Result
# data$heart_attack <- as.factor(data$heart_attack)
# str(data$heart_attack)
# boxplot(data$troponin_ngL, main = "Troponin Boxplot")
# boxplot(data$troponin_ngL)
# 
# install.packages("ggplot2")
# library(ggplot2)
# 
# class("troponin_ngL")
# # data$troponin_ngl <- as.numeric(data$troponin_ngl)
# names(data)
# data$troponin <- as.numeric(data$troponin)
# data$troponin_ngl <- data$troponin * 1000
# 
# hist(data$troponin_ngl, breaks = 50)
# boxplot(data$troponin_ngl)
# boxplot(troponin_ngl ~ heart_attack, data = data)
# head(data$troponin, 20)
# class(data$troponin)
# unique(data$troponin)[1:20]
# data$troponin_ngl <- data$troponin * 1000
# class(data$troponin_ngl)
# hist(data$troponin_ngl)
# boxplot(data$troponin_ngl,
#         main = "Troponin (ng/L) Boxplot",
#         horizontal = TRUE)
# boxplot(troponin_ngl ~ heart_attack, data = data,
#         main = "Troponin Levels by Heart Attack",
#         xlab = "Heart Attack (0 = No, 1 = Yes)",
#         ylab = "Troponin (ng/L)",
#         col = c("lightgreen", "tomato"))
# hist(data$age,
#      main = "Age Distribution",
#      xlab = "Age",
#      col = "skyblue")
# hist(data$ck_mb,
#      main = "CK-MB Distribution",
#      xlab = "CK-MB",
#      col = "orange")
# summary(data$ck_mb)
# boxplot(ck_mb ~ heart_attack, data = data,
#         main = "ck_mb by Heart Attack",
#         xlab = "Heart Attack (0 = No, 1 = Yes)",
#         ylab = "ck_mb",
#         col = c("lightgreen", "tomato"))
# tapply(data$ck_mb, data$heart_attack, summary)
# library(ggplot2)
# 
# ggplot(data, aes(x = ck_mb, fill = factor(heart_attack))) +
#   geom_density(alpha = 0.5) +
#   labs(title = "CK-MB Density by Heart Attack Outcome",
#        x = "CK-MB",
#        fill = "Heart Attack") +
#   xlim(0, 300)
# library(dplyr)
# 
# renv::init(force = TRUE)
# 
# data$gender <- as.factor(data$gender)  # ensure factor
# data$gender <- factor(
#   as.character(data$gender),
#   levels = c("0", "1"),
#   labels = c("Female", "Male")
# )
# str(data$gender)
# str(data$heart_attack)
# data$heart_attack <- factor(data$heart_attack,levels = c("negative", "positive"),labels = c(FALSE, TRUE))
# table(data$gender)
# View(data$heart_attack)
# str(data$heart_attack)
# table(data$heart_attack)
# library(ggplot2)
# boxplot(blood_sugar ~ heart_attack, data = data,
#         main = "Blood Sugar vs Heart Attack",
#         xlab = "Heart Attack",
#         ylab = "Blood Sugar")
# write.csv(data, "cleaned_heart_attack_data.csv", row.names = FALSE)
# class("heart_attack")
# 









##############################
# HEART ATTACK DATA CLEANING #
##############################

# Load required packages
library(readxl)
library(dplyr)
library(janitor)
library(ggplot2)

# Load raw data
install.packages("readxl")
data_raw <- read_xlsx("Medicaldataset.xlsx")

# Create working copy
data <- data_raw

# Clean column names
data <- data %>% clean_names()

# Rename result column to heart_attack
data <- data %>% rename(heart_attack = result)

# Convert troponin to numeric and create troponin in ng/L
data$troponin <- as.numeric(data$troponin)
data$troponin_ngl <- data$troponin * 1000

# Convert gender to Female/Male
# Convert gender 0/1 → Female/Male (numeric safe)
data$gender <- factor(
  as.character(data$gender),
  levels = c("0", "1"),
  labels = c("Female", "Male")
)

# Convert heart_attack values to FALSE/TRUE
data$heart_attack <- factor(
  tolower(trimws(as.character(data$heart_attack))),
  levels = c("negative", "positive"),
  labels = c(FALSE, TRUE)
)

# Inspect structure
str(data)
table(data$gender)
table(data$heart_attack)

# Example plots (optional)
boxplot(troponin_ngl ~ heart_attack, data = data,
        main = "Troponin Levels by Heart Attack",
        xlab = "Heart Attack",
        ylab = "Troponin (ng/L)",
        col = c("lightgreen", "tomato"))

boxplot(blood_sugar ~ heart_attack, data = data,
        main = "Blood Sugar vs Heart Attack",
        xlab = "Heart Attack",
        ylab = "Blood Sugar")
boxplot(ck_mb ~ heart_attack, data = data,
                main = "ck_mb by Heart Attack",
                xlab = "Heart Attack (0 = No, 1 = Yes)",
                ylab = "ck_mb",
                col = c("lightgreen", "tomato"))

# Save cleaned dataset
write.csv(data, "cleaned_heart_attack_data.csv", row.names = FALSE)

##############################
# END OF CLEANING SCRIPT     #
##############################

