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
# END OF CLEANING SCRIPT     
##############################

