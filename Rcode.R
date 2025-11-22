library(readxl)
#loadrawdata----
data_raw <- read_xlsx("D:/R studio/Shiny/heartattackproject/Heartattackproject/Heartattackproject1/Medicaldataset.xlsx")
#workingcopy for cleaning-----
data <- data_raw    # working copy
View(data)
names(data)
library(dplyr)
library(janitor)
#Cleaning--------
data <- data %>% clean_names()
names(data)
data <- data %>%
  rename(heart_attack = result)
names(data)
summary(data$troponin)
table(data$troponin > 14)
data$troponin_ngL <- data$troponin * 1000
str(data)
data$heart_attack <- as.factor(data$heart_attack)
str(data$heart_attack)
class(data$heart_attack)
data$heart_attack <- factor(data$heart_attack, 
                            levels = c(0, 1),
                            labels = c("negative", "positive"))
str(data$heart_attack)
table(data$heart_attack, useNA = "ifany")
data$heart_attack <- data_raw$Result
data$heart_attack <- as.factor(data$heart_attack)
str(data$heart_attack)
data$heart_attackB <- ifelse(data$heart_attack == 1, 0, 1)
str(data$heart_attackB)
data$heart_attack <- ifelse(data$heart_attack == "negative", 0, 
                            ifelse(data$heart_attack == "positive", 1, NA))
data$heart_attackB <- NULL
summary()
boxplot(data$troponin_ngL, main = "Troponin Boxplot")
boxplot(data$troponin_ngL)
system("git status")
system("git --version")
file.exists(".git")