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
View(data)
install.packages("ggplot2")
library(ggplot2)
View(data)
str(data)
summary(data)
hist(data$troponin_ngl,
     breaks = 50,
     main = "Troponin Levels (ng/L)",
     xlab = "Troponin (ng/L)",
     col = "lightblue")
hist(as.numeric(data$troponin_ngl),
     breaks = 50,
     main = "Troponin Levels (ng/L)",
     xlab = "Troponin (ng/L)",
     col = "lightblue")
class("troponin_ngL")
data$troponin_ngl <- as.numeric(data$troponin_ngl)
names(data)
length(data$troponin_ngl)
data$troponin <- as.numeric(data$troponin)
data$troponin_ngl <- data$troponin * 1000
str(data)
data$troponin
summary(data$troponin_ngl)
str(data)
summary(data$troponin_ngl)
head(data$Troponin, 10)
data$troponin_ngL <- NULL
data$troponin_ngl <- NULL
hist(data$troponin_ngl, breaks = 50)
boxplot(data$troponin_ngl)
boxplot(troponin_ngl ~ heart_attack, data = data)
head(data$troponin, 20)
class(data$troponin)
unique(data$troponin)[1:20]
data$troponin_ngl <- data$troponin * 1000
class(data$troponin_ngl)
hist(data$troponin_ngl)
boxplot(data$troponin_ngl,
        main = "Troponin (ng/L) Boxplot",
        horizontal = TRUE)
boxplot(troponin_ngl ~ heart_attack, data = data,
        main = "Troponin Levels by Heart Attack",
        xlab = "Heart Attack (0 = No, 1 = Yes)",
        ylab = "Troponin (ng/L)",
        col = c("lightgreen", "tomato"))
hist(data$age,
     main = "Age Distribution",
     xlab = "Age",
     col = "skyblue")
hist(data$ck_mb,
     main = "CK-MB Distribution",
     xlab = "CK-MB",
     col = "orange")
summary(data$ck_mb)
boxplot(ck_mb ~ heart_attack, data = data,
        main = "ck_mb by Heart Attack",
        xlab = "Heart Attack (0 = No, 1 = Yes)",
        ylab = "ck_mb",
        col = c("lightgreen", "tomato"))
tapply(data$ck_mb, data$heart_attack, summary)
library(ggplot2)

ggplot(data, aes(x = ck_mb, fill = factor(heart_attack))) +
  geom_density(alpha = 0.5) +
  labs(title = "CK-MB Density by Heart Attack Outcome",
       x = "CK-MB",
       fill = "Heart Attack") +
  xlim(0, 300)
library(dplyr)
datashiny <- data %>% select(age, gender, heart_rate,
                        systolic_blood_pressure, diastolic_blood_pressure,
                        blood_sugar, ck_mb, troponin_ngl, heart_attack)
View(datashiny)
renv:: init()
renv::snapshot()
list.files(all.files = TRUE)
renv::snapshot()
getwd()
renv::init(force = TRUE)
list.files(all.files = TRUE)
system("git status")
data$gender <- factor(
  data$gender,
  levels = c(0, 1),
  labels = c("Female", "Male")
)
str(data$gender)
str(data$heart_attack)
data$heart_attack <- factor(
  data$heart_attack,
  levels = c(0, 1),
  labels = c("No Attack", "Attack")
)
View(data$heart_attack)
str(data$heart_attack)
table(data$heart_attack)
library(ggplot2)
boxplot(blood_sugar ~ heart_attack, data = data,
        main = "Blood Sugar vs Heart Attack",
        xlab = "Heart Attack",
        ylab = "Blood Sugar")
write.csv(data, "cleaned_heart_attack_data.csv", row.names = FALSE)