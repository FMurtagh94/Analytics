#Students : Brian Ruiz Flynn (D00262271) & Fiachra Murtagh (D00155450)
#getwd()

if(!require("tidyverse"))
  install.packages("tidyverse")
library(tidyverse)

if(!require("esquisse"))
  install.packages("esquisse")
library(esquisse)

if(!require("dplyr"))
  install.packages("dplyr")
library(dplyr)

if (!require("magrittr"))
  install.packages("magrittr")
library(magrittr)

Experimental <- read.csv("GROUP_9_2023_GCA_RESULTS_EXPERIMENTAL.csv")
Control <- read.csv("GROUP_9_2023_GCA_RESULTS_CONTROL.csv")
#Cleaning up the data based on GAD and STAI ranges 

#Cleaning Control Group
Control <- Control %>%
  select(patientID, gender, pretrial_GAD, pretrial_STAI, posttrial_GAD, posttrial_STAI) %>%
  filter(pretrial_GAD >= 0, pretrial_GAD <= 21,
         pretrial_STAI >= 20, pretrial_STAI <= 80,
         posttrial_GAD >= 0, posttrial_GAD <= 21,
         posttrial_STAI >= 20, posttrial_STAI <= 80) %>%
  mutate(gender = ifelse(gender == "F" | gender == "f", "female", gender))
tibble(Control)

#esquisser(Control, viewer = "browser")



#---

#Cleaning Experimental Group
Experimental <- Experimental %>%
  select(patientID, gender, pretrial_GAD, pretrial_STAI, posttrial_GAD, posttrial_STAI) %>%
  filter(pretrial_GAD >= 0, pretrial_GAD <= 21,
         pretrial_STAI >= 20, pretrial_STAI <= 80,
         posttrial_GAD >= 0, posttrial_GAD <= 21,
         posttrial_STAI >= 20, posttrial_STAI <= 80) %>%
  mutate(gender = ifelse(gender == "F" | gender == "f", "female", gender))
tibble(Experimental)

#converting values in csv column to numerical so the cleaning has effect. 
Control$pretrial_GAD <- as.numeric(Control$pretrial_GAD)
Control$pretrial_STAI <- as.numeric(Control$pretrial_STAI)
Control$posttrial_GAD <- as.numeric(Control$posttrial_GAD)
Control$posttrial_STAI <- as.numeric(Control$posttrial_STAI)

Experimental$pretrial_GAD <- as.numeric(Experimental$pretrial_GAD)
Experimental$pretrial_STAI <- as.numeric(Experimental$pretrial_STAI)
Experimental$posttrial_GAD <- as.numeric(Experimental$posttrial_GAD)
Experimental$posttrial_STAI <- as.numeric(Experimental$posttrial_STAI)

#creating Descriptive data tables and variables to act as reference in future formulas

Experimental_summary_table <- tibble(
  Statistic = c("n1", "Minimum", "Maximum", "1st Quartile", "Median", "3rd Quartile", "Mean", "Variance (n-1)", "Standard deviation (n-1)"),
  pretrial_GAD = c(
    length(Experimental$pretrial_GAD),
    min(Experimental$pretrial_GAD),
    max(Experimental$pretrial_GAD),
    quantile(Experimental$pretrial_GAD, 0.25),
    median(Experimental$pretrial_GAD),
    quantile(Experimental$pretrial_GAD, 0.75),
    mean(Experimental$pretrial_GAD),
    var(Experimental$pretrial_GAD),
    sd(Experimental$pretrial_GAD, na.rm = TRUE)
  ),
  pretrial_STAI = c(
    length(Experimental$pretrial_STAI),
    min(Experimental$pretrial_STAI),
    max(Experimental$pretrial_STAI),
    quantile(Experimental$pretrial_STAI, 0.25),
    median(Experimental$pretrial_STAI),
    quantile(Experimental$pretrial_STAI, 0.75),
    mean(Experimental$pretrial_STAI),
    var(Experimental$pretrial_STAI),
    sd(Experimental$pretrial_STAI, na.rm = TRUE)
  ),
  posttrial_GAD = c(
    length(Experimental$posttrial_GAD),
    min(Experimental$posttrial_GAD),
    max(Experimental$posttrial_GAD),
    quantile(Experimental$posttrial_GAD, 0.25),
    median(Experimental$posttrial_GAD),
    quantile(Experimental$posttrial_GAD, 0.75),
    mean(Experimental$posttrial_GAD),
    var(Experimental$posttrial_GAD),
    sd(Experimental$posttrial_GAD, na.rm = TRUE)
  ),
  posttrial_STAI = c(
    length(Experimental$posttrial_STAI),
    min(Experimental$posttrial_STAI),
    max(Experimental$posttrial_STAI),
    quantile(Experimental$posttrial_STAI, 0.25),
    median(Experimental$posttrial_STAI),
    quantile(Experimental$posttrial_STAI, 0.75),
    mean(Experimental$posttrial_STAI),
    var(Experimental$posttrial_STAI),
    sd(Experimental$posttrial_STAI, na.rm = TRUE)
  )
)

Control_summary_table <- tibble(
  Statistic = c("n2", "Minimum", "Maximum", "1st Quartile", "Median", "3rd Quartile", "Mean", "Variance (n-1)", "Standard deviation (n-1)"),
  pretrial_GAD = c(
    length(Control$pretrial_GAD),
    min(Control$pretrial_GAD),
    max(Control$pretrial_GAD),
    quantile(Control$pretrial_GAD, 0.25),
    median(Control$pretrial_GAD),
    quantile(Control$pretrial_GAD, 0.75),
    mean(Control$pretrial_GAD),
    var(Control$pretrial_GAD),
    sd(Control$pretrial_GAD, na.rm = TRUE)
  ),
  pretrial_STAI = c(
    length(Control$pretrial_STAI),
    min(Control$pretrial_STAI),
    max(Control$pretrial_STAI),
    quantile(Control$pretrial_STAI, 0.25),
    median(Control$pretrial_STAI),
    quantile(Control$pretrial_STAI, 0.75),
    mean(Control$pretrial_STAI),
    var(Control$pretrial_STAI),
    sd(Control$pretrial_STAI, na.rm = TRUE)
  ),
  posttrial_GAD = c(
    length(Control$posttrial_GAD),
    min(Control$posttrial_GAD),
    max(Control$posttrial_GAD),
    quantile(Control$posttrial_GAD, 0.25),
    median(Control$posttrial_GAD),
    quantile(Control$posttrial_GAD, 0.75),
    mean(Control$posttrial_GAD),
    var(Control$posttrial_GAD),
    sd(Control$posttrial_GAD, na.rm = TRUE)
  ),
  posttrial_STAI = c(
    length(Control$posttrial_STAI),
    min(Control$posttrial_STAI),
    max(Control$posttrial_STAI),
    quantile(Control$posttrial_STAI, 0.25),
    median(Control$posttrial_STAI),
    quantile(Control$posttrial_STAI, 0.75),
    mean(Control$posttrial_STAI),
    var(Control$posttrial_STAI),
    sd(Control$posttrial_STAI, na.rm = TRUE)
  )
)

##Shapiro test for normal distribution on experimental

#Experimental <- rnorm(88)
Experimental_Norm_Distribution <- rnorm(88)


# Perform Shapiro-Wilk test
shapiro_test_result <- shapiro.test(Experimental_Norm_Distribution)

# Print the result
print(shapiro_test_result)

# Extract p-value
p_value <- shapiro_test_result$p.value

# Check for normality at a significance level of 0.05
if (p_value < 0.05) {
  cat("Reject the null hypothesis.The data is not normally distributed")
} else {
  cat("Failed to reject the null hypothesis. The data follows a normal distribution")
}

##Shapiro test for normal distribution on  control

Control_Norm_Distribution <- rnorm(85)


# Perform Shapiro-Wilk test 
shapiro_test_result <- shapiro.test(Control_Norm_Distribution)

# Print the result
print(shapiro_test_result)

# Extract p-value
p_value <- shapiro_test_result$p.value

# Check for normality at a significance level of 0.05
if (p_value < 0.05) {
  cat("Reject the null hypothesis.The data is not normally distributed")
} else {
  cat("Failed to reject the null hypothesis. The data follows a normal distribution")
}

# as n1>n2 (n1≠n2) unable to do paired sample test. Using Welch t-test

#data pulled from Experimental Summary Table
Experimental_posttrial_GAD <- c(5.000000, 20.000000, 9.000000, 12.000000, 14.000000, 11.954545, 11.584117, 3.403545)
Experimental_posttrial_STAI <- c(45.000000, 72.000000, 53.000000, 59.000000, 63.000000, 58.26136, 49.43665, 7.03112)

#data pulled from Control Summary Table
Control_posttrial_GAD <- c(9.000000, 21.000000, 14.000000, 16.000000, 18.000000, 15.917647, 8.862185, 2.976942)
Control_posttrial_STAI <- c(46.000000, 74.000000, 58.000000, 62.000000, 67.000000, 61.823529, 39.408964, 6.277656)



# Welch's t-test for posttrial_GAD
t_test_posttrial_GAD <- t.test(Experimental_posttrial_GAD, Control_posttrial_GAD, var.equal = FALSE)
cat("\nWelch's t-test for posttrial_GAD:\n")
print(t_test_posttrial_GAD)

# Welch's t-test for posttrial_STAI
t_test_posttrial_STAI <- t.test(Experimental_posttrial_STAI, Control_posttrial_STAI, var.equal = FALSE)
cat("\nWelch's t-test for posttrial_STAI:\n")
print(t_test_posttrial_STAI)


#both the Welch test for posttrial GAD & STAI support the alternative hypothesis which means that VR(Experimental) has delivered a statistical reduction in both GAD and STAI scales.

#Spearman's R Correlation

#Experimental GAD
cor.test(Experimental$pretrial_GAD, Experimental$posttrial_GAD, method = "spearman", exact = FALSE)
#Experimental STAI
cor.test(Experimental$pretrial_STAI, Experimental$posttrial_STAI, method = "spearman", exact = FALSE)

#Control GAD
cor.test(Control$pretrial_GAD, Control$posttrial_GAD, method = "spearman", exact = FALSE)
#Controll STAI
cor.test(Control$pretrial_STAI, Control$posttrial_STAI, method = "spearman", exact = FALSE)




#Graphs ----

#ggplot(Experimental) +
#  aes(x = pretrial_GAD, y = posttrial_GAD) +
#  geom_point(colour = "RED")

#STAI on Scatter Plot

#Control

#ggplot(Control) +
#  aes(x = pretrial_STAI, y = posttrial_STAI) +
#  geom_point(colour = "Control STAI")
plot(Control$pretrial_STAI, Control$posttrial_STAI, main = "Test", col="RED")
abline(lm(Control$posttrial_STAI~Control$pretrial_STAI))

#Experimental
plot(Experimental$pretrial_STAI, Experimental$posttrial_STAI, main = "Test", col="RED")
abline(lm(Experimental$posttrial_STAI~Experimental$pretrial_STAI))

#Bar Charts GAD

#Control
testTable1 <- table(Control$patientID, Control$pretrial_GAD)
barplot(testTable1, main = "Con Groups's Post GAD Scores", xlab = "GAD Score",
        col = "Red")


testTable2 <- table(Experimental$patientID, Experimental$pretrial_GAD)
barplot(testTable2, main = "Exp Groups's Post GAD Scores", xlab = "GAD Score",
        col = "Red")

#Experimental
#barplot((Experimental$gender, Experimental$posttrial_GAD), main = "Exp Groups's Post GAD Scores", xlab = "GAD Score",
        #col = c("Green", "Red"))#, legend = rownames(counts))






