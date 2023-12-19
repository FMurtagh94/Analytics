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
#Cleaning up the data based on GAD and STAI ranges only outliers found in Posttrial STAI.

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
print(summary_table_control)

