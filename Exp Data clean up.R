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

print(Experimental)

