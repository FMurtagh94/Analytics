#install dplyr & tidyverse
install.packages("tidyverse")
install.packages("dplyr")

library(dplyr)
library(tidyverse)

Experimental <- read.csv("GROUP_9_2023_GCA_RESULTS_EXPERIMENTAL.csv")

#Cleaning up the data based on GAD and STAI ranges

remove_outliers_column <- function(column){
  lower_limit <- 0
  upper_limit <- 20
  
  return(column[column >= lower_limit & column <= upper_limit])
  
}

remove_outliers_column2 <- function(column){
  lower_limit <- 20
  upper_limit <- 80
  
  return(column[column >= lower_limit & column <= upper_limit])
  
}

Experimental$pretrial_GAD_no_outliers <- remove_outliers_column(Experimental$pretrial_GAD)
Experimental$posttrial_GAD_no_outliers <- remove_outliers_column(Experimental$posttrial_GAD)
Experimental$pretrial_STAI_no_outliers <- remove_outliers_column(Experimental$pretrial_STAI)
Experimental$posttrial_STAI_no_outliers <- remove_outliers_column(Experimental$posttrial_STAI)

print(Experimental)

