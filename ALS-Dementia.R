# Trial Status Column

setwd("/Users/juliatran/Desktop/neuro lab/ALS & dementia/studies")


library(data.table)
library(tidyverse)
library(dplyr)

df <- read.csv("ALS & FTD clinical trials.csv")
head(df)
view(df)
colnames(df)
args(data)



data <- dt %>% mutate ('Trial Status' = ifelse('Trial Status' == "Open", "Success",
                                             ifelse('Trial Status' == "Planned", "Ongoing",
                                               ifelse('Trial Status' == "Completed", "Success",
                                                ifelse('Trial Status' == "Terminated", "Failure",
                                                 ifelse('Trial Status' == "Temporarily Closed", "Ongoing",
                                                 ifelse('Trial Status' == "Closed", "Failure", "N/A")))))))

view(data)

write.csv(data, "ALS&FTD_trial_status.csv", row.names = FALSE)

-----
# Count the amount of successes, failures, and ongoing trials

table(REGRESSION$IDENTIFIER)

-----

# Removal of Non-specified stages
data1 <- filter(data, Trial.Phase != "Other")
data2 <-filter(data1, Trial.Phase != "(N/A)")

view(data2)
  
----------

# Replace of in-between stages
data_1 <- replace(data2, "I/II", "II")

data <- data2 %>% mutate (Trial.Phase = ifelse(Trial.Phase == "I/II", "II",
                                             ifelse(Trial.Phase == "II/III", "III",
                                                   ifelse(Trial.Phase == "II", "II",
                                                          ifelse(Trial.Phase == "I", "I",
                                                                 ifelse(Trial.Phase == "III", "III",
                                                                        ifelse(Trial.Phase == "IV", "IV", "N/A")))))))
view(data)

--------

# Count how many trial statuses are in each phase
sum(data$Trial.Status== 'Success' & data$Trial.Phase == "I")
sum(data$Trial.Status== 'Success' & data$Trial.Phase == "II")
sum(data$Trial.Status== 'Success' & data$Trial.Phase == "III")
sum(data$Trial.Status== 'Success' & data$Trial.Phase == "IV")

sum(data$Trial.Status== 'Failure' & data$Trial.Phase == "I")
sum(data$Trial.Status== 'Failure' & data$Trial.Phase == "II")
sum(data$Trial.Status== 'Failure' & data$Trial.Phase == "III")
sum(data$Trial.Status== 'Failure' & data$Trial.Phase == "IV")

sum(data$Trial.Status== 'Ongoing' & data$Trial.Phase == "I")
sum(data$Trial.Status== 'Ongoing' & data$Trial.Phase == "II")
sum(data$Trial.Status== 'Ongoing' & data$Trial.Phase == "III")
sum(data$Trial.Status== 'Ongoing' & data$Trial.Phase == "IV")

-----------

# End Point

# Load the necessary library
 library(data.table)

# Read the CSV file into a data table
setwd("/Users/juliatran/Desktop/neuro lab/ALS & dementia")
dt <- fread("ALS & FTD clinical trials.csv")

# Print the first few rows to check the data
print(head(data))

# Print column names to check
print(colnames(data))

# Define the keyword(s)
keywords <- c("ALS", "ALSFRS-R")


# Combine keywords into a single regular expression pattern
pattern <- paste(keywords, collapse = "|")
print(pattern)  

# Debug: Print the pattern

new_value <- "primary"

# Define the column name
column_name <- "Primary Endpoint"

# Ensure the column exists and is of type character
if (column_name %in% colnames(dt)) {
  # Convert to character if not already
  dt[, (column_name) := as.character(get(column_name))]
  # Print the first few rows to verify
  print(head(dt[, ..column_name]))
} else {
  stop(paste("The column", column_name, "does not exist in the data table."))
}

# Create a new column based on the combined keyword pattern
dt[, new_column := ifelse(grepl(pattern, get(column_name), ignore.case = TRUE), new_value, NA)]

# Print the first few rows to verify the new column
print(head(dt))

if ("new_column" %in% colnames(dt)) {
  print("New column successfully added.")
} else {
  print("New column not found in the data table.")
}

view(dt)

------
# Endpoint Trial Objective
  keywords <- c("ALS", "ALSFRS-R")


pattern <- paste(keywords, collapse = "|")
print(pattern)  

# Debug: Print the pattern

new_value <- "primary"


column_name <- "Trial Objective"

table(data$new_column)

if (column_name %in% colnames(data)) {
  dt[, (column_name) := as.character(get(column_name))]
  print(head(data[, ..column_name]))
} else {
  stop(paste("The column", column_name, "does not exist in the data table."))
}


dt[, new_column := ifelse(grepl(pattern, get(column_name), ignore.case = TRUE), new_value, NA)]


print(head(dt))

if ("new_column" %in% colnames(dt)) {
  print("New column successfully added.")
} else {
  print("New column not found in the data table.")
}

fwrite(dt, "modified_data_table2.csv")

--- 
# Sponsor column
  setwd("/Users/juliatran/Desktop/neuro lab/ALS & dementia/studies")

sponsor <- read.csv("clinical trials - ALS & FTD.csv")

print(names(sponsor))  

selected_columns <- sponsor[, c("Trial.ID", "Sponsor", "Trial.Phase", "Primary.Tested.Drug", "Primary.Endpoint", "Primary.Endpoint.Details")] 

sponsor_table <- selected_columns[order(selected_columns$Sponsor), ]

write.csv(sponsor_table, "sponsor_clinical_trial_.csv", row.names = FALSE)
  
view(sponsor_table)

-----
  install.packages("openxlsx")  
data5 <- sponsor_table %>% mutate (Trial.Phase = ifelse(Trial.Phase == "I/II", "II",
                                               ifelse(Trial.Phase == "II/III", "III",
                                                      ifelse(Trial.Phase == "II", "II",
                                                             ifelse(Trial.Phase == "I", "I",
                                                                    ifelse(Trial.Phase == "III", "III",
                                                                           ifelse(Trial.Phase == "IV", "IV", "N/A")))))))

view(data5)

write.csv(data5, "sponsor_endpoint_phase_clinical_trial.csv", row.names = FALSE)

--- 
# Count how many have the same sponsor

if (!require("dplyr")) install.packages("dplyr")
library(dplyr)

sponsors <- read.csv("sponsor_clinical_trial_.csv")

sponsor_counts <- sponsors %>%
  group_by(Sponsor) %>%
  summarise(count = n())

tested_drug_counts <- sponsors %>%
  group_by(Primary.Tested.Drug) %>%
  summarise(count = n())


print(sponsor_counts)


view(sponsor_counts)
view(tested_drug_counts)

----
install.packages("readxl")  
library(readxl)

df <- read_excel("sponsor_endpoint_drug_clinical_trial_.xlsx")
view(df)

df %>%
  group_by(Trial.Phase, Primary.Tested.Drug)

df <- read_excel("sponsor_endpoint_drug_clinical_trial_.xlsx")
head(df)
view(df)
colnames(df)
args(data)



data <- dt %>% mutate ('Trial Status' = ifelse('Trial Status' == "Open", "Success",
                                               ifelse('Trial Status' == "Planned", "Ongoing",
                                                      ifelse('Trial Status' == "Completed", "Success",
                                                             ifelse('Trial Status' == "Terminated", "Failure",
                                                                    ifelse('Trial Status' == "Temporarily Closed", "Ongoing",
                                                                           ifelse('Trial Status' == "Closed", "Failure", "N/A")))))))

view(data)

-----
# table - quantifying data
  
setwd("/Users/juliatran/Desktop/neuro lab/ALS & dementia/clinical trials")

phase_endpoint <- read_excel("phases & endpoint ALS & FTD.xlsx")

colnames(phase_endpoint)

selected_columns <- phase_endpoint[, c("Trial.ID", "Phases")] 

view(selected_columns)

de <- phase_endpoint %>%
  distinct(`Primary Tested Drug`, .keep_all = TRUE)  

view(de)

df <-de %>%
  group_by(Phases) %>%
  summarise(count = n())

da <- de %>%
  group_by(Endpoint) %>%
  summarise (count = n())

view(da)

view(df)

write.csv(de, "no_repeats.csv", row.names = FALSE)


write.csv(phase_count, "phase_transitions_ALS&FTD.csv", row.names = FALSE)

------
# Phase transitions

# removal of 'natural product' which was Phase I-IV
62-1=  
  
phase_transitions <- data.frame(Phase=c('Phase I','Phase II','Phase III','Phase IV', 'N/A'),
                                        'Clinical Trials'=c(115, 220,71,59,28),
                                Percentage=c(0.247,0.473,0.153,0.127,0.060))
view(phase_transitions)

---
  write.csv(df, "pri_drug_mech_action.csv", row.names = FALSE)

----
  
  # table - quantifying data

regression <- read_excel("regression_table.xlsx")

colnames(regression)

selected_columns <- regression[, c("TRIAL.ID", "PHASE.TRANSITIONS", "ARMS.RANGE", "COMBO", "GENDER", "DELIVERY.ROUTE.GROUPS", "IDENTIFIER")] 

view(selected_columns)

de <- regression %>%
  distinct(`TRIAL.ID`, .keep_all = TRUE)  

view(de)

df <-de %>%
  group_by(PHASE.TRANSITIONS) %>%
  summarise(count = n())

da <- de %>%
  group_by(ARMS.RANGE) %>%
  summarise (count = n())

view(da)

view(df)




ABB <- regression %>%
  distinct(`TRIAL.ID`, .keep_all = TRUE)  

view(AB)
view(ABB)

view(new_table)

head(new_table)
str(new_table)

AB[AB == "N/A"] <- NA

nrow(new_table[is.na(new_table$Phases) | is.na(new_table$Endpoint),])

nrow(new_table[!is.na(new_table$Phases) | is.na(new_table$Endpoint),])

nrow(new_table[is.na(new_table$Phases) | !is.na(new_table$Endpoint),])

xtabs( ~ PHASE.TRANSITIONS + IDENTIFIER, data=regression)

table(regression$IDENTIFIER)

