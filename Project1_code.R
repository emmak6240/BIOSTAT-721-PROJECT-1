knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)

#finding my R version
R.version.string

#finding the versions for the R Packages used for this project
target_packages <- c(
  "gtsummary", "gt", "lubridate",
  "dplyr", "tidyverse", "here", "lintr"
)
sapply(target_packages, packageVersion)

#loading the necessary libraries for this project
library(tidyverse)
library(dplyr)
library(gt)
library(gtsummary)
library(lubridate)
library(lintr)
library(here)

#importing the datasets into R
patient <- readr::read_csv(here::here("PatientData-2.csv"))
encounter <- readr::read_csv(here::here("EncounterData-2.csv"))

#getting the overview of what the program looks like
head(patient)
head(encounter)

#merging the two data together through MRN (patient id)
total_data <- merge(patient, encounter, by = "MRN")

#make sure the dates of contact are consistent
total_data <- total_data |>
  mutate(contact_date = mdy(contact_date), DOB = mdy(DOB))

#see if there are any duplicates in MRN after merging and
#compare if the rows between the two initial
#datasets are consistent

#seeing the rows for the patient file
nrow(patient)

#seeing the rows for the encounter file
nrow(encounter)

#seeing the rows for the total file
nrow(total_data)

#confirm the unique IDs present
length(unique(total_data$MRN))

#Finding the Granularity
nrow(total_data)

#Finding the Dimensions
dim(total_data)

#Getting the Details of the variables and the MRN values
names(total_data)
length(unique(total_data$MRN))

#checking for missing values in totalData
colSums(is.na(total_data))

#checking for implausible data to see how the values
#are distributed across totalData
#found "-989" for BMI and DOB year labeled "1889"
summary(total_data)

#see how many BMI's are labeled as "-989" (which is implausible)
sum(total_data$BMI == -989, na.rm = TRUE)  #there are 6

#see how many DOB years are labeled <1900 (which is implausible)
sum(year(total_data$DOB) < 1900, na.rm = TRUE) #there are 17

#changing BMI = -989 to NA as -989 is implausible
total_data <- total_data |>
  mutate(BMI = na_if(BMI, -989))

#confirming whether the 6 implausible values are converted:
#86 initial NA + 6 implausible values = 92 total NAs
summary(total_data$BMI)

#changing the DOB year of "1889" to NA as it is implausible at this moment
total_data |> filter(year(DOB) < 1900) |> distinct(MRN) |> nrow()

#confirming whether the 17 implausible encounters in
#patient's DOB are converted:
# show 17 NAs total with newly modified DOB
#ranging from: 1948-04-11 to 1999-06-05
summary(total_data$DOB)

#calculate how many WBC levels are <1
#==> 14 WBC values were below 1
sum(encounter$WBC < 1, na.rm = TRUE)

#Convert the WBC levels from numerical values
#into categories (Low, Normal, High, Not Taken)
#and converting any WBC levels that are <1 to 1
total_data <- total_data |>
  mutate(WBC = if_else(WBC < 1, 1, WBC),
    #establishing the categories
    WBC_categorize = case_when(
      is.na(WBC) ~ "Not Taken",
      WBC < 3.2  ~ "Low",
      WBC <= 9.8 ~ "Normal",
      WBC > 9.8  ~ "High"
    ),
    #organizing the levels of categories
    WBC_categorize = factor(WBC_categorize,
                            levels = c("Low", "Normal",
                                       "High", "Not Taken"))
  )
total_data

#Establishing my table for WBC levels
wbc_tbl <- total_data |>
  select(WBC_categorize) |>
  tbl_summary(
              statistic = all_categorical() ~ "{n}       ({p}%)") |>
  modify_header(label ~ "**WBC category**", stat_0 ~ "**Samples (%)**") |>
  modify_table_body(~ .x |>
                      # removes the "WBC_categorize" row
                      filter(row_type != "label")) |>
  # removes the "1" footnote marker
  modify_footnote(all_stat_cols() ~ NA)


#Establishing my table for WBC count in
#its categorical order
wbc_tbl |>
  as_gt() |>
  gt::tab_header(
    title = "White Blood Cell (WBC) Count Category",
    subtitle = paste0("Counts (%) of WBC samples: n = ",
      nrow(total_data)
    )
  )

#generating a separate table that extracts
#the necessary information from totalData:
#MRN, race, ethnicity, Financial Class,
#Hypertension, CHF, diabetes)

patient_info <- total_data |>
  arrange(MRN, contact_date) |>
  group_by(MRN) |>
  slice(1) |>
  ungroup() |>
  select(MRN, race, ethnicity, financialclass,
         hypertension, CHF, diabetes)

#Since the data consist of patient information
#from multiple encounter visits, we want to get
#the set of distinctive patients present in
#the dataset
distinct_patients <- distinct(patient_info)

#Reorganize the order of ethnicity ranging
#from the largest ethnicity size to the
#smallest ethnicity size
distinct_patients <- distinct_patients |>
  mutate(race = factor(race, levels = c("White", "Black", "Other")))

#Organize the binary results (Y/N) to ("Yes"/"No")
distinct_patients <- distinct_patients |>
  mutate(across(c(hypertension, CHF, diabetes),
                ~ factor(.x, levels = c("Y", "N"),
                         labels = c("Yes", "No"))))

#Making consistency with the capitalization
#with Hispanic and "N"on-Hispanic
distinct_patients <- distinct_patients |>
  mutate(ethnicity = factor(ethnicity,
                            levels = c("non-Hispanic", "Hispanic"),
                            labels = c("Non-Hispanic", "Hispanic")))

#generating the table with the corresponding
#variables for table
table1 <- distinct_patients |>
  select(race, ethnicity, financialclass,
         hypertension, CHF, diabetes) |>
  tbl_summary(
    type = list(hypertension ~ "categorical",
                CHF ~ "categorical",
                diabetes ~ "categorical"),
    label = list(
      race ~ "Race",
      ethnicity ~ "Ethnicity",
      financialclass ~ "Financial class",
      hypertension ~ "Hypertension",
      CHF ~ "Congestive heart failure",
      diabetes ~ "Diabetes"
    ),
    missing = "ifany",
    statistic = all_categorical() ~ "{n}       ({p}%)"
  ) |>
  modify_header(
                label ~ "**Characteristics**",
                stat_0 ~ "**Number of Patients (%)**") |>
  # removes the "1" footnote
  modify_footnote(all_stat_cols() ~ NA)


#Create Table 1
table1 |>
  as_gt() |>
  gt::tab_header(title = "Table 1",
                 subtitle = paste0("Counts (%) of patients, n = ",
                                   nrow(distinct_patients)))
