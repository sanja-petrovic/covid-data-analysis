library(sparklyr)
library(dplyr)
library(ggplot2)

sc <- spark_connect(master = "local")
spark_get_java()

covid.raw <- spark_read_csv(sc, name="covid", path=".")

covid.clean <- covid.raw %>%
  mutate(across(
    everything(), 
    ~ if_else(. == "Unknown" || . == "Missing" || . == "NA", NA, .),
    .names = "{col}"
  ))

covid.clean <- covid.clean %>%
  filter(
    !is.na(case_month) &
    !is.na(age_group) &
    current_status == "Laboratory-confirmed case"
  )
