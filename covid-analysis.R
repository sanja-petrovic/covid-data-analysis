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

age_group_order <- c("0 - 17 years", "18 to 49 years", "50 to 64 years", "65+ years")

age_group_cases <- covid.clean %>%
  group_by(age_group) %>%
  summarise(case_count = n()) %>%
  arrange(desc(age_group))

age_group_cases_df <- collect(age_group_cases)
age_group_cases_df$age_group <- factor(age_group_cases_df$age_group, levels = age_group_order)

ggplot(age_group_cases_df, aes(x = reorder(age_group, -case_count), y = case_count)) +
  geom_bar(stat = "identity") +
  labs(title = "COVID-19 Cases by Age Group", x = "Age Group", y = "Number of Cases") +
  theme_minimal() +
  scale_x_discrete(limits = age_group_order)

