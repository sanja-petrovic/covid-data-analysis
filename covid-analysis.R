# DATA PREP

library(sparklyr)
library(dplyr)
library(ggplot2)
library(maps)

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
    !is.na(res_state) &
    current_status == "Laboratory-confirmed case"
  )

# INITIAL DATA ANALYSIS

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
  scale_x_discrete(limits = age_group_order) +
  scale_y_continuous(labels = scales::comma)

age_group_outcomes <- covid.clean %>%
  group_by(age_group) %>%
  summarise(case_count = n(),
            icu_count = sum(ifelse(icu_yn == TRUE, 1, 0), na.rm = TRUE)
            ) %>%
  arrange(desc(case_count))

age_group_outcomes_df <- collect(age_group_outcomes)

age_group_outcomes_df$age_group <- factor(age_group_outcomes_df$age_group, levels = age_group_order)

ggplot(age_group_outcomes_df, aes(x = case_count, y = icu_count, label = age_group)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5, hjust = 0.5) +
  labs(title = "COVID-19 Cases vs ICU admissions by Age Group", x = "Number of Cases", y = "Number of ICU admissions") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma)

covid.ca <- covid.clean %>%
  filter(res_state == "CA")

ca_monthly_summary <- covid.ca %>%
  group_by(case_month) %>%
  summarise(case_count = n()) %>%
  arrange(case_month)

ca_monthly_summary_df <- collect(ca_monthly_summary)
ca_monthly_summary_df$case_month <- as.Date(paste0(ca_monthly_summary_df$case_month, "-01"), format = "%Y-%m-%d")

ggplot(ca_monthly_summary_df, aes(x = case_month, y = case_count)) +
  geom_line(aes(group=1), color="blue") + geom_point(color="red") +
  labs(title = "COVID-19 Case Counts Over Time in California", x = "Month", y = "Number of Cases") +
  theme_minimal() +
  scale_x_date(breaks = seq(min(ca_monthly_summary_df$case_month), max(ca_monthly_summary_df$case_month), by = "6 month")) +
  scale_y_continuous(labels = scales::comma)

covid.outcome_summary <- covid.clean %>%
  filter(
    sex %in% c("Male", "Female") & 
      hosp_yn %in% c("Yes", "No") & 
      icu_yn %in% c("Yes", "No") & 
      death_yn %in% c("Yes", "No") &
      symptom_status %in% c("Symptomatic", "Asymptomatic")
  ) %>%
  mutate(
    outcome = case_when(
      hosp_yn == "Yes" ~ "Hospitalized",
      death_yn == "Yes" ~ "Death",
      symptom_status == "Asymptomatic" ~ "Asymptomatic",
      symptom_status == "Symptomatic" & hosp_yn != "Yes" & death_yn != "Yes" & icu_yn != "Yes" ~ "Symptomatic, not severe",
      TRUE ~ "Other"
    )
  ) %>%
  filter(outcome != "Other") %>%
  group_by(age_group, sex, outcome)  %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(age_group, sex) %>%
  mutate(percent = count / sum(count) * 100)

covid.outcome_summary_df <- collect(covid.outcome_summary)

ggplot(covid.outcome_summary_df, aes(x = age_group, y = percent, fill = outcome)) +
  geom_bar(stat = "identity", position = "fill", color = "white") +
  facet_wrap(~sex) +
  labs(title = "COVID-19 Laboratory-Confirmed Case Outcome by Age Group and Sex",
       x = "Age Group",
       y = "Percentage",
       fill = "Outcome") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  scale_x_discrete(limits = age_group_order) +
  scale_y_continuous(labels = scales::comma)

# CLASSIFICATION


