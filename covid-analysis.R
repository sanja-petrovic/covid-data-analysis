# DATA PREP

library(sparklyr)
library(dplyr)
library(ggplot2)
library(ggmosaic)
library(reshape2)
library(corrplot)


sc <- spark_connect(master = "local")
spark_get_java()

covid.raw <- spark_read_csv(sc, name="covid", path=".")

covid.clean <- covid.raw %>%
  mutate(across(c(where(is.character)), 
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

sdf_describe(
  covid.clean %>% filter(case_positive_specimen_interval >= 0 & case_onset_interval >= 0), 
  cols=c("case_positive_specimen_interval", "case_onset_interval")
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
      death_yn %in% c("Yes", "No") &
      symptom_status %in% c("Symptomatic", "Asymptomatic")
  ) %>%
  mutate(
    outcome = case_when(
      hosp_yn == "Yes" & death_yn != "Yes" ~ "Hospitalized",
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

covid.mosaic.data <- covid.clean %>%
  select(age_group, hosp_yn, death_yn) %>%
  na.omit()

covid.mosaic.data <- covid.mosaic.data %>% head(20000) %>% collect()

covid.mosaic.data <- covid.mosaic.data %>%
  mutate(across(everything(), as.factor))

ggplot(data = covid.mosaic.data) +
  geom_mosaic(aes(x = product(age_group, death_yn), fill = hosp_yn), na.rm=TRUE) +
  labs(
    title = "Mosaic Plot of Age Group, Death, and Hospitalization",
    x = "Age Group and Death Status",
    y = "Proportion",
    fill = "Hospitalized"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

race_data <- covid.clean %>%
  group_by(race) %>%
  summarise(count = n())

sex_data <- covid.clean %>%
  group_by(sex) %>%
  summarise(count = n())

ggplot(race_data %>% collect(), aes(x = "", y = count, fill = race)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(
    title = "Distribution of COVID-19 Cases by Race",
    fill = "Race"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

ggplot(sex_data %>% collect(), aes(x = "", y = count, fill = sex)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(
    title = "Distribution of COVID-19 Cases by Sex",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

balloon_data <- covid.clean %>%
  group_by(age_group, sex) %>%
  summarise(count = n())

ggplot(balloon_data %>% collect(), aes(x = age_group, y = sex, size = count, fill = count)) +
  geom_point(shape = 21, color = "black") +
  scale_size_continuous(range = c(1, 20), breaks = scales::pretty_breaks(n = 5), labels = scales::comma) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", labels = scales::comma) +
  labs(
    title = "Balloon Plot of COVID-19 Cases by Age Group and Sex",
    x = "Age Group",
    y = "Sex",
    size = "Number of Cases",
    fill = "Number of Cases"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )  

covid.numeric <- covid.clean %>% head(20000) %>%
  mutate(
    age_group_index = case_when(
      age_group == "0 - 17 years" ~ 1,
      age_group == "18 to 49 years" ~ 2,
      age_group == "50 to 64 years" ~ 3,
      age_group == "65+ years" ~ 4,
      TRUE ~ NA_real_
    ),
    hosp_yn_index = case_when(
      hosp_yn == "Yes" ~ 1,
      hosp_yn == "No" ~ 0,
      TRUE ~ NA_real_
    ),
    death_yn_index = case_when(
      death_yn == "Yes" ~ 1,
      death_yn == "No" ~ 0,
      TRUE ~ NA_real_
    ),
    symptom_status_index = case_when(
      symptom_status == "Symptomatic" ~ 1,
      symptom_status == "Asymptomatic" ~ 0,
      TRUE ~ NA_real_
    ),
    underlying_conditions_yn_index = case_when(
      underlying_conditions_yn == "Yes" ~ 1,
      underlying_conditions_yn == "No" ~ 0,
      TRUE ~ NA_real_
    )
  )

covid.numeric <- covid.numeric %>%
  select(
    age_group_index,
    hosp_yn_index,
    death_yn_index,
    symptom_status_index,
    underlying_conditions_yn_index
  ) %>%
  na.omit()


# Calculate the correlation matrix
covid.correlation <- cor(covid.numeric %>% collect())

corrplot(covid.correlation, method = "color", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.7)


# CLASSIFICATION

covid.patients <- covid.clean %>%
  select(age_group, sex, race, symptom_status, underlying_conditions_yn, hosp_yn, death_yn, icu_yn) %>%
  na.omit()

covid.class.formula <- death_yn ~ 
  age_group +
  sex +
  race +
  symptom_status +
  underlying_conditions_yn +
  hosp_yn + 
  icu_yn

k <- 4
covid.split <- covid.patients %>%
  sdf_random_split(seed=1,
                   s1=0.25,
                   s2=0.25,
                   s3=0.25,
                   s4=0.25)

covid.training <- list(
  s1 = sdf_bind_rows(covid.split$s2, covid.split$s3, covid.split$s4),
  s2 = sdf_bind_rows(covid.split$s1, covid.split$s3, covid.split$s4),
  s3 = sdf_bind_rows(covid.split$s1, covid.split$s2, covid.split$s4),
  s4 = sdf_bind_rows(covid.split$s1, covid.split$s2, covid.split$s3)
)

covid.test <- list(
  s1 = covid.split$s1,
  s2 = covid.split$s2,
  s3 = covid.split$s3,
  s4 = covid.split$s4
)

iters <- c(1, 1, 1, 3, 3, 3, 10, 10, 10)
ths <- c(0.5, 0.45, 0.55, 0.5, 0.45, 0.55, 0.5, 0.45, 0.55)
covid.log.reg.accuracy <- list()
covid.log.reg.f1 <- list()
covid.log.reg.prec <- list()
covid.log.reg.recall <- list()
covid.log.reg.tp <- list()
covid.log.reg.eval <- list()

for(i in 1:1:length(iters)) {
  covid.log.reg.trained = list(s1=ml_logistic_regression(covid.training$s1, covid.class.formula, family="binomial", threshold=ths[i], max_iter = iters[i]),
                               s2=ml_logistic_regression(covid.training$s2, covid.class.formula, family="binomial", threshold=ths[i], max_iter = iters[i]),
                               s3=ml_logistic_regression(covid.training$s3, covid.class.formula, family="binomial", threshold=ths[i], max_iter = iters[i]),
                               s4=ml_logistic_regression(covid.training$s4, covid.class.formula, family="binomial", threshold=ths[i], max_iter = iters[i])
  )
  
  covid.log.reg.eval.s1 <- ml_evaluate(covid.log.reg.trained$s1, covid.test$s1)
  covid.log.reg.eval.s2 <- ml_evaluate(covid.log.reg.trained$s2, covid.test$s2)
  covid.log.reg.eval.s3 <- ml_evaluate(covid.log.reg.trained$s3, covid.test$s3)
  covid.log.reg.eval.s4 <- ml_evaluate(covid.log.reg.trained$s4, covid.test$s4)
  
  covid.log.reg.accuracy[[i]] <- (covid.log.reg.eval.s1$accuracy() + covid.log.reg.eval.s2$accuracy() + covid.log.reg.eval.s3$accuracy() + covid.log.reg.eval.s4$accuracy()) / k
  covid.log.reg.prec[[i]] <- (covid.log.reg.eval.s1$weighted_precision() + covid.log.reg.eval.s2$weighted_precision() + covid.log.reg.eval.s3$weighted_precision() + covid.log.reg.eval.s4$weighted_precision()) / k
  covid.log.reg.f1[[i]] <- (covid.log.reg.eval.s1$weighted_f_measure() + covid.log.reg.eval.s2$weighted_f_measure() + covid.log.reg.eval.s3$weighted_f_measure() + covid.log.reg.eval.s4$weighted_f_measure()) / k
  covid.log.reg.recall[[i]] <- (covid.log.reg.eval.s1$weighted_recall() + covid.log.reg.eval.s2$weighted_recall() + covid.log.reg.eval.s3$weighted_recall() + covid.log.reg.eval.s4$weighted_recall()) / k
}
data <- data.frame(iter = iters, th = ths, accuracy = unlist(covid.log.reg.accuracy))
ggplot(data, aes(x = iter, y = accuracy, color = as.factor(th))) +
  geom_point(size=4, position = position_jitter(width = 0.15, height = 0), alpha = 0.6) +
  scale_x_continuous(breaks = c(1, 3, 10)) +
  scale_color_manual(values = c("red", "blue", "green")) +
  labs(title = "Logistic Regression Accuracy Over Iterations and Thresholds",
       x = "Iteration", 
       y = "Accuracy",
       color = "Threshold") +
  theme_minimal()


inst <- c(1, 1, 1, 1000, 1000, 1000, 2000, 2000, 2000)
depth <- c(3, 5, 20, 3, 5, 20, 3, 5, 20)
covid.dec.tree.accuracy <- list()
covid.dec.tree.f1 <- list()
covid.dec.tree.prec <- list()
covid.dec.tree.recall <- list()
covid.dec.tree.tp <- list()
covid.dec.tree.eval <- list()


for(i in 1:1:length(inst)) {
  covid.dec.tree.trained = list(s1=ml_decision_tree_classifier(covid.training$s1, covid.class.formula, min_instances_per_node = inst[i], max_depth=depth[i], impurity = "gini"),
                               s2=ml_decision_tree_classifier(covid.training$s2, covid.class.formula, min_instances_per_node = inst[i], max_depth=depth[i], impurity = "gini"),
                               s3=ml_decision_tree_classifier(covid.training$s3, covid.class.formula, min_instances_per_node = inst[i], max_depth=depth[i], impurity = "gini"),
                               s4=ml_decision_tree_classifier(covid.training$s4, covid.class.formula, min_instances_per_node = inst[i], max_depth=depth[i], impurity = "gini")
  )
  
  covid.dec.tree.eval.s1 <- ml_evaluate(covid.dec.tree.trained$s1, covid.test$s1)
  covid.dec.tree.eval.s2 <- ml_evaluate(covid.dec.tree.trained$s2, covid.test$s2)
  covid.dec.tree.eval.s3 <- ml_evaluate(covid.dec.tree.trained$s3, covid.test$s3)
  covid.dec.tree.eval.s4 <- ml_evaluate(covid.dec.tree.trained$s4, covid.test$s4)
  
  covid.dec.tree.accuracy[[i]] <- (covid.dec.tree.eval.s1$Accuracy + covid.dec.tree.eval.s2$Accuracy + covid.dec.tree.eval.s3$Accuracy + covid.dec.tree.eval.s4$Accuracy) / k
}
data <- data.frame(inst = inst, depth = depth, accuracy = unlist(covid.dec.tree.accuracy))
ggplot(data, aes(x = depth, y = accuracy, color = as.factor(inst))) +
  geom_point(size=4, position = position_jitter(width = 0.15, height = 0), alpha = 0.6) +
  scale_x_continuous(breaks = c(3, 5, 20)) +
  scale_color_manual(values = c("red", "blue", "green")) +
  labs(title = "Decision Tree Accuracy Over Min Instances Per Node and Max Depth",
       x = "Depth", 
       y = "Accuracy",
       color = "Min Instances Per Node") +
  theme_minimal()

iters <- c(1, 1, 1, 3, 3, 3, 10, 10, 10)
ths <- c(0.5, 0.45, 0.55, 0.5, 0.45, 0.55, 0.5, 0.45, 0.55)
covid.svm.accuracy <- list()
covid.svm.f1 <- list()
covid.svm.prec <- list()
covid.svm.recall <- list()
covid.svm.tp <- list()
covid.svm.eval <- list()

for(i in 1:1:length(iters)) {
  covid.svm.trained = list(s1=ml_linear_svc(covid.training$s1, covid.class.formula, threshold=ths[i], max_iter = iters[i]),
                                s2=ml_linear_svc(covid.training$s2, covid.class.formula, threshold=ths[i], max_iter = iters[i]),
                                s3=ml_linear_svc(covid.training$s3, covid.class.formula, threshold=ths[i], max_iter = iters[i]),
                                s4=ml_linear_svc(covid.training$s4, covid.class.formula, threshold=ths[i], max_iter = iters[i])
  )
  
  covid.svm.eval.s1 <- ml_evaluate(covid.svm.trained$s1, covid.test$s1)
  covid.svm.eval.s2 <- ml_evaluate(covid.svm.trained$s2, covid.test$s2)
  covid.svm.eval.s3 <- ml_evaluate(covid.svm.trained$s3, covid.test$s3)
  covid.svm.eval.s4 <- ml_evaluate(covid.svm.trained$s4, covid.test$s4)
  
  covid.svm.accuracy[[i]] <- (covid.svm.eval.s1$Accuracy + covid.svm.eval.s2$Accuracy + covid.svm.eval.s3$Accuracy + covid.svm.eval.s4$Accuracy) / k
}
data <- data.frame(iter = iters, th = ths, accuracy = unlist(covid.svm.accuracy))
ggplot(data, aes(x = iter, y = accuracy, color = as.factor(th))) +
  geom_point(size=4, position = position_jitter(width = 0.15, height = 0), alpha = 0.6) +
  scale_x_continuous(breaks = c(1, 3, 10)) +
  scale_color_manual(values = c("red", "blue", "green")) +
  labs(title = "Support Vector Machine Accuracy Over Iterations and Thresholds",
       x = "Iteration", 
       y = "Accuracy",
       color = "Threshold") +
  theme_minimal()


# CLUSTERING
covid.clustering.data <- covid.clean %>%
  mutate(age_group_index = case_when(
    age_group == "0 - 17 years" ~ 0,
    age_group == "18 to 49 years" ~ 1,
    age_group == "50 to 64 years"~ 2,
    age_group == "65+ years" ~ 3,
    TRUE ~ NA_real_
  )) %>%
  select(age_group_index, sex, race, case_month, res_state, death_yn) %>%
  na.omit()

covid.clustering.data <- covid.clustering.data %>%
  group_by(age_group_index, sex, case_month, res_state, race) %>%
  summarise(death_count = sum(ifelse(death_yn == TRUE, 1, 0), na.rm = TRUE), .groups = "keep")

covid.clustering.data.kmeans <- covid.clustering.data %>% 
  ml_kmeans(~ age_group_index + death_count, 
            k = 2, 
            max_iter = 1, 
            init_mode = "k-means||")

covid.clustering.data.kmeans

cluster.values2 <- covid.clustering.data.kmeans$model$summary$cluster() %>% collect()
covid.clustering.data <- covid.clustering.data %>% collect()
covid.clustering.data$clust <- as.factor(cluster.values2$prediction)

covid.clustering.data.kmeans$centers

cluster.centers.df2 <- data.frame(covid.clustering.data.kmeans$centers)
cluster.centers.df2
ggplot(data = covid.clustering.data,
       aes(x = age_group_index, y = death_count, colour = clust)) +
  geom_jitter(size = 1) +
  geom_point(data = cluster.centers.df2,
             color = "black",
             size = 2,
             shape = 0 )+
  theme_minimal() +
  labs(
    y = "Death Count",
    x = "Age Group Index",
    color = "Cluster"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  scale_color_manual(values = c("hotpink", "deepskyblue", "darkorchid1", "darkolivegreen2", "red", "orange", "green")) 
