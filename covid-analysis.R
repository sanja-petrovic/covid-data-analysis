# DATA PREP

library(sparklyr)
library(dplyr)
library(ggplot2)
library(ggmosaic)


sc <- spark_connect(master = "local")
spark_get_java()
# учитавање података
covid.raw <- spark_read_csv(sc, name="covid", path=".")

# уређивање учитаних података
# испитивање присуства недостајућих вредности
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

# израчунавање вредности дескриптивних статистика по појединачним обележјима
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

# визуализовање расподеле по појединачним обележјима
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

# визуализовање расподеле по појединачним обележјима
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

# визуализовање расподеле по појединачним обележјима
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

# испитивање односа између обележја -- Pitanje: Da li ovde treba dodati jos nesto? Npr. correlation matrix?
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


# CLASSIFICATION

# формирање класификационих модела за исто циљно обележје применом три
# различита метода класификације и оцењивање перформанси у
# класификацији | logisticka regresija, stabla odlucivanja, masina potpornih vektora

covid.patients <- covid.clean %>%
  select(age_group, sex, race, symptom_status, underlying_conditions_yn, hosp_yn, death_yn, icu_yn) %>%
  na.omit()

covid.split <- sdf_random_split(covid.patients, training = 0.75, test = 0.25, seed = 5)

covid.training <- covid.split$training
covid.test <- covid.split$test

covid.class.formula <- death_yn ~ 
  age_group +
  sex +
  race +
  symptom_status +
  underlying_conditions_yn +
  hosp_yn + 
  icu_yn

# logistic regression

# за сваки метод класификације формирање класификационих модела за различите вредности параметара према три сценарија 
# Pitanje: Tri scenarija - da li je to broj iteracija?
covid.class.iters <- c(1, 3, 5, 15, 30)
covid.class.model.accuracy.reg <- c(length(iters))
covid.class.model.precision.reg <- c(length(iters))
covid.class.model.f1.reg <- c(length(iters))
covid.class.model.recall.reg <- c(length(iters))
counter <- 0

for (iter in covid.class.iters) {
  counter <- counter + 1
  log.reg <- ml_logistic_regression(
    x = covid.training, 
    formula = covid.class.formula,
    family="binomial", max_iter = iter, threshold = 0.5)
  
  log.reg.eval <- ml_evaluate(log.reg, covid.test)
  
  # израчунавање вредности различитих бројчаних показатеља перформанси
  covid.class.model.accuracy.reg[counter] <- log.reg.eval$accuracy()
  covid.class.model.precision.reg[counter] <- log.reg.eval$weighted_precision()
  covid.class.model.f1.reg[counter] <- log.reg.eval$weighted_f_measure()
  covid.class.model.recall.reg[counter] <- log.reg.eval$weighted_recall()
}

# to do:  испитивање односа између вредности параметара и перформанси у класификацији
# Pravljenje grafika koji poredi iteracije i accuracy

# decision tree

covid.class.iters <- c(1, 3, 5, 15, 30)
covid.class.model.accuracy.tree <- c(length(iters))
counter <- 0

for (iter in covid.class.iters) {
  counter <- counter + 1
  dec.tree <- ml_decision_tree_classifier(
    x = covid.training, 
    formula = covid.class.formula,
    max_depth = iter, 
    min_instances_per_node = 1000, 
    impurity = "gini")
  
  dec.tree.eval <- ml_evaluate(dec.tree, covid.test)
  covid.class.model.accuracy.tree[counter] <- dec.tree.eval$Accuracy
}

# support vector machine

covid.class.iters <- c(1, 3, 5)
covid.class.model.accuracies.svm <- c(length(iters))
counter <- 0

for (iter in covid.class.iters) {
  counter <- counter + 1
  svm <- ml_linear_svc(
    x = covid.training, 
    formula = covid.class.formula,
    max_iter = iter)
  
  svm.eval <- ml_evaluate(svm, covid.test)
  covid.class.model.accuracies.svm[counter] <- svm.eval$Accuracy
}

covid.class.model.accuracies.svm

# бирање решења за класификацију по сваком од метода и на нивоу свих метода - Sta ovo tacno podrazumeva?
summary(covid.class.model.accuracies.svm)
summary(covid.class.model.accuracy.tree)
summary(covid.class.model.accuracy.reg)

# k-fold cross validation, k = 4

# одређивање перформанси применом унакрсне валидације
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


covid.log.reg.trained = list(s1=ml_logistic_regression(covid.training$s1, formula, family="binomial", threshold=0.5),
               s2=ml_logistic_regression(covid.training$s2, formula, family="binomial", threshold=0.5),
               s3=ml_logistic_regression(covid.training$s3, formula, family="binomial", threshold=0.5),
               s4=ml_logistic_regression(covid.training$s4, formula, family="binomial", threshold=0.5)
)

covid.log.reg.eval.accuracy <- (ml_evaluate(covid.log.reg.trained$s1, covid.split$s1)$accuracy() +
                     ml_evaluate(covid.log.reg.trained$s2, covid.split$s2)$accuracy() +
                     ml_evaluate(covid.log.reg.trained$s3, covid.split$s3)$accuracy() +
                     ml_evaluate(covid.log.reg.trained$s4, covid.split$s4)$accuracy()
) / 4

covid.dec.tree.trained = list(s1=ml_decision_tree_classifier(covid.training$s1, formula, min_instances_per_node = 1000, impurity = "gini"),
                             s2=ml_decision_tree_classifier(covid.training$s2, formula, min_instances_per_node = 1000, impurity = "gini"),
                             s3=ml_decision_tree_classifier(covid.training$s3, formula, min_instances_per_node = 1000, impurity = "gini"),
                             s4=ml_decision_tree_classifier(covid.training$s4, formula, min_instances_per_node = 1000, impurity = "gini")
)

covid.dec.tree.eval.accuracy <- (ml_evaluate(covid.dec.tree.trained$s1, covid.split$s1)$Accuracy +
                                  ml_evaluate(covid.dec.tree.trained$s2, covid.split$s2)$Accuracy +
                                  ml_evaluate(covid.dec.tree.trained$s3, covid.split$s3)$Accuracy +
                                  ml_evaluate(covid.dec.tree.trained$s4, covid.split$s4)$Accuracy
) / 4


covid.svm.trained = list(s1=ml_linear_svc(covid.training$s1, formula, min_instances_per_node = 1000, impurity = "gini"),
                              s2=ml_linear_svc(covid.training$s2, formula, min_instances_per_node = 1000, impurity = "gini"),
                              s3=ml_linear_svc(covid.training$s3, formula, min_instances_per_node = 1000, impurity = "gini"),
                              s4=ml_linear_svc(covid.training$s4, formula, min_instances_per_node = 1000, impurity = "gini")
)

covid.svm.eval.accuracy <- (ml_evaluate(covid.svm.trained$s1, covid.split$s1)$Accuracy +
                                   ml_evaluate(covid.svm.trained$s2, covid.split$s2)$Accuracy +
                                   ml_evaluate(covid.svm.trained$s3, covid.split$s3)$Accuracy +
                                   ml_evaluate(covid.svm.trained$s4, covid.split$s4)$Accuracy
) / 4


# CLUSTERING
covid.clustering.data <- covid.clean %>%
  mutate(age_group_index = case_when(
    age_group == "0 - 17 years" ~ 0,
    age_group == "18 to 49 years" ~ 1,
    age_group == "50 to 64 years"~ 2,
    age_group == "65+ years" ~ 3,
    TRUE ~ NA_real_
  )) %>%
  select(age_group_index, sex, race, case_month, res_state, hosp_yn) %>%
  na.omit()

covid.clustering.data <- covid.clustering.data %>%
  group_by(age_group_index, sex, case_month, res_state, race) %>%
  summarise(death_count = sum(ifelse(death_yn == TRUE, 1, 0), na.rm = TRUE), .groups = "keep")

covid.clustering.data.kmeans <- covid.clustering.data %>% 
  ml_kmeans(~ age_group_index + death_count, 
            k = 4, 
            max_iter = 3, 
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

covid.clustering.data.kmeans$summary$ml_summary
# 