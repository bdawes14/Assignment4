

#Downloading/formatting data
library(tidyverse)
library(gtsummary)
library(ggplot2)

cohort <- read.csv("cohort.csv")
View(cohort)
cohort$smoke <- factor(cohort$smoke, levels = c(0, 1), labels = c("No", "Yes"))
cohort$female <- factor(cohort$female, levels = c(0, 1), labels = c("Male", "Female"))
cohort$cardiac <- factor(cohort$cardiac, levels = c(0, 1), labels = c("No", "Yes"))

##Descriptive table of variables
cohort %>%
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ± {sd}")) 

##Predictive models
#Predicting cardiac disease
cardiac_model <- glm(cardiac~female + smoke + age, data=cohort, family=binomial)
summary(cardiac_model)
cardiac_model %>%
  tbl_regression(exponentiate = TRUE) %>%
  add_glance_table(include = c(logLik, AIC)) %>%
  modify_caption("Prediction Model for Cardiac Disease")

#Predicting Cost
cost_model <- lm(cost ~ female + smoke + age + cardiac, data = cohort)
summary(cost_model)
cost_model %>%
  tbl_regression() %>%
  add_glance_table(include = c(r.squared, AIC)) %>%
  modify_caption("Predictive Model for Healthcare Costs")

# Store t-test results for cost by smoking status
cost_ttest <- t.test(cost ~ smoke, data = cohort)
print(cost_ttest)

#Create violin plot for healthcare costs by smoking status
ggplot(cohort, aes(x = smoke, y = cost, fill = smoke)) + 
  geom_violin(alpha = 0.7, trim = FALSE) + 
  geom_boxplot(width = 0.2, alpha = 0.8) +
  
  # Add significance annotation
  annotate("text", x = 1.5, y = max(cohort$cost) * 0.95, 
           label = "p < 0.001 ***", size = 4, fontface = "bold") +
  
  labs(
    title = "Healthcare Cost Distribution by Smoking Status",
    x = "Smoking Status", 
    y = "Healthcare Costs (USD)"
  ) + 
  scale_fill_manual(values = c("white", "white"), guide = "none") +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12)
  )

# T-test for cost difference by cardiac status
cardiac_cost_ttest <- t.test(cost ~ cardiac, data = cohort)

# Create violin plot for cardiac events vs cost
ggplot(cohort, aes(x = cardiac, y = cost, fill = cardiac)) + 
  geom_violin(alpha = 0.7, trim = FALSE) + 
  geom_boxplot(width = 0.2, alpha = 0.8) +
  annotate("text", x = 1.5, y = max(cohort$cost) * 0.95, 
           label = "p < 0.001 ***", size = 4, fontface = "bold") +
  labs(
    title = "Healthcare Cost Distribution by Cardiac Event Status",
    x = "Cardiac Events", 
    y = "Healthcare Costs (USD)"
  ) + 
  scale_fill_manual(values = c("white", "white"), guide = "none") +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  )

