# Learning Generalized Liner Model (GLM)
## url: https://www.guru99.com/r-generalized-linear-model.html

library(dplyr)
data_adult <-read.csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/adult.csv")
glimpse(data_adult)

#ERROS CORRETION
##convert the chr var to factors
data_adult <- data_adult %>%
  mutate_if(is.character, as.factor)
str(data_adult)


#OBJECTIVE: predict which individual will gave a revenue higher than 50K

#1)
continuous <- select_if(data_adult, is.numeric)
summary(continuous)

## Histogram with kernel density curve
library(ggplot2)
ggplot(continuous, aes(x = hours.per.week)) +
  geom_density(alpha = 0.2, fill = "pink")

## Removing the top 0.01 percent(outliers)
top_one_percent <- quantile(data_adult$hours.per.week, 0.99)
top_one_percent
### 99% of the population works under 80 hours per week

data_adult_drop <- data_adult %>%
  filter(hours.per.week < top_one_percent)
dim(data_adult_drop)

## Standardize the continuous variables
data_adult_rescale <- data_adult_drop %>%
  mutate_if(is.numeric, funs(as.numeric(scale(.))))
head(data_adult_rescale)            

#2) Check factor variables
## select the categorical columns
factor <- select_if(data_adult_rescale, is.factor)
ncol(factor)

## create graph for each column
graph <- lapply(names(factor),
                function(x) 
                  ggplot(factor, aes(get(x))) +
                  geom_bar() +
                  theme(axis.text.x = element_text(angle = 90)))
graph

#3) Feature engineering
## recast education 
recast_data <- data_adult_rescale %>% 
  select(-x) %>%
  mutate(education = factor(ifelse(education == "Preschool" | education == "10th" | education == "11th" | education == "12th" | education == "1st-4th" | education == "5th-6th" | education == "7th-8th" | education == "9th", "dropout", ifelse(education == "HS-grad", "HighGrad", ifelse(education == "Some-college" | education == "Assoc-acdm" | education == "Assoc-voc", "Community",
                                                                                                                                                                                                                                                                                            ifelse(education == "Bachelors", "Bachelors",
                                                                                                                                                                                                                                                                                                   ifelse(education == "Masters" | education == "Prof-school", "Master", "PhD")))))))

recast_data %>%
  group_by(education) %>%
  summarize(average_educ_year = mean(educational.num),
            count = n()) %>%
  arrange(average_educ_year)

## recast marital-status
recast_data <- recast_data %>%
  mutate(marital.status = factor(ifelse(marital.status == "Never-married" | marital.status == "Married-spouse-absent", "Not_married", ifelse(marital.status == "Married-AF-spouse" | marital.status == "Married-civ-spouse", "Married", ifelse(marital.status == "Separated" | marital.status == "Divorced", "Separated", "Widow")))))
table(recast_data$marital.status)


#4) Summary Statistic
# Plot gender income
ggplot(recast_data, aes(x = gender, fill = income)) +
  geom_bar(position = "fill") +
  theme_classic()

# Plot origin income
ggplot(recast_data, aes(x = race, fill = income)) +
  geom_bar(position = "fill") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))

# box plot gender working time
ggplot(recast_data, aes(x = gender, y = hours.per.week)) +
  geom_boxplot() +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               color = "steelblue") +
  theme_classic()

# Plot distribution working time by education
ggplot(recast_data, aes(x = hours.per.week)) +
  geom_density(aes(color = education), alpha = 0.5) +
  theme_classic()

# The ANOVA test confirms the difference in average between groups
anova <- aov(hours.per.week~education, recast_data)
summary(anova)

# Non-linearity
## number of hours worked is related to age?
ggplot(recast_data, aes(x = age, y = hours.per.week)) +
  geom_point(aes(color = income),
             size = 0.5) +
  stat_smooth(method = 'lm',
              formula = y~poly(x, 2),
              se = TRUE,
              aes(color = income)) +
  theme_classic()

#Correlation
## visualize the correlation between the variables
library(GGally)
# Convert data to numeric
corr <- data.frame(lapply(recast_data, as.integer))
# Plot the graph
ggcorr(corr,
  method = c("pairwise", "spearman"),
  nbreaks = 6,
  hjust = 0.8,
  label = TRUE,
  label_size = 3,
  color = "grey50")


#5 Train/test set
set.seed(1234)
create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}
data_train <- create_train_test(recast_data, 0.8, train = TRUE)
data_test <- create_train_test(recast_data, 0.8, train = FALSE)
dim(data_train)

#6 Build the model
formula <- income~.
logit <- glm(formula, data = data_train, family = 'binomial')
summary(logit)

# The list is very long, print only the first three elements
lapply(logit, class)[1:3]

#AIC (Akaike Information Criteria)
logit$aic

#7 Assess the performance of the model
predict <- predict(logit, data_test, type = 'response')
# confusion matrix
table_mat <- table(data_test$income, predict > 0.5)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

# Precision vs Recall
precision <- function(matrix) {
  # True positive
  tp <- matrix[2, 2]
  # false positive
  fp <- matrix[1, 2]
  return (tp / (tp + fp))
}

recall <- function(matrix) {
  # true positive
  tp <- matrix[2, 2]# false positive
  fn <- matrix[2, 1]
  return (tp / (tp + fn))
}

prec <- precision(table_mat)
prec
rec <- recall(table_mat)
rec

f1 <- 2 * ((prec * rec) / (prec + rec))
f1
