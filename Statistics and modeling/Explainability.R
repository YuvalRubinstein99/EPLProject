library(tidyverse)
library(lmtest)
library(sandwich)
library(lfe)
library(MASS)
library(glmnet)
library(pROC)
library(ordinalNet)
library(orf)

setwd('C:/Users/Yuval/OneDrive/Desktop/EPLproject/Epl Website Scraping/Statistics and modeling')
rm(list=ls())


data <- read_csv("train.csv")


test <- read_csv('test.csv')
data <- data%>%
  mutate("Status" = as.factor(ifelse(home_score > away_score, 1,
                           ifelse(home_score == away_score, 0,
                                  -1))))%>%
  mutate_if(is.character, ~replace_na(.,'Unknown'))%>%
  mutate(GD = home_score-away_score)%>%
  mutate(home_team_name = as.factor(home_team_name),
         away_team_name = as.factor(away_team_name),
         home_formation = as.factor(home_formation),
         away_fromation = as.factor(away_fromation))

test <- test%>%
  mutate("Status" = as.factor(ifelse(home_score > away_score, 1,
                           ifelse(home_score == away_score, 0,
                                  -1))))%>%
  mutate_if(is.character, ~replace_na(.,'Unknown'))%>%
  mutate(GD = home_score-away_score)%>%
  mutate(home_team_name = as.factor(home_team_name),
         away_team_name = as.factor(away_team_name),
         home_formation = as.factor(home_formation),
         away_fromation = as.factor(away_fromation))


# Check for NaN values in each column
sapply(data, function(x) any(is.nan(x)))

### Graphing

ggplot(data = data, aes(x = HomePlayer_Age_mean, y = GD)) +
  geom_point()

ggplot(data, aes(x = HomePlayer_Age_mean)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of HomePlayer_Age_mean",
       x = "HomePlayer_Age_mean",
       y = "Density")

ggplot(data, aes(x = HomePlayer_Overall_sd)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of HomePlayer_Age_sd",
       x = "sd",
       y = "Density")

ggplot(data, aes(x = HomePlayer_Overall_mean)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of HomePlayer_Age_mean",
       x = "HomePlayer_Age_mean",
       y = "Density")

ggplot(data = data, aes(x = HomePlayer_Age_mean,
                        y = HomePlayer_Overall_mean)) +
  geom_point()


ggplot(data = data, aes(x = HomePlayer_Height_mean,
                        y = Status)) +
  geom_point()



ggplot(data = data, aes(x = log(HomePlayer_Overall_mean),
                        y = log(AwayPlayer_Overall_mean), color=GD)) +
  geom_point()


# Summary for the attributes

summary(data$HomePlayer_Overall_mean)
summary(data$HomePlayer_Overall_min)
summary(data$HomePlayer_Overall_max)
summary(data$HomePlayer_Overall_sd)
summary(data$AwayPlayer_Overall_mean)
summary(data$AwayPlayer_Overall_min)
summary(data$AwayPlayer_Overall_max)
summary(data$AwayPlayer_Overall_sd)

# modeling

ordinal_model <- polr(
  formula = Status ~ 
    log(HomePlayer_Overall_mean) +
    log(HomePlayer_Overall_min) +
    log(HomePlayer_Overall_max) +
    log(HomePlayer_Overall_sd) +
    log(HomePlayer_bench_Overall_mean) +
    log(HomePlayer_bench_Overall_min) +
    log(HomePlayer_bench_Overall_max) +
    log(HomePlayer_bench_Overall_sd) +
    log(AwayPlayer_Overall_mean) +
    log(AwayPlayer_Overall_min) +
    log(AwayPlayer_Overall_max) +
    log(AwayPlayer_Overall_sd) +
    log(AwayPlayer_bench_Overall_mean) +
    log(AwayPlayer_bench_Overall_min) +
    log(AwayPlayer_bench_Overall_max) +
    log(AwayPlayer_bench_Overall_sd) +
    log(HomePlayer_Height_mean) +
    log(AwayPlayer_Height_mean) +
    log(HomePlayer_Weight_mean) +
    log(AwayPlayer_Weight_mean) +
    log(HomePlayer_bench_Height_mean) +
    log(AwayPlayer_bench_Height_mean) +
    log(HomePlayer_bench_Weight_mean) +
    log(AwayPlayer_bench_Weight_mean) +
    
    HomePlayer_Age_mean+
    AwayPlayer_Age_mean,
  data = data,
  method = 'probit'
)
summary(ordinal_model)

lm_model <- lm(data=data, formula = home_score ~
                 HomePlayer_Overall_mean +
                 HomePlayer_Overall_min + 
                 HomePlayer_Overall_max + 
                 HomePlayer_Overall_sd + 
                 HomePlayer_bench_Overall_mean +
                 HomePlayer_bench_Overall_min + 
                 HomePlayer_bench_Overall_max + 
                 HomePlayer_bench_Overall_sd + 
                 AwayPlayer_Overall_mean +
                 AwayPlayer_Overall_min + 
                 AwayPlayer_Overall_max + 
                 AwayPlayer_Overall_sd  +
                 AwayPlayer_bench_Overall_mean  +
                 AwayPlayer_bench_Overall_min  + 
                 AwayPlayer_bench_Overall_max  + 
                 AwayPlayer_bench_Overall_sd  +
                 HomePlayer_Height_mean  +
                 AwayPlayer_Height_mean  +
                 HomePlayer_Weight_mean  +
                 AwayPlayer_Weight_mean  +
                 HomePlayer_bench_Height_mean  +
                 AwayPlayer_bench_Height_mean  +
                 HomePlayer_bench_Weight_mean  +
                 AwayPlayer_bench_Weight_mean  +
                 HomePlayer_Age_mean + I(HomePlayer_Age_mean^2)+
                 AwayPlayer_Age_mean + I(AwayPlayer_Age_mean^2)
               + home_team_name+ away_team_name)

summary(lm_model)

preda <- predict(object=ordinal_model, newdata=test, type='probs')
summary(preda)

test = na.omit(test)

roc_curve <- multiclass.roc(test$Status, preda)

# Print AUC
cat("AUC:", auc(roc_curve), "\n")

# Ordered Forest ----------------------------------------------------------
library(ROCR)

X_train = as.matrix(read_csv('X.csv')[,2:16])
Y_train = as.matrix(read_csv('Y.csv')[,'label'])
# Create a mapping vector
mapping <- c("-1" = 1, "0" = 2, "1" = 3)
Y_train = recode(Y_train, !!!mapping)

orf_model <- orf(X_train, Y_train)
print(orf_model)
summary(orf_model)
plot(orf_model)
X_test = as.matrix(read_csv('X_test.csv')[,2:16])
Y_test = as.matrix(read_csv('Y_test.csv')[,'label'])
Y_test = recode(Y_test, !!!mapping)

# Make predictions on the test set
orf_test <- predict(object = orf_model, newdata = X_test, type = "class")

# Convert predicted probabilities to class labels
predicted_labels <- orf_test$predictions
library(caret)

# Create a confusion matrix
conf_matrix <- table(Y_test,predicted_labels)

# Print the confusion matrix
print(conf_matrix)
