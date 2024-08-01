# Load necessary libraries
library(ranger)
library(tidyr) #for replace_na
library(dplyr) # for data manipulation

# Create binary outcome for the first part of the model
data$nonzero_expend <- as.factor(ifelse(data$expend > 0, 1, 0))


#Fit Random Forest for the first part (classification) using ranger
#With Weights
rf_part1 <- ranger(nonzero_expend ~ asthma_treated + sex + age + agesq + income + educ + race + insurance + married + region+ Year,
                   data = data, case.weights=data$PERWTF,
                   num.trees = 500,
                   probability = TRUE)

# Fit Random Forest for the second part (regression on non-zero expenditures) using ranger
#With Weights
rf_part2 <- ranger(expend ~ asthma_treated + sex + age + agesq + income + educ + race + insurance + married + region + Year,
                   data = subset(data, nonzero_expend == 1), case.weights=data%>%filter(nonzero_expend==1)%>%select(PERWTF),
                   num.trees = 500)

# #pred on full data
# full_data$prob_nonzero <- predict(rf_part1, full_data)$predictions[, 2]
# full_data$pred_expend <- predict(rf_part2, full_data)$predictions
# full_data$expected_expend <- round(full_data$prob_nonzero) * full_data$pred_expend

# Calculate incremental effect of asthma_treated
base_data <- subset(data, asthma_treated == 1)
counterfactual_data <- base_data
counterfactual_data$asthma_treated <- 0

# Predict for base and counterfactual scenarios
base_data$prob_nonzero <- predict(rf_part1, base_data)$predictions[, 2]
base_data$pred_expend <- predict(rf_part2, base_data)$predictions
base_data$expected_expend <- round(base_data$prob_nonzero) * base_data$pred_expend

counterfactual_data$prob_nonzero <- predict(rf_part1, counterfactual_data)$predictions[, 2]
counterfactual_data$pred_expend <- predict(rf_part2, counterfactual_data)$predictions
counterfactual_data$expected_expend <- round(counterfactual_data$prob_nonzero) * counterfactual_data$pred_expend

#calculated individual treatment effect on treated
difference<-base_data$expected_expend - counterfactual_data$expected_expend

weights<-base_data$PERWTF/sum(base_data$PERWTF)
weighted_att<-sum(difference*weights)
unweighted_att <- mean(base_data$expected_expend - counterfactual_data$expected_expend)

# Print the incremental cost
print(weighted_att)
print(unweighted_att)



##### MISSED WORK DAYS MODELING
# Load necessary libraries
library(MASS)
library(pscl)

data<-subset(data,missed_work>=0)

# Fit a negative binomial regression model
nb_model <- glm.nb(missed_work ~ asthma_treated + sex + age + agesq + income + educ + race + insurance + married + region, data = data)

# Display the summary of the negative binomial model
summary(nb_model)

# Calculate the incremental effect of asthma_treated
base_data <- subset(data,asthma_treated==1)
counterfactual_data <- base_data
counterfactual_data$asthma_treated <- 0

base_prediction <- predict(nb_model, newdata = base_data,type="response")
counterfactual_prediction <- predict(nb_model, newdata = counterfactual_data,type="response")

#calculated individual treatment effect on treated
difference<-base_prediction - counterfactual_prediction

weights<-base_data$PERWTF/sum(base_data$PERWTF)
weighted_att_work<-sum(difference*weights)
unweighted_att_work <- mean(difference)

# Print the incremental cost
print(weighted_att_work)
print(unweighted_att_work)


