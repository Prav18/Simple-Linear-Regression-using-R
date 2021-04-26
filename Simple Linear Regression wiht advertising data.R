#==============Simple Linear Regression Model: Advertising======================
# 
# Problem Statement
#-------------------
# Build a model which predicts sales based on the money spent on different 
# platforms for marketing.


# Step 1:  Reading and Understanding the Data

md <- read.csv("advertising.csv")

str(md)

# Step 2: Exploratory Data Anlaysis

library(psych)

describe(md)

plot(md$TV, md$Sales)

pairs.panels(md)

# Step 3: Searching Missing Values and Outliers

sum(is.na(md))

# Step 4: Partition the Data

set.seed(18)
train=sample(1:nrow(md), 0.7*nrow(md))
md_train=md[train,]
md_test=md[-train,]

# Step 5: Train the Model

fit <- lm(Sales~TV, data = md)
summary(fit)

# Step 6: Evalute the model with train data set

md_train$Score <- round(predict(fit, newdata = md_train),2)

head(md_train$Sales)
head(md_train$Score)
library(dplyr)
rmse_train <- mean((md_train$Sales-score)**2) %>%
  sqrt()
rmse_train

library(ggplot2)
md_train %>%
  ggplot(aes(x=Sales,y=Score))+geom_point()

# Step 6: Evalute the model with test data set

md_test$Score <- round(predict(fit, newdata = md_test),2)

head(md_test$Sales)
head(md_test$Score)

rmse_test <- mean((md_test$Sales-md_test$Score)**2) %>%
  sqrt()
rmse_test

library(ggplot2)
md_test %>%
  ggplot(aes(x=Sales,y=Score))+geom_point()

# Step 7:  Checking the Assumptions of Linear Regression

# par(mfrow = c(2,2))
# 
# plot(fit)
# 
# par(mfrow = c(1,1))

plot(fit, which = 1)

plot(fit, which = 2)

plot(fit, which = 3)

plot(fit, which = 4)


# Step 8: Save the model to disk

saveRDS(fit, "adv_SLR.rds")


# later we can use the model as follows:

# load the model
super_model <- readRDS("adv_SLR.rds")
print(super_model)
# make a predictions on "new data" using the final model
final_predictions <- predict(super_model, md_test)
round(head(final_predictions),2)
