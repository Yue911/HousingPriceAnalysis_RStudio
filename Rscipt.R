##################
## Load data set ##
##################
data <- read.csv("D:/KCL sm1/R language/California_housing.csv", header = TRUE)

##################
## Explore data ##
##################
## 1. Data screening: remove rows with missing values
data <- na.omit(data)
head(data)
str(data)
data_num <- data[, -9]
head(data_num)
data

## 2. Summary estimates
# 2.1 quantitative variables
summary(data_num)

iqr <- sapply(data_num, function(col) {
  if (is.numeric(col)) {
    IQR(col, na.rm = TRUE)
  } else {
    NA  
  }
})
print(iqr)

modes <- sapply(data_num, function(x) {
  freq_table <- table(x)
  mode <- as.numeric(names(freq_table)[freq_table == max(freq_table)])
  mode_count <- max(freq_table)
  return(c(Mode = mode, Count = mode_count))
})

print(modes)

library(psych)
describe(data_num)

# 2.2 categorical variables
data_cat <- factor(data$ocean_proximity)
summary(data_cat)
by(data, data$ocean_proximity, summary)

# 2.3 histograms, Kernel Density plots and QQ-plot
par(mfrow=c(1, 3))
hist(data$longitude, xlab = 'Longitude', ylab = 'Frequency', main = 'Histogram of longitude')
plot(density(data$longitude, na.rm = TRUE), main = 'Density of longitude in California housing')
qqnorm(data$longitude, main = 'Normal QQ-plot for longitude')
qqline(data$longitude, col = 2)

hist(data$latitude, xlab = 'Latitude', ylab = 'Frequency', main = 'Histogram of latitude')
plot(density(data$latitude, na.rm = TRUE), main = 'Density of latitude in California housing')
qqnorm(data$latitude, main = 'Normal QQ-plot for latitude')
qqline(data$latitude, col = 2)

hist(data$house_median_age, xlab = 'House median age', ylab = 'Frequency', main = 'Histogram of house median age')
plot(density(data$house_median_age, na.rm = TRUE), main = 'Density of house median age in California housing')
qqnorm(data$house_median_age, main = 'Normal QQ-plot for house median age')
qqline(data$house_median_age, col = 2)

hist(data$total_rooms, xlab = 'Total rooms', ylab = 'Frequency', main = 'Histogram of total rooms')
plot(density(data$total_rooms, na.rm = TRUE), main = 'Density of total rooms in California housing')
qqnorm(data$total_rooms, main = 'Normal QQ-plot for total rooms')
qqline(data$total_rooms, col = 2)

hist(data$total_bedrooms, xlab = 'Total bedrooms', ylab = 'Frequency', main = 'Histogram of total bedrooms')
plot(density(data$total_bedrooms, na.rm = TRUE), main = 'Density of total bedrooms in California housing')
qqnorm(data$total_bedrooms, main = 'Normal QQ-plot for total bedrooms')
qqline(data$total_bedrooms, col = 2)

hist(data$population, xlab = 'Population', ylab = 'Frequency', main = 'Histogram of population')
plot(density(data$population, na.rm = TRUE), main = 'Density of population in California housing')
qqnorm(data$population, main = 'Normal QQ-plot for population')
qqline(data$population, col = 2)

hist(data$households, xlab = 'Households', ylab = 'Frequency', main = 'Histogram of households')
plot(density(data$households, na.rm = TRUE), main = 'Density of households in California housing')
qqnorm(data$households, main = 'Normal QQ-plot for households')
qqline(data$households, col = 2)

hist(data$median_income, xlab = 'Median income', ylab = 'Frequency', main = 'Histogram of median income')
plot(density(data$median_income, na.rm = TRUE), main = 'Density of median income in California housing')
qqnorm(data$median_income, main = 'Normal QQ-plot for median income')
qqline(data$median_income, col = 2)

hist(data$median_house_value, xlab = 'Median house value', ylab = 'Frequency', main = 'Histogram of median house value')
plot(density(data$median_house_value, na.rm = TRUE), main = 'Density of median house value in California housing')
qqnorm(data$median_house_value, main = 'Normal QQ-plot for median house vallue')
qqline(data$median_house_value, col = 2)

# 2.4 scatter plot and box plot
par(mfrow=c(1, 1))
plot(data, cex = 0.1)

par(mfrow=c(3, 3))
boxplot(data$longitude~data$ocean_proximity, 
        xlab = 'Ocean proximity', ylab = 'Longitude', 
        main = 'Longitude values vs. ocean proximity')

boxplot(data$latitude~data$ocean_proximity, 
        xlab = 'Ocean proximity', ylab = 'Latitude', 
        main = 'Latitude vs. ocean proximity')

boxplot(data$house_median_age~data$ocean_proximity, 
        xlab = 'Ocean proximity', ylab = 'House median age', 
        main = 'House median age vs. ocean proximity')

boxplot(data$total_rooms~data$ocean_proximity, 
        xlab = 'Ocean proximity', ylab = 'Total rooms', 
        main = 'Total rooms vs. ocean proximity')

boxplot(data$total_bedrooms~data$ocean_proximity, 
        xlab = 'Ocean proximity', ylab = 'Total bedrooms', 
        main = 'Total bedrooms vs. ocean proximity')

boxplot(data$population~data$ocean_proximity, 
        xlab = 'Ocean proximity', ylab = 'Population', 
        main = 'Population vs. ocean proximity')

boxplot(data$households~data$ocean_proximity, 
        xlab = 'Ocean proximity', ylab = 'Households', 
        main = 'Households vs. ocean proximity')

boxplot(data$median_house_value~data$ocean_proximity, 
        xlab = 'Ocean proximity', ylab = 'Median house value', 
        main = 'Median house values vs. ocean proximity')







###############################
## Multiple Linear Regression ##original data
###############################
## 1. Create original linear model
attach(data_num)

full_model = lm(
  median_house_value ~ longitude + latitude + house_median_age + total_rooms + total_bedrooms + population + households + median_income, 
  data = data_num)
summary(full_model)
# R^2 = 0.6369 and SE = 69570 are measures for fit of LSE

## 2. Test of all predictors
null_model = lm(median_house_value ~ 1, data = data_num)
summary(null_model)
anova(null_model, full_model)
# H0: β1 = β2 = ... = β8 = 0
# H1: At least one βj != 0
# F is really high, so p-value is really low, reject H0

## 3. Test of one predictor
summary(full_model)

# confidence interval for βj (assume confidence level is 95%)
print(confint(full_model))
# 0 is not included in all CI of coefficients


#######################
## Diagnostic Checks ##ori-model
#######################

## 1. Check error assumptions
# 1.1 identical distributed (constant variance)
par(mfrow=c(1, 2))
plot(full_model,1)
abline(h = 0,col= 'blue')  

# 1.2 normaml distributed
plot(full_model,2)

# 1.3 error independent (uncorrelated under N. dist.)
# Durbin-Watson test
library(lmtest)
dwtest(full_model)
# H0: errors are uncorrelated
# p-value is really low, so reject H0


## 2. Find unusual observations
# 2.1 Leverage
par(mfrow=c(1, 2))
plot(full_model,5)
# 29643th, 37131th and 46083th observations

# 2.2 Outliers
# jackknife
jack = rstudent(full_model)
jack[which.max(abs(jack))]
# 46083th is the highest
n = nrow(data_num) - 1
df = n-8-1
qt(0.05/(n*2),df)
# H0: 46083th is not an outlier
# 12.37>4.71, so reject H0, treat it as an outlier

# 2.3 Influential observations
plot(full_model,4)
# 46083th observations are really high

# try to remove them
cook = cooks.distance(full_model)
exclude46083 = lm(
  median_house_value ~ longitude + latitude + house_median_age + total_rooms + total_bedrooms + population + households + median_income, 
  subset = cook < max(cook))
summary(exclude46083)
summary(full_model)
# households changed by 27.8%

## 3. Check structural part (partial regression)
par(mfrow=c(1, 1))
library(car)
crPlots(full_model)
# log-transformation 4 variables
log_model = lm(
  median_house_value ~ longitude + latitude + house_median_age + log(total_rooms) + log(total_bedrooms) + log(population) + log(households) + median_income, 
  data = data_num)
summary(log_model)
print(confint(log_model))
par(mfrow=c(2, 2))
plot(log_model)

crPlots(log_model)

# Box-Cox for transformation of response variables
library(MASS)
par(mfrow=c(1, 1))
boxcox(log_model,plotit = T)
# lambda is roughly in [0.2,0.25], consider the right-skewed distribution and huge amount of count mode, apply log transformation
log_model2 = lm(
  log(median_house_value) ~ longitude + latitude + house_median_age + log(total_rooms) + log(total_bedrooms) + log(population) + log(households) + median_income, 
  data = data_num)
summary(log_model2)
print(confint(log_model2))
par(mfrow=c(2, 2))
plot(log_model2)

crPlots(log_model2)

# Unusual observations
par(mfrow=c(2,2))
plot(log_model2,4)
# 50010th observations
jack = rstudent(log_model2)
jack[which.max(abs(jack))]
# 27567th is the highest
n = nrow(data_num) - 1
df = n-8-1
qt(0.05/(n*2),df)
#-7.9<-4.7, 27567 is not an outlier


## 4. Check the collinearity
data_log = data_num
data_log$total_rooms = log(data_log$total_rooms)
data_log$total_bedrooms = log(data_log$total_bedrooms)
data_log$population = log(data_log$population)
data_log$households = log(data_log$households)
data_log$median_house_value = log(data_log$median_house_value)

library('PerformanceAnalytics')
numeric_data <- data_log[, sapply(data_log, is.numeric)]
chart.Correlation(numeric_data, histogram = TRUE, pch = 19)

cor(data_log)

vif(log_model2)
# total_rooms, total_bedrooms and households have VIF > 10
# exclude total bedrooms
log_model3 = lm(
  log(median_house_value) ~ longitude + latitude + house_median_age + log(total_rooms) + log(population) + log(households) + median_income, 
  data = data_num)
summary(log_model3)
par(mfrow=c(2, 2))
plot(log_model3)

vif(log_model3)
# exclude household
log_model4 = lm(
  log(median_house_value) ~ longitude + latitude + house_median_age + log(total_rooms) + log(population) + median_income, 
  data = data_num)
summary(log_model4)
par(mfrow=c(2, 2))
plot(log_model4)

vif(log_model4)


## 5.Variable selection
cur_model = log_model4
# 5.1 All-subset selection
library(leaps)
regfit_full = regsubsets(log(median_house_value)~.,data = data_num, method = 'exhaustive',nvmax = 8)
reg.summary = summary((regfit_full))
summary(regfit_full)

# best subset
# 5.1.1 adjusted R^2
par(mfrow=c(1,2))
plot(reg.summary$rsq, xlab = 'Number of variables', type = 'b', ylab = 'RSq')
plot(reg.summary$adjr2, xlab = 'Number of variables', ylab = 'Adjusted RSq')
# From x=5, adjusted Rsq increases slowly
par(mfrow=c(1,1))
plot(regfit_full, scale = 'adjr2')
coef(regfit_full,id = 8)

# 5.1.2 BIC
plot(regfit_full,scale = 'bic')
coef(regfit_full,id = 8)

# 5.1.3 Mellow'Cp
which.min(reg.summary$cp)
coef(regfit_full,id = 8)


# 5.2 stepwise selection
model_stepwise = step(cur_model,direction = 'both',trace = 0)
summary(model_stepwise)
aic_value = AIC(model_stepwise)
cat("AIC value:", aic_value, "\n")

# 5.3 backward selection
drop1(log_model2,~ longitude + latitude + house_median_age + log(total_rooms) + log(total_bedrooms) + log(population) + log(households) + median_income, test = 'F')
result_table <- drop1(log_model2, ~ . - longitude - latitude - house_median_age - log(total_rooms) - log(total_bedrooms) - log(population) - log(households) - median_income, test = 'F')
print(result_table)
