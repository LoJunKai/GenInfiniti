## The first part below on feature selection can be left out.

## Part 1, Feature selection
# Read the file
w = read.csv("weather_training.csv")

## Data Cleaning
# All Factor columns have this \t in the front of the text which must be removed
library(stringr)
a = str_replace_all(string=w$RainToday, pattern=" ", repl="")

# Change 'Yes' and 'No' into binary values
w$RainToday = ifelse(a == "No", 0, 1)

# Remove all the other factor columns
str(w)
w=w[c(-1,-2,-8,-10,-11)] #these numbers are based on the above code

# Remove Na values
w=na.omit(w)

# Visual check for na values
colSums(is.na(w))
sapply(w, mean) #if there's nan, mean would be NA

# Remove outliers
mod = lm(w$RainTomorrow~., data=w)
cookdst = cooks.distance(mod)
cookdst[cookdst>4*mean(cookdst, na.rm=T)] #norm is 4*mean
w = na.omit(w[!cookdst>4*mean(cookdst, na.rm=T),])

# Remove RISK_MM
w=w[-18]

# Feature selection using stepwise regression in linear regression
l = lm(RainTomorrow ~ ., data = w)
step(l, direction = "both")

# Selected variables

# Coefficients:
#   (Intercept)        MinTemp    Evaporation       Sunshine  WindGustSpeed
#      9.213108      -0.022965       0.031602      -0.033172       0.008809
#  WindSpeed3pm    Humidity9am    Humidity3pm    Pressure9am    Pressure3pm
#     -0.006422       0.006174       0.007033       0.060908      -0.070990
#       Temp9am
#      0.027086


## Part 2, Testing
# Read file again
w = read.csv("weather_training.csv")

# Select wanted features
w=w[
c("MinTemp", "Evaporation", "Sunshine", "WindGustSpeed", "WindSpeed3pm", "Humidity9am", "Humidity3pm", "Pressure9am", "Pressure3pm", "Temp9am", "RainTomorrow")
]

# Remove Na values
w=na.omit(w)

# Remove outliers
mod = lm(w$RainTomorrow~., data=w)
cookdst = cooks.distance(mod) # 154 is NA, idk why
cookdst[cookdst>4*mean(cookdst, na.rm=T)] #norm is 4*mean
w = na.omit(w[!cookdst>4*mean(cookdst, na.rm=T),])

# Import sgboost
library(xgboost)
w=data.matrix(w)

# Run the testing 10 times
total = 0
for (i in 1:10){
	a = sample(nrow(w),nrow(w)*0.70)
	train_data = w[a, 1:10]
	train_labels = w[a, 11]
	test_data = w[-a, 1:10]
	test_labels = w[-a, 11]

	dtrain = xgb.DMatrix(data = train_data, label = train_labels)
	dtest = xgb.DMatrix(data = test_data, label = test_labels)
	
	model = xgboost(data = dtrain, max.depth = 3, nround = 10, objective = "binary:logistic")
	pred = predict(model, dtest)
	pred = ifelse(pred>.5, 1, 0)

	total = c(total, mean(test_labels == pred))
}
total = total[-1]

# Print out the summary of accuracies
summary(total) #91.4%
