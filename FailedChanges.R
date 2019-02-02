## These are all the changes done to the code. Methods that improves the accuracy is kept while those that doesn't are dropped.

## Baseline code - 81.7%
w = read.csv("weather_training.csv")

w = w[-1:-2]
# w = w[-21] remove RISK_MM

library(stringr)
for (i in c(“WindGustDir”, “WindDir9am”, “WindDir3pm”)){
# There is a tab infront of the character so we run:

a = str_replace_all(string=w[,i], pattern=” “, repl=””)
w=w[a!='nan',]
}

w=na.omit(w)

a = str_replace_all(string=w$RainToday, pattern=” “, repl=””)
w$RainToday = ifelse(a == "No", 0, 1)

mod = lm(w$RainTomorrow~., data=w)
cookdst = cooks.distance(mod) # 154 is NA, idk why
cookdst[cookdst>4*mean(cookdst, na.rm=T)] #norm is 4*mean
w = na.omit(w[!cookdst>4*mean(cookdst, na.rm=T),])


library(rpart)

total = 0
for (i in 1:150){
	a = sample(nrow(w),nrow(w)*0.70)
	train = w[a,]
	test = w[-a,]

	dtree = rpart(RainTomorrow~.,data=train, method="class")

	# Test accuracy - 81.7%
	pred = predict(dtree, test, type="class")
	total = c(total, mean(test$RainTomorrow == pred))
}
total = total[-1]
summary(total)

post(dtree, file="")



## Improvement - averaging - 80.8%
avgTemp = (w[,"MinTemp"]+ w[,"MaxTemp"])/2
avgHumidity = (w[,"Humidity9am"] + w[,"Humidity3pm"] )/2
avgCloud = (w[,"Cloud9am"] + w[,"Cloud3pm"])/2
avgPressure = (w[,"Pressure9am"] + w[,"Pressure3pm"])/2

a = c("Rainfall", "Evaporation", "Sunshine", "WindGustDir", "WindGustSpeed", "WindDir9am", "WindDir3pm", "RainToday", "RainTomorrow")
# omitted RainToday as Rainfall is a better guage
w=w[, a]

w$AvgTemp = avgTemp
w$AvgHumidity = avgHumidity
w$AvgCloud = avgCloud
w$AvgPressure = avgPressure

rm(avgCloud, avgHumidity, avgPressure, avgTemp)


## Improvement - add seasons - 81.5%
dates = as.Date(w$Date, format = '%m/%d/%Y')
month = as.numeric(strftime(dates, '%m'))

# a = matrix(ncol=4)
a = ''
for (i in month){
	if (i >= 3 && i <= 5){
		# a = rbind(a, c(1, 0, 0, 0))
		a = c(a, 'Autumn')
	} else if (i >= 6 && i <= 8){
		# a = rbind(a, c(0, 1, 0, 0))
		a = c(a, 'Winter')
	} else if (i >= 9 && i <= 11){
		# a = rbind(a, c(0, 0, 1, 0))
		a = c(a, 'Spring')
	} else if (i == 12 || i <= 2){
		# a = rbind(a, c(0, 0, 0, 1))
		a = c(a, 'Summer')
	}
}

w$Season = a[-1]
# a = data.frame(a[-1,])
# colnames(a) = c('Autumn', 'Winter', 'Spring', 'Summer')
# w = cbind(w, a)

## Improvement - normalising - 83.1%
normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  }

w = data.frame(lapply(w, normalize))

## Improvement - feature selection - 88.5%
w=original
w = read.csv("weather_training.csv")

# cleaning
library(stringr)
a = str_replace_all(string=w$RainToday, pattern=” “, repl=””)
w$RainToday = ifelse(a == "No", 0, 1)
str(w)
w=w[c(-1,-2,-8,-10,-11)] # remove all the factor column

w=na.omit(w)
colSums(is.na(w)) #check for na
sapply(w, mean) #if there's nan, mean would be NA

# remove outliers
mod = lm(w$RainTomorrow~., data=w)
cookdst = cooks.distance(mod) # 154 is NA, idk why
cookdst[cookdst>4*mean(cookdst, na.rm=T)] #norm is 4*mean
w = na.omit(w[!cookdst>4*mean(cookdst, na.rm=T),])

# remove RISK_MM
w=w[-18]

l = lm(RainTomorrow ~ ., data = w)
step(l, direction = "both")

# Using RainTomorrow as independent var, removed RISK_MM - 88.5%
w=w[
c("MinTemp", "Evaporation", "Sunshine", "WindGustSpeed", "WindSpeed3pm", "Humidity9am", "Humidity3pm", "Pressure9am", "Pressure3pm", "Temp9am", "RainTomorrow")
]

# Using RISK_MM as independent var, removed RainTomorrow - 87.3%
w=w[
c("Evaporation", "WindGustSpeed", "Humidity9am", "Humidity3pm", "Pressure9am", "Pressure3pm", "Cloud3pm", "RainToday", "RainTomorrow")
]

w=original[
c("MinTemp", "Evaporation", "Sunshine", "WindGustSpeed", "WindSpeed3pm", "Humidity9am", "Humidity3pm", "Pressure9am", "Pressure3pm", "Temp9am", "RainTomorrow")
]

w=na.omit(w)

mod = lm(w$RainTomorrow~., data=w)
cookdst = cooks.distance(mod) # 154 is NA, idk why
cookdst[cookdst>4*mean(cookdst, na.rm=T)] #norm is 4*mean
w = na.omit(w[!cookdst>4*mean(cookdst, na.rm=T),])

# do testing


## All code after this has feature selection implemented


## Improvement - imputation - 87.5%
w$Sunshine[is.nan(w$Sunshine)] = median(w$Sunshine[!is.nan(w$Sunshine)])
w$WindGustSpeed[is.nan(w$WindGustSpeed)] = median(w$WindGustSpeed[!is.nan(w$WindGustSpeed)])
colSums(is.na(w)) #check for na
sapply(w, mean) #if there's nan, mean would be NA

total = 0
for (i in 1:150){
	a = sample(nrow(w),nrow(w)*0.70)
	train = w[a,]
	test = w[-a,]

	train = train[-12]

	l = lm(RISK_MM ~ ., data = train)

	rain = test$RainTomorrow
	test = test[-12]

	# Test accuracy - 81.7%
	pred = predict(l, test)

	pred = ifelse(pred>1.1, 1, 0)

	total = c(total, mean(rain== pred))
}
total = total[-1]
summary(total)


## Improvements - xgboost - 91.4%
library(xgboost)
w=data.matrix(clean)

total = 0
for (i in 1:50){
	a = sample(nrow(w),nrow(w)*0.70)
	train_data = w[a, 1:10]
	train_labels = w[a, 11]
	test_data = w[-a, 1:10]
	test_labels = w[-a, 11]

	dtrain = xgb.DMatrix(data = train_data, label = train_labels)
	dtest = xgb.DMatrix(data = test_data, label = test_labels)
	
	# model = xgb.cv(data = dtrain, nfold = 5, max.depth = 3, nround = 10, objective = "binary:logistic")
	model = xgboost(data = dtrain, max.depth = 3, nround = 10, objective = "binary:logistic")
	pred = predict(model, dtest)
	pred = ifelse(pred>.5, 1, 0)

	total = c(total, mean(test_labels == pred))
}
total = total[-1]
summary(total)



## Me learning R :P

# Rscript to run file
# import file using source('file_name')

myfirstfn = function(x){
	percent = round(x*100, digits=1)
	result = paste(percent, '%', sep='')
	return (result)
}

# myfirstfn(c(1.222, 34.894, 8.9834))

mysecondfn = function(x, multi=100){
	percent = round(x*multi, digits=1)
	return (paste(percent, '%', sep=''))
}


# if...else ladder
mythirdfn = function(x){
	if (x==2){
		return(2)
	} else if (x==3){
		return(3)
	} else if (x==4){
		return(4)
	} else ifelse (x==1, 1, x*2) 
}

# mythirdfn(1)
# mythirdfn(2)
# mythirdfn(3)
# mythirdfn(4)
# mythirdfn(5)

myfourthfn = function(x){
	for (i in 1:x)
		print (i)
}

# myfourthfn(3)

myfifthfn = function(x){
	for (i in x){
		print (ifelse(i%%2, i*3, i*2))
	}
}

myfifthfn(1:10)	

# lambda functions: h=function(x) x/2




#? Unsolved mysteries
w[is.nan(WindSpeed9am),]

mod = lm(RainTomorrow~., data=w)
cookdst = cooks.distance(mod)
cookdst[cookdst>4*mean(cookdst, na.rm=T)]

But idk why are they outliers tho

WindGustDir = as.character(WindGustDir)
wgdstr = str_replace_all(string=WindGustDir, pattern=" ", repl='')
WindGustDir[wgdstr=='nan']
strnan[strnan=='nan']=NA
na.omit(strnan)


for (i in c(WindGustDir, WindDir9am, WindSpeed3pm, RainToday)){
# There is a tab infront of the character so we run:

a = str_replace_all(string=i, pattern=" ", repl="")
w=w[a!='nan',]
}

#? Does not work cause R appearently does this i = vector of values in WindGustDir + WindDir9am +...???? either that or there is some weird stuff going on

library(stringr)
for (i in c(“WindGustDir”, “WindDir9am”, “WindDir3pm”)){
# There is a tab infront of the character so we run:

a = str_replace_all(string=w[,i], pattern=” “, repl=””)
w=w[a!='nan',]
}

#? Does not work as w$"WindGustDir" work outside but refuses to work within the loop
