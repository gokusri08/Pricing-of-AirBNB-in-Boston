rm(list = ls())
gc()

FinalData = read.csv('final_dataV1.csv', header = T, stringsAsFactors = F, na.strings = '')
#FinalDataWithPC = read.csv('final_data_with_PCsV2.csv', header = T, stringsAsFactors = F, na.strings = '')

dim(FinalData)
#dim(FinalDataWithPC)
View(FinalData)


Cor_Dummies = cor(FinalData[,c(13:length(FinalData))])

library(corrplot)
require(corrplot)


corrplot(Cor_Dummies)
corrplot(cor(FinalData[,c(2:length(FinalData))]), method = 'square')
corrplot(cor(FinalData[,c(6:11)]), method = 'square')

#Removing variables (Review_Cleanliness,communication, value)
#FinalData01 = FinalData[,-c('review_cleanliness', 'review_communication', 'review_value')]
#FinalData01 = FinalData[,-c(10,12,14)]

#View(FinalData01)
#dim(FinalData01)

#####################PCA for Dummies
####################################

'#PCA_D = prcomp(FinalData[,c(13:length(FinalData))], scale. = T)

PCA_D$sdev
summary(PCA_D)
PCA_D$x
dim(PCA_D$x)
View(PCA_D$x)

#PC_ScoresOfDummies = PCA_D$x[,c(1:27)]
class(PC_ScoresOfDummies)
View(PC_ScoresOfDummies)
dim(PC_ScoresOfDummies)

# Replacing the dummies with PCA_Scores of Dummies
FinalData_Temp = FinalData[,-c(13:length(FinalData))]
dim(FinalData_Temp)
View(FinalData_Temp)
#merging both files
FinalData_PCs = data.frame(FinalData_Temp,PC_ScoresOfDummies)

dim(FinalData_PCs)
View(FinalData_PCs)'

#write.csv(FinalData_PCs, file = 'FinalData_w_PCs_Sri.csv')

# Regression with PCs for Dummies
# removing id, lat and long
#FinalData03 = FinalData02[,-c(1,2,3)] (##thery are already removed this time##)

#View(FinalData03)



#Data Partioning
set.seed(1)
id.training = sample(1:nrow(FinalData), nrow(FinalData)*.6)
id.validation = setdiff(1:nrow(FinalData), id.training)

View(FinalData [id.training,])
View(FinalData[id.validation,])
dim(FinalData[id.training,])
dim(FinalData[id.validation,])

#MLR Y Vs X from 1 to al variables
min.model = lm(Price ~ 1, data = FinalData[id.training,])
max.model = lm(Price ~ ., data = FinalData[id.training,])

# Stepwise Selection
Obj.Stepwise = step(min.model, scope = list(lower = min.model, upper = max.model), direction = "both")
summary(Obj.Stepwise)

#Backward Selection
Obj.Backward = step(max.model, direction = "backward")
summary(Obj.Backward)

#Forward Selection
Obj.forward = step(min.model, scope = list(lower = min.model, upper = max.model), direction = "forward")
summary(Obj.forward)
Obj.forward

Obj.forward$call 

Obj.forward$call$formula
'##Price ~ room.entire_apt + accommodates + Cleaning_fee + dorchester.nhbr + 
    bath.one + review_location + prop_house + beacon_hill.nhbr + 
downtown.nhbr + Laundry + east_boston.nhbr + allston.nhbr + 
brighton.nhbr + review_cleanliness + bedtype.real + bed.other + 
jamica_plain.nhbr + review_value + review_checkin + bedroom.one + 
bedroom.other + Free.Parking + Air.Conditioning + reviews_per_month'

# this is the model we choose, so we use the same formula

Model_FS = lm (Obj.forward$call$formula, data = FinalData[id.training,]) 
summary(Model_FS)
#removing review_location, Air.Conditioning, reviews_per_month, review_checkin, Free.Parking, review_value  -due to high p vlue
Model_FS_Final = lm(Price ~ room.entire_apt + accommodates + Cleaning_fee + dorchester.nhbr + 
                      bath.one + prop_house + beacon_hill.nhbr + 
                      downtown.nhbr + Laundry + east_boston.nhbr + allston.nhbr + 
                      brighton.nhbr + review_cleanliness + bedtype.real + bed.other + 
                      jamica_plain.nhbr + bedroom.one + 
                      bedroom.other, data = FinalData[id.training,])
summary(Model_FS_Final)

#Predict future values
yhat = predict(Model_FS_Final, newdata = FinalData[id.validation,])
length(yhat)
length(id.validation)
plot(FinalData[id.validation, 'Price'], yhat, xlab='Actual y', ylab='Fitted y', pch = 16, col = 'red')
#lines(FinalData_PCs[id.training,]$Price, fitted(Model_FS), col = 'blue')
abline(h = mean(FinalData$Price[id.validation]), col = "red")


install.packages("hydroGOF")
require(hydroGOF)
rmse(FinalData[id.validation, 'Price'], yhat)

class(Model_FS_Final$residuals)

residuals_FS = as.data.frame(Model_FS_Final$residuals)
colnames(residuals_FS)
class(residuals_FS)
######### Check model assumption##########
##########################################
# normality (of the residual)
hist(Model_FS_Final$residuals, col ='green')
require(ggplot2)
ggplot(residuals_FS, aes(x= 'Model_FS_Final$residuals') + geom_histogram(breaks = seq(-150, 200, by = 50), col = 'red', aes(fill =..count..)) + scale_fill_gradient("count", low="green", high = "red"))

# linearity
plot(FinalData$Security_deposit, FinalData$Price, col = 'red')
plot(FinalData$room.entire_apt, FinalData$Price, col = 'red')
plot(FinalData$Cleaning_fee, FinalData$Price, col ='red')
plot(FinalData$accommodates, FinalData$Price, col ='red')
#plot(FinalData$PC1, FinalData$Price, col = 'red')

# Homoscedasticity
plot(Model_FS_Final$residuals, Model_FS_Final$fitted.values)
plot(Model_FS_Final$fitted.values, Model_FS_Final$residuals, col = 'blue')

# there are some other assumptions such as "No Multicollinearity"

## A simpler approach to do model diagnosis
layout(matrix(c(1,2,3,4),2,2)) # check the difference by removing this line
plot(obj)

pairs(FinalData[1:12])

