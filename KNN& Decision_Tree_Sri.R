rm(list=ls()); gc()
setwd('/Users/yinfeikong/Documents/aFullerton/ISDS 574 - 2017 Spring/Rcodes/')

# require(xlsx)
# dat = read.xlsx('RidingMowers.xls', sheetIndex = 1, stringsAsFactors=F, head=T)

datforKnn = read.csv('Final_DataV1.csv', stringsAsFactors=T, head=T)
View(datforKnn)
class(datforKnn)
dim(datforKnn)

# Regression based KNN
scaled.data = scale(datforKnn)
class(scaled.data)
scaled.dataknn = as.data.frame(scaled.data)
scaled.dataknn$Price
class(scaled.dataknn)
scaled.dataknn$Price <- datforKnn$Price 
scaled.dataknn$Price
View(scaled.dataknn)

id.training_Scaled = sample(1:nrow(scaled.dataknn), nrow(scaled.dataknn)*.6)
id.validation_Scaled = setdiff(1:nrow(scaled.dataknn), id.training_Scaled)

train_k = scaled.dataknn[id.training_Scaled, c(2:length(scaled.dataknn))]
test_k = scaled.dataknn[id.validation_Scaled, c(2:length(scaled.dataknn))]
y_train = scaled.dataknn[id.training_Scaled, 1]
y_test = scaled.dataknn[id.validation_Scaled, 1]

#install.packages('FNN')
require(FNN)
#install.packages("hydroGOF")
require(hydroGOF)

# Selecting the best k for kNN regression
knn.bestk_reg = function(train_k, test_k, y_train, y_test, k.max = 20) {
  
  #each_rmse = rep(NA, k.max)
  each_rmse <- as.data.frame(matrix(0, ncol = 4, nrow = k.max))
  colnames(each_rmse) <- c("K", "RMSE", "MSE", "MAD") # MAD looks erroneous // NOT ERRONEOUS ANYMORE :)
  for (i in 1:k.max){
    
    knn_reg_obj_val = knn.reg(train_k, test_k,
                              y_train, k = i)
    
    each_rmse[i,1] <- i
    each_rmse[i,2] <- rmse(y_test, knn_reg_obj_val$pred)
    each_rmse[i,3] <- mse(y_test, knn_reg_obj_val$pred)
    each_rmse[i,4] <- mad(y_test, median(knn_reg_obj_val$pred))
    
  }
  return(each_rmse)
}

#class(each_rmse)


'#knn_reg_errors = knn.bestk_reg(scaled.dataknn[id.training_scaled, 1:(length(scaled.dataknn)-1)],
                               scaled.dataknn[id.validation_scaled, 1:(length(scaled.dataknn)-1)],
                               scaled.dataknn[id.training_scaled,]$Price, scaled.dataknn[id.validation_scaled,]$Price)'
knn_reg_errors = knn.bestk_reg(train_k, test_k, y_train, y_test)
knn_reg_errors

knn_reg.rmse = knn_reg_errors[which.min(knn_reg_errors$RMSE),which(names(knn_reg_errors) == "K")]
knn_reg.rmse #<-------------------------------------------Best K
knn_reg_errors[knn_reg.rmse, 2]#<-------------------------------------Besk K's RMSE = 53.98684
class(knn_reg_errors)

write.csv(knn_reg_errors, file = 'KNN_Table_new.csv')
###something I tried
require(ggplot2)
ggplot(plot.df, aes(Sepal.Length, Sepal.Width, color = predicted, fill = predicted)) + 
  geom_point(size = 5) + 
  geom_polygon(data = boundary, aes(x,y), alpha = 0.5)
#############

#Regression Tree
require('rpart')

# reg_toyota_data = toyota_data[,1:(length(toyota_data))]

reg_tree = rpart(Price ~ ., method = "anova", data = toyota_data)
summary(reg_tree)
printcp(reg_tree)
plotcp(reg_tree)

par(mfrow = c(1,2))
rsq.rpart(reg_tree)

plot(reg_tree, uniform=T, main="Regression Tree")
text(reg_tree, use.n=T, all=T, cex=.8)

which.min(reg_tree$cptable[,"xerror"])

prune_tree = prune(reg_tree,
                   cp = reg_tree$cptable[which.min(reg_tree$cptable[,"xerror"]),"CP"])

plot(prune_tree, uniform=T, main="Best Prune Tree")
text(prune_tree, use.n=T, all=T, cex=.8)

summary(prune_tree)

# We find that the tree with 3 splits is the best pruned tree

# Trying cross validation with separate training and test data

tree = rpart(Price ~. ,method = "anova" ,data = toyota_data[id.train,])
id.cp = which.min(tree$cptable[,"xerror"])

tree.std_dev = tree$cptable[id.cp ,5]
tree.cv_error = tree$cptable[id.cp,4]  #standard error of the lowest cv error

tree.cv_error + tree.std_dev
# How much standard error can we afford to prune the tree?

# We may have to settle down with 3 splits. Let's see!

pruned_tree2 = prune(tree, cp = tree$cptable[which.min(reg_tree$cptable[,"xerror"]),"CP"])
summary(pruned_tree2)

# We were correct in our assumption. 3 splits it is!

pData1 <- predict(tree, toyota_data[id.validation,])
summary(pData1)
summary(tree)

tree.rmse = rmse(toyota_data[id.validation, 'Price'], pData1)
tree.rmse
# RMSE = 1619.5318 when full tree is trained

pData2 <- predict(pruned_tree2, toyota_data[id.validation,])
summary(pData2)
summary(tree)
pruned_tree2.rmse = rmse(toyota_data[id.validation, 'Price'], pData2) #<---Regression Tree RMSE = 1639.08
pruned_tree2.rmse

# RMSE = 1696.206 with pruned tree. Saves us some time, huh?

#--------------------------- Classification section starts here ----------------------------#

classify_dat = toyota_data[,- which(colnames(toyota_data) == 'Price')]
classify_dat$high_price <- high_price
View(classify_dat)

# Fitting a Logistic Regression model
table(classify_dat$high_price)

dat.train = classify_dat[id.train,]
dat.valid = classify_dat[id.validation,]

min.model_logit = glm(high_price ~ 1, data = dat.train, family = 'binomial')
max.model_logit = glm(high_price ~ ., data = dat.train, family = 'binomial')

forward.logit = step(min.model_logit, scope = list(lower = min.model_logit, upper = max.model_logit),
                     direction = 'forward')

formula(forward.logit)
summary(forward.logit)

out = matrix(NA, 11, 3)
colnames(out) = c('OR', 'SE', 'pval')
rownames(out) = rownames(summary(forward.logit)$coefficients)

out[,1] = exp(summary(forward.logit)$coefficient[,1])
out[,2] = out[,1] *exp(summary(forward.logit)$coefficient[,2])
out[,3] = exp(summary(forward.logit)$coefficient[,4])

yhat = predict(forward.logit, newdata = dat.valid, type = 'response')
hist(yhat)

threshold = function(yhat, cutoff=.5) {
  temp_pred = rep(0, length(yhat))
  temp_pred[yhat > cutoff] = 1
  return(temp_pred)
}

yhat.class = threshold(yhat)
hist(yhat.class)
table(yhat.class)

# confusion matrix
cm <- table(actual = dat.valid$high_price, predicted = yhat.class)

# mean misclassification error
# sum(yhat.class != dat.valid$high_price)/length(id.validation)
miss_class.error <- 1- (sum(diag(cm))/sum(cm))
miss_class.error # Misclassification error = 0.0918 --> 9.18%

# sensitivity
sentvty = cm[2,2]/sum(cm[2,2],cm[2,1]) # 0.7714
# specificity
specfty = cm[1,1]/sum(cm[1,1],cm[1,2]) # 0.9581
#FPR
fpr = cm[1,2]/sum(cm[1,2],cm[2,2]) # 0.1290
#FNR
fnr = cm[2,1]/sum(cm[2,1],cm[1,1]) # 0.0802

# A hand-written function to plot the ROC curve :)
ROC_curve = function(predicted, actual = dat.valid$high_price, roc_step = 0.002){
  
  roc_size = (1/roc_step) - 1
  
  out = matrix(NA, roc_size, 3)
  colnames(out) <- c('Cut-off', '1-specificity', 'sensitivity')
  
  count = 1
  for(i in seq(0,1, by = roc_step)){
    
    if(i == 1) break;
    if(i == 0) next;
    
    x = threshold(predicted,i)
    cm <- table(actual, x)
    sentvty = cm[2,2]/sum(cm[2,2],cm[2,1])
    specfty = cm[1,1]/sum(cm[1,1],cm[1,2])
    
    out[count,1] <- i
    out[count,2] <- 1-specfty
    out[count,3] <- sentvty
    
    count = count+1
  }
  return(out)
}

roc_obj = ROC_curve(yhat, roc_step = 0.0001)
dev.off()
plot(roc_obj[,2],roc_obj[,3],xlab = '1 - Specificity', ylab = 'Sensitivity')

# Classification based kNN (the data is already normalized in the scaled.dat data.frame)

require(class)
scaled.dat <- scaled.dat[,-30] # Removing the Price feature
scaled.dat$high_price <- high_price # Adding the high_price feature


knn_class_obj = knn(train = scaled.dat[id.train, 1:(length(scaled.dat)-1)],
                    test =  scaled.dat[id.validation, 1:(length(scaled.dat)-1)],
                    scaled.dat[id.train,]$high_price,
                    k = 11)
table(knn_class_obj)

knn.bestk_class = function(my_train, my_test, my_y.train, my_y.validation, k.max = 20){
  
  each_val_error <- as.data.frame(matrix(0, ncol = 3, nrow = k.max))
  colnames(each_val_error) <- c('k', 'training error', 'validation error')
  
  for(i in 1:k.max){
    
    knn_class_obj = knn(train = my_train,
                        test =  my_test,
                        my_y.train,
                        k = i)
    
    knn_class_obj_train = knn(train = my_train,
                              test =  my_train,
                              my_y.train,
                              k = i)
    
    each_val_error[i,1] <- i
    each_val_error[i,2] <- (sum(knn_class_obj_train != my_y.train))/length(my_y.train)
    each_val_error[i,3] <- (sum(knn_class_obj != my_y.validation))/length(my_y.validation)
    
  }
  
  return(each_val_error)
  
  
} 

knn_class_result = knn.bestk_class(scaled.dat[id.train, 1:(length(scaled.dat)-1)],
                                   scaled.dat[id.validation, 1:(length(scaled.dat)-1)],
                                   scaled.dat[id.train,]$high_price,
                                   scaled.dat[id.validation,]$high_price)

knn_class_result # Hurray! 

#lowest_class_error = knn_class_result[which.min(knn_class_result$`validation error`),
#                                which(names(knn_class_result) == "validation error")]

best_k_class = which.min(knn_class_result$`validation error`) # I am retrieving the index, not the k. Change this

best_k_class # <---------------------------------- the best k is k = 7 

# Using k = 7 to train a new kNN model
knn_class.best = knn(train = scaled.dat[id.train, 1:(length(scaled.dat)-1)],
                     test =  scaled.dat[id.validation, 1:(length(scaled.dat)-1)],
                     scaled.dat[id.train,]$high_price,
                     k = 7)
table(knn_class.best)
cm_knn <- table(actual = scaled.dat[id.validation,]$high_price, predicted = knn_class.best)
knn.error <- 1- (sum(diag(cm_knn))/sum(cm_knn))
knn.error #  Misclassification error = 0.0969 --> 9.69%

# ---------------- CART ------------------------

# Full Tree
class_tree = rpart(high_price ~ ., method = "class",
                   data = classify_dat[id.train,], minsplit = 2, minbucket = 1) # This should give us the full tree
printcp(class_tree)
plot(class_tree, uniform=T, main="Classification Tree")
text(class_tree, use.n=T, all=T, cex=.8)

# Pruned Tree
class_tree_pruned = prune(class_tree, cp = class_tree$cptable[which.min(class_tree$cptable[,"xerror"]),"CP"])
plot(class_tree_pruned, uniform = T, main = "Minimum Error Tree")
text(class_tree_pruned, use.n=T, all=T, cex=.8)

# Best pruned tree
id.cp = which.min(class_tree$cptable[,"xerror"])
tree.std_dev = class_tree$cptable[id.cp, 5]
tree.cv_error = class_tree$cptable[id.cp, 4]
tree.cv_error + tree.std_dev # 0.3746502
# The best pruned tree can have 5 splits, as per the cptable. We use cp = 0.013986
class_best.pruned = prune(class_tree, cp = 0.014) # For some reason this cut-off value for cp gives us the desired split
printcp(class_best.pruned)
plot(class_best.pruned, uniform = T, main = "Best Pruned Tree")
text(class_best.pruned, use.n=T, all=T, cex=.8)

class.predict = predict(class_best.pruned, newdata = classify_dat[id.validation,], type = "class")

# confusion matrix
cm <- table(actual = classify_dat[id.validation,]$high_price, predicted = class.predict)
class.error <- 1- (sum(diag(cm))/sum(cm))
class.error # Misclassification error = 0.0892 --> 8.92%
