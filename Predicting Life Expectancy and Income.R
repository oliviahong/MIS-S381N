################################################################################################################################
# This group project is an extension of Hans Rosling's video entitled 200 Countries, 200 Years, 4 Minutes. 
# Our objective was to take the forecast for life expectancy and income of around 200 countries out until 2050, 
# using a variety of economic indicators and modeling techniques. I contributed the mlp (multilayer perceptron) section
# and compiling the code for all modeling techniques to pick the best model. 
################################################################################################################################

library(xtable)
library(fpp)
library(rpart)
library(RSNNS)
ys<-read.csv("~/R Code/APM Project/y_data.csv")
xs<-read.csv('~/R Code/APM Project/indicators_over_time.csv')

list<-list()

#####################
# final models and forecasts
#####################
modellist_i <- list() 
modellist_a <- list() 
predlist_i <- list() 
predlist_a <- list() 

#####################
# store model statistics
#####################
pick_tree_i <- 0
pick_reg_i <- 0
pick_mlpm_i <- 0
pick_tree_a <- 0
pick_reg_a <- 0
pick_mlpm_a <- 0
error_tree_i <- 0
error_tree_a <- 0
error_reg_i<- 0
error_reg_a<- 0
error_mlpm_i<- 0
error_mlpm_a<-0

# store rmse in table, one table for each model
tree<-NULL
tree$rmse_i<-NULL
tree$rmse_a<-NULL
reg<-NULL
reg$rmse_i<-NULL
reg$rmse_a<-NULL
mlpm<-NULL
mlpm$rmse_i<-NULL
mlpm$rmse_a<-NULL

for (i in unique(xs$Country.Name)){
  features<-list()
  print (i)
  xx<-subset.data.frame(xs, Country.Name==i)
  X<-as.data.frame(t(xx))
  X<-X[,1:ncol(X)]
  new_header<-X[1,]
  X<-X[2:nrow(X),]
  #colnames(X, new_header)
  
  # Save y series in memory for later modeling
  income<-subset.data.frame(ys, Country==i)[3]
  age<-subset.data.frame(ys, Country==i)[4]
  
  # PCA
  d<-X[,apply(X[2:22,], 2, var, na.rm=TRUE) != 0]
  d<-as.data.frame(d[2:22,])
  dd=apply(d, 2, as.numeric)
  pca<-prcomp(dd, scale=T, center=T)
  var<-varimax(pca$rotation)
  
  if(i=="Afghanistan"){
    plot( cumsum((pca$sdev)^2) / sum(pca$sdev^2), main=paste('All Countries','Scree Plot'), xlab='Components', ylab='Cumulative Proportion of Variance Explained')
  }
  else{
    points(cumsum((pca$sdev)^2) / sum(pca$sdev^2))
  }
  
  # Get 3 factors from PC1, 2 from PC2, 1 from PC3-5
  table<-xtable(unclass(var$loadings))
  list<-append(list, paste(i, 'Extracted Features:', xs[2][names(d)[which.max(abs(table$PC1))],] ,'|', xs[2][names(d)[match(tail(sort(abs(table$PC1)),3), abs(table$PC1))[2]],] ,'|', xs[2][names(d)[match(tail(sort(abs(table$PC1)),3), abs(table$PC1))[1]],], '|', xs[2][names(d)[match(tail(sort(abs(table$PC2)),2+length(intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))), abs(table$PC2))[match(tail(sort(abs(table$PC2)),2+length(intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))), abs(table$PC2))!=ifelse(length(intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))==0, 0, intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))][1]],] , '|', xs[2][names(d)[match(tail(sort(abs(table$PC2)),2+length(intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))), abs(table$PC2))[match(tail(sort(abs(table$PC2)),2+length(intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))), abs(table$PC2))!=ifelse(length(intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))==0, 0, intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))][2]],], '|', xs[2][names(d)[match(tail(sort(abs(table$PC3)),1+length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))), abs(table$PC3))[match(tail(sort(abs(table$PC3)),1+length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))), abs(table$PC3))!=ifelse(length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))==0, 0, Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))][1]],], '|', xs[2][names(d)[match(tail(sort(abs(table$PC4)),1+length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))), abs(table$PC4))[match(tail(sort(abs(table$PC4)),1+length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))), abs(table$PC4))!=ifelse(length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))==0, 0, Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))][1]],], '|', xs[2][names(d)[match(tail(sort(abs(table$PC5)),1+length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)), match(tail(sort(abs(table$PC5)),1), abs(table$PC5)), match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))), abs(table$PC5))[match(tail(sort(abs(table$PC5)),1+length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)), match(tail(sort(abs(table$PC5)),1), abs(table$PC5)), match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))), abs(table$PC5))!=ifelse(length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)), match(tail(sort(abs(table$PC5)),1), abs(table$PC5)), match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))==0, 0, Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)), match(tail(sort(abs(table$PC5)),1), abs(table$PC5)), match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))][1]],]))
  
  
  features<-paste(which.max(abs(table$PC1)) , match(tail(sort(abs(table$PC1)),3), abs(table$PC1))[2] , match(tail(sort(abs(table$PC1)),3), abs(table$PC1))[1], match(tail(sort(abs(table$PC2)),2+length(intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))), abs(table$PC2))[match(tail(sort(abs(table$PC2)),2+length(intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))), abs(table$PC2))!=ifelse(length(intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))==0, 0, intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))][1], match(tail(sort(abs(table$PC2)),2+length(intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))), abs(table$PC2))[match(tail(sort(abs(table$PC2)),2+length(intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))), abs(table$PC2))!=ifelse(length(intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))==0, 0, intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))][2], match(tail(sort(abs(table$PC3)),1+length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))), abs(table$PC3))[match(tail(sort(abs(table$PC3)),1+length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))), abs(table$PC3))!=ifelse(length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))==0, 0, Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))][1], 
match(tail(sort(abs(table$PC4)),1+length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))), abs(table$PC4))[match(tail(sort(abs(table$PC4)),1+length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))), abs(table$PC4))!=ifelse(length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))==0, 0, Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))][1], 
match(tail(sort(abs(table$PC5)),1+length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)), match(tail(sort(abs(table$PC5)),1), abs(table$PC5)), match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))), abs(table$PC5))[match(tail(sort(abs(table$PC5)),1+length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)), match(tail(sort(abs(table$PC5)),1), abs(table$PC5)), match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))), abs(table$PC5))!=ifelse(length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)), match(tail(sort(abs(table$PC5)),1), abs(table$PC5)), match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))==0, 0, Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)), match(tail(sort(abs(table$PC5)),1), abs(table$PC5)), match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))][1])
  
  # below code to get the forecasts
  green<-NULL
  green_test<-NULL
  nom<-NULL
  
  d_test<-X[,apply(X[2:22,], 2, var, na.rm=TRUE) != 0]
  d_test<-as.data.frame(d_test[23:62,])
  dd_test=apply(d_test, 2, as.numeric)
  
  for (i in unique(as.numeric(unlist(strsplit(features, "\\s+"))))){
    green <- rbind(green, dd[,i])
    nom <- cbind(nom, as.character(xs[2][names(d)[i],]))
    row.names(green) <- nom
    
    green_test <- rbind(green_test, dd_test[,i])
    row.names(green_test) <- nom
    
  }
  green <- t(green)
  green_test <- t(green_test)
  
  #####################
  # arima
  #####################
  
  # convert model inputs to dataframes
  x_train<-as.data.frame(green)
  x_test<-as.data.frame(green_test)
  y_train<-income[1:21,]
  y_test<-income[22:nrow(income),]
  xxx<-cbind(y_train, x_train)
  y_train_age<-age[1:21,]
  xxxx<-cbind(y_train_age, x_train)
  y_test_age<-age[22:nrow(age),]
  
  arima_income<-lm(formula = y_train~.,data = xxx)
  arima_age<-lm(formula = y_train_age~.,data = xxxx)
  # make pred 2011-2050
  pred_income<-predict(arima_income, newdata = x_test)
  pred_age<-predict(arima_age, newdata = x_test)
  rmse_income <- sqrt( mean( (pred_income[1:5] - y_test)^2 , na.rm = TRUE ) )
  rmse_age <- sqrt( mean( (pred_age[1:5] - y_test_age)^2 , na.rm = TRUE ) )
  
  error_reg_i <- error_reg_i + rmse_income
  error_reg_a <- error_reg_a + rmse_age
  
  reg<-rbind(reg,c(rmse_income,rmse_age))
  
  #####################
  # tree
  #####################
  # income
  train_i<-cbind(income[1:21,], green)
  colnames(train_i)[1]<-'Income'
  test_i<- cbind(income[22:26,], x_test)
  colnames(test_i)[1]<-'Income'
  
  # age
  train_a<-cbind(age[1:21,], green)
  colnames(train_a)[1]<-'Age'
  test_a <- cbind(age[22:26,], x_test)
  colnames(test_a)[1]<-'Age' 
  
  train_i<-data.frame(train_i)
  test_i<-data.frame(test_i)
  train_a<-data.frame(train_a)
  test_a<-data.frame(test_a)
  
  test_i$Income <- 1
  test_a$Age <- 1
  
  fit_i <- rpart(Income~.,method="anova", data=train_i)
  fit_a <- rpart(Age~.,method="anova", data=train_a)
  ypred_i<-predict(fit_i,test_i)
  ypred_a<-predict(fit_a,test_a)
  
  rmse_i <- sqrt(mean((ypred_i[1:5]-y_test)^2, na.rm=TRUE))
  rmse_a <- sqrt(mean((ypred_a[1:5]-y_test_age)^2, na.rm=TRUE))
  
  error_tree_i <- error_tree_i + rmse_i
  error_tree_a <- error_tree_a + rmse_a
  
  tree<-rbind(tree,c(rmse_i, rmse_a))
  
  #####################
  # mlp
  #####################
  scale_data <- function(data){
    maxs <- apply(data, 2, max) 
    mins <- apply(data, 2, min)
    scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
    return(scaled)
  }
  
  datatr <- data.frame(green, age[1:21,], income[1:21,])
  datate <- data.frame(green_test[1:5,],  age[22:26,], income[22:26,])
  colnames(datatr)[ncol(datatr)-1] <- "Age"
  colnames(datatr)[ncol(datatr)] <- "Income"
  colnames(datate)[ncol(datate)-1] <- "Age"
  colnames(datate)[ncol(datate)] <- "Income"
  combinetrte <-rbind(datatr, datate)
  
  scaled <- scale_data(combinetrte)
  
  index <- 1:21
  train <- scaled[index,]
  test <- scaled[-index,]
  
  xtr_i <- subset(train, select = -c(Income,Age))
  ytr_i <- subset(train, select = c(Income))
  xte_i <- subset(test, select = -c(Income,Age))
  yte_i <- subset(test, select = c(Income))
  
  xtr_a <- subset(train, select = -c(Age,Income))
  ytr_a <- subset(train, select = c(Age))
  xte_a <- subset(test, select = -c(Age,Income))
  yte_a <- subset(test, select = c(Age))
  
  x11to50 <- x_test
  
  # choose the best model
  mlpcv <- function(data, train_x, train_y, test_x, test_y, test_x_long, cname){
    mlpmodels <- list()
    mlprmses <- list()
    for (i in 1:8){
      model1 <- mlp(x=train_x, y=train_y, size=i, learnFuncParams=c(0.1), maxit=50, linOut=F, inputsTest=test_x, targetsTest=test_y)
      model2 <- mlp(x=train_x, y=train_y, size=i, learnFuncParams=c(0.1), maxit=50, linOut=T, inputsTest=test_x, targetsTest=test_y)
      
      pred_unscale1 <- predict(model1, test_x)*(max(data[,cname])-min(data[,cname]))+min(data[,cname])
      pred_unscale2 <- predict(model2, test_x)*(max(data[,cname])-min(data[,cname]))+min(data[,cname])
      test_y_unscale <- (test_y[,cname])*(max(data[,cname])-min(data[,cname]))+min(data[,cname])
      rmse1 <- sqrt(mean((test_y_unscale - pred_unscale1)^2, na.rm=TRUE))
      rmse2 <- sqrt(mean((test_y_unscale - pred_unscale2)^2, na.rm=TRUE))

      if (rmse1 < rmse2){
        model <- model1
        rmse <- rmse1
      }
      else{
        model <- model2
        rmse <- rmse2
      }
      key <- toString(i)
      mlpmodels[[key]] <- model
      mlprmses[[key]] <- rmse
    }
    
    finalmlp <- mlpmodels[[which.min(mlprmses)]]
    finalmlprmse <- mlprmses[[which.min(mlprmses)]]
    finalmlppred <- predict(model, test_x_long)*(max(data[,cname])-min(data[,cname]))+min(data[,cname])
    newList <- list("model" = finalmlp, "error" = finalmlprmse, "pred" = finalmlppred)
    return(newList)
  }
  
  incomemlp <- mlpcv(combinetrte, xtr_i,ytr_i,xte_i,yte_i,x11to50,"Income")
  mlp_i <- incomemlp$model
  mlprmse_i <- incomemlp$error
  mlppred_i <- incomemlp$pred
  
  agemlp <- mlpcv(combinetrte, xtr_a,ytr_a,xte_a,yte_a,x11to50,"Age")
  mlp_a <- agemlp$model
  mlprmse_a <- agemlp$error
  mlppred_a <- agemlp$pred
  
  error_mlpm_i <- error_mlpm_i + mlprmse_i
  error_mlpm_a <- error_mlpm_a + mlprmse_a
  
  mlpm<-rbind(mlpm,c(mlprmse_i, mlprmse_a))

  #####################
  # pick best model
  #####################
  #income
  if (rmse_i<rmse_income & rmse_i<mlprmse_i){ # pick tree
    final_i_model <- fit_i
    final_i_pred <- ypred_i
    pick_tree_i <- pick_tree_i + 1
    
  }
  else if (rmse_income<rmse_i & rmse_income<mlprmse_i){ # pick arima
    final_i_model <- arima_income
    final_i_pred <- pred_income
    pick_reg_i <- pick_reg_i + 1
  }
  else{ # pick mlp
    final_i_model <- mlp_i
    final_i_pred <- mlppred_i
    pick_mlpm_i <- pick_mlpm_i + 1
    
  }
  #age
  if (rmse_a<rmse_age & rmse_a<mlprmse_a){ # pick tree
    final_a_model <- fit_a
    final_a_pred <- ypred_a
    pick_tree_a <- pick_tree_a + 1
    
  }
  else if (rmse_age<rmse_a & rmse_age<mlprmse_a){ # pick arima
    final_a_model <- arima_age
    final_a_pred <- pred_age
    pick_reg_a <- pick_reg_a + 1
  }
  else{ # pick mlp
    final_a_model <- mlp_a
    final_a_pred <- mlppred_a
    pick_mlpm_a <- pick_mlpm_a + 1
  }
  modellist_i <- c(modellist_i, list(final_i_model))
  modellist_a <- c(modellist_a, list(final_a_model))
  predlist_i <- c(predlist_i, list(final_i_pred))
  predlist_a <- c(predlist_a, list(final_a_pred))
  
}
reg<-data.frame(reg)
tree<-data.frame(tree)
mlpm<-data.frame(mlpm)

# make list of all forecasts 2011-2050
all_forecasts_i <- data.frame(matrix(ncol = 188, nrow = 40))
for (i in 1:188){
  all_forecasts_i[,i] <- predlist_i[i]
}
all_forecasts_a <- data.frame(matrix(ncol = 188, nrow = 40))
for (i in 1:188){
  all_forecasts_a[,i] <- predlist_a[i]
}

# make list of all rmse
all_rmse <- data.frame(reg, tree, mlpm)
colnames(all_rmse) <- c("IncomeReg","AgeReg","IncomeTree","AgeTree","IncomeMLP","AgeMLP")

write.table(all_forecasts_a, file = "all_forecasts_age.csv", sep = ",")
write.table(all_forecasts_i, file = "all_forecasts_income.csv", sep = ",")
# write.table(all_rmse, file = "all_rmse.csv", sep = ",")


