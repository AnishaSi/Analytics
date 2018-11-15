#*******************************************
# IDS 575 Project, Spring 2018
#*******************************************
# Group Members:
#======Name====================UIN=====
# Sumeet Kukreja            674668076
# Priya Panchaksharappa     674668076
# Anisha Singhal            662904858
#======================================

library(mice)
library(ROCR)
library(caret)
library(e1071)
library(randomForest)
library(ROSE)
library(rpart)
library(rpart.plot)

# Data import and exploration

enova <- training
backup <- enova
str(enova)

# Removing irrelevant variables
sapply(enova,function(x) sum(is.na(x)))
enova$address_line_2<-NULL
enova$floor_of_unit<-NULL
enova$damage_code<-NULL
enova$average_neighborhood_price<-NULL
enova$misc_features <- NULL
enova$remodel_date<-NULL
enova<-enova[!is.na(enova$build_date),] # only 35 NAs
sapply(enova,function(x) sum(is.na(x)))
enova$sub_type<-as.factor(enova$sub_type)
enova<-enova[!enova$sub_type=="vacant lot",]
enova<-enova[!is.na(enova$schools_in_area),] 
sapply(enova,function(x) sum(is.na(x)))
View(enova)

# Creating a profit variable with value as 1 for profit and 0 for loss
enova$FinalSpending <- enova$initial_price + enova$investment
enova$ProfitLoss <- enova$final_price - enova$FinalSpending
enova$Profit <- ifelse(enova$ProfitLoss > 0, "1" , "0")
enova$Profit<-as.factor(enova$Profit)

# Converting variables in to factors
cols<-c("zone","sub_type","city_name","zip_code","area_type",
        "schools_in_area","public_transit_score","inspection_type","structural_quality_grade",
        "exterior_condition_grade","interior_condition_grade","utilities_grade","damage_and_issue_grade",
        "floors_in_building","floors_in_unit","bedrooms","bathrooms","parking","basement","central_hvac",
        "exterior_color","exterior_material","purchase_decision")
enova[,cols]<-lapply(enova[,cols],factor)

#Collaborating the data set
cols<-c("property_id","street_name","street_number","city_name","zip_code","bedrooms","bathrooms",
        "initial_price","purchase_decision","investment","final_price","FinalSpending","ProfitLoss")
enova1<-enova[,!(names(enova) %in% cols)]
View(enova1)
sapply(enova1,function(x) sum(is.na(x)))

#**************************************************************************************
# Data Cleaning by imputing missing values using the MICE package
imputed_Data <- mice(enova1, m=5, maxit =5, seed = 500)
completeData <- complete(imputed_Data,2)
cols<-enova[,c("bedrooms","bathrooms")] # Adding back after mice
enova2<-cbind(completeData,cols)

#**************************************************************************************
# Univariate Data Exploration
# zone
zone_tab<-table(enova$zone)
zone_tabp<-prop.table(zone_tab)
barplot(zone_tabp)

# subtype
sub_tab<-table(enova$sub_type)
sub_tabp<-prop.table(sub_tab)
barplot(sub_tabp)

# days_on market
hist(enova$days_on_market)
summary(enova$days_on_market) # Right skewed

#area_type
area_tab<-table(enova$area_type)
area_tabp<-prop.table(area_tab)
barplot(area_tabp)

# current_population
hist(enova$current_population)
summary(enova$current_population) # asymmetric distribution

# population_5_years_ago
hist(enova$population_5_years_ago)
summary(enova$population_5_years_ago) # Right skewed

# schools_in_area
schools_tab<-table(enova$schools_in_area)
schools_tabp<-prop.table(schools_tab)
barplot(schools_tabp)  # right skewed

# public_transit_score
transit_tab<-table(enova$public_transit_score)
transit_tabp<-prop.table(transit_tab)
barplot(transit_tabp)  # right skewed
levels(enova$public_transit_score)

# crime_score
hist(enova$crime_score)
summary(enova$crime_score) # Right skewed

# culture_score
hist(enova$culture_score)
summary(enova$culture_score) # Normal dist

# inspection_type
inspection_tab<-table(enova$inspection_type)
inspection_tabp<-prop.table(inspection_tab)
barplot(inspection_tabp) # buyer: 70%, seller: 22%, foreclosure: 7%

# structural_quality_grade
structure_tab<-table(enova$structural_quality_grade)
structure_tabp<-prop.table(structure_tab)
barplot(structure_tabp)

#exterior condition grade
ext_grade_tab<-table(enova$exterior_condition_grade)
ext_grade_tabp<-prop.table(ext_grade_tab)
barplot(ext_grade_tabp)

#interior condition grade
int_grade_tab<-table(enova$interior_condition_grade)
int_grade_tabp<-prop.table(int_grade_tab)
barplot(int_grade_tabp)

#utilities grade
util_grade_tab<-table(enova$utilities_grade)
util_grade_tabp<-prop.table(util_grade_tab)
barplot(util_grade_tabp)

#damage and issue grade
damage_grade_tab<-table(enova$damage_and_issue_grade)
damage_grade_tabp<-prop.table(damage_grade_tab)
barplot(damage_grade_tabp)

#overall_inspector_score
hist(enova$overall_inspector_score)
summary(enova$overall_inspector_score) #Normal dist

#sqft
hist(enova$sqft)
summary(enova$sqft) # only till 10000 is in majority

#floors_in_building
floors_tab<-table(enova$floors_in_building)
floors_tabp<-prop.table(floors_tab)
barplot(floors_tabp) # most of them have less than 5 floors

floors_tab<-table(enova$floors_in_building)
floors_tabp<-prop.table(floors_tab)
barplot(floors_tabp)

#floors_in_unit
unit_tab<-table(enova$floors_in_unit)
unit_tabp<-prop.table(unit_tab)
barplot(unit_tabp) # Less than 4

#bedrooms
enova$bedrooms<-as.factor(enova$bedrooms)
bed_tab<-table(enova$bedrooms)
bed_tabp<-prop.table(bed_tab)
barplot(bed_tabp)

#bathrooms
enova$bathrooms<-as.factor(enova$bathrooms)
bath_tab<-table(enova$bathrooms)
bath_tabp<-prop.table(bath_tab)
barplot(bath_tabp)

#parking
parking_tab<-table(enova$parking)
parking_tabp<-prop.table(parking_tab)
barplot(parking_tabp) #70% have parking

#basement
basement_tab<-table(enova$basement)
basement_tabp<-prop.table(basement_tab)
barplot(basement_tabp) # Only 11% have basement

#AC
ac_tab<-table(enova$central_hvac)
ac_tabp<-prop.table(ac_tab)
barplot(ac_tabp)

#exterior_color
color_tab<-table(enova$exterior_color)
color_tabp<-prop.table(color_tab)
barplot(color_tabp) #grey, beige, brown and white have higher propertion

#exterior_material
material_tab<-table(enova$exterior_material)
material_tabp<-prop.table(material_tab)
barplot(material_tabp) # wood and brick are in majority

#initial_price
hist(enova$initial_price)
summary(enova$initial_price)

#initial_value
hist(enova$initial_value)
summary(enova$initial_value)

#investment
hist(enova$investment)
summary(enova$investment) # normal dist

#final price
hist(enova$final_price)
summary(enova$final_price)

#final_Spending
hist(enova$FinalSpending)
summary(enova$FinalSpending)

#**************************************************************************************
# Divide data into residential and non-residential types
enova_res<-enova2[enova2$zone=="residential",]
View(enova_res)
sum(!complete.cases(enova_res))
sapply(enova_res,function(x) sum(is.na(x)))

enova_comm<-subset(enova2,!zone=="residential")
View(enova_comm)
sapply(enova_comm,function(x) sum(is.na(x)))
sapply(enova,function(x) sum(is.na(x)))

#**************************************************************************************
# Divide Residential in train and test
set.seed(1234)		# Generate Random Number
trainIndex <- sample(1:nrow(enova_res), size = 0.7*nrow(enova_res))

# Residential Train Dataset
res.train <- enova_res[trainIndex,]
# Residential Test Dataset
res.test <- enova_res[-trainIndex,] 

# Stepwise Regression
null = glm(Profit~1, data= res.train[,-1], family = "binomial") 
full = glm(Profit~., data= res.train[,-1], family = "binomial")
step(null, scope=list(lower=null, upper=full), direction="both")

logit=glm(Profit ~ initial_value + crime_score + population_5_years_ago +current_population + 
            structural_quality_grade + bathrooms + floors_in_unit + sqft + interior_condition_grade 
          + bedrooms,data=res.train[,-1],family="binomial")
summary(logit)
# AIC: 3082.7

# function for cutoff probability: Logistic regression
opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]],
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)}

# creating data frame with probabilities and labels
mod1_data<-data.frame(predict(logit,res.test, type="response"),res.test$Profit)
colnames(mod1_data)<-c("predictions","labels")

# calculating the required values for ROC curve
pred<-prediction(mod1_data$predictions,mod1_data$labels)
perf<-performance(pred,"tpr","fpr")

# plotting curve
plot(perf,col="black",lty=3,lwd=3)

# calculating cutoff probability
print(opt.cut(perf, pred))
# cut-off probability: 0.83

pred<-predict(logit,newdata=res.test,type="response")
prediction<-data.frame(pred)
predicted<-ifelse(prediction>0.83,1,0)
predicted<-factor(predicted)
confusionMatrix(predicted,res.test$Profit,positive='1')
# Accuracy: 76%, Sensitivity: 75%, Specificity: 79%

# Applying random forest on residential properties
formula1 <- res.train$Profit~sub_type+schools_in_area+days_on_market+area_type+current_population+population_5_years_ago+
  +public_transit_score+crime_score+culture_score+inspection_type+structural_quality_grade+
  exterior_condition_grade+interior_condition_grade+utilities_grade+damage_and_issue_grade+overall_inspector_score+
  sqft+floors_in_unit+parking+basement+central_hvac+exterior_color+exterior_material+initial_value+bedrooms+bathrooms


rf.res <- randomForest(formula1,data = res.train, ntree = 301, mtry = sqrt(ncol(res.train)), 
                       proximity =TRUE, replace = TRUE, sampsize = nrow(res.train), 
                       importance = TRUE )
summary(rf.res)
print(rf.res) 
plot(rf.res)

#tuning the random forest to get best value of mtry
best.mtry <- tuneRF(res.train[,-c(1,4,20,28)], res.train[,28],stepFactor = 0.5, 
                    ntreeTry = 301, improve = 0.01)
# best.mtry
# mtry  OOBError
# 2.OOB     2 0.1771351
# 5.OOB     5 0.1624492
# 10.OOB   10 0.1556710
# 20.OOB   20 0.1549932
#using mtry = 20
rf.res <- randomForest(formula1,data = res.train, ntree = 301, mtry = 20, 
                       proximity =TRUE, replace = TRUE, sampsize = nrow(res.train), 
                       importance = TRUE )
summary(rf.res)
print(rf.res) 
plot(rf.res)
layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot(rf.res, log="y")
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rf.res$err.rate),col=1:4,cex=0.8,fill=1:4)
table(predict(rf.res), res.train$Profit, dnn = c("Actual", "Predicted"))
#         Predicted
# Actual    0    1
# 0       265  110
# 1       567 3484
resrf.pred = predict(rf.res, newdata = res.test)
table(resrf.pred, res.test$Profit,dnn = c("Actual", "Predicted"))
#          Predicted
# Actual    0    1
# 0        107  40
# 1        262 1488

importance(rf.res, type = 2)
varImpPlot(rf.res)

#current_population,population_5_years_ago,crime_score,initial_value are the 
#important predicters for profit and loss for a residential property

# Checking the Accuracy measures and Area under the curve
plot(margin(rf.res, res.test$Profit))
accuracy.meas(res.test$Profit, resrf.pred)
confusionMatrix(resrf.pred,res.test$Profit)
# Call: 
#   accuracy.meas(response = res.test$Profit, predicted = resrf.pred)
# 
# Examples are labelled as positive when predicted is greater than 0.5 
# 
# precision: 0.805
# recall: 1.000
# F: 0.446
roc.curve(res.test$Profit, resrf.pred, plotit = F)
# Area under the curve (AUC): 0.632

#**********************************************
#balancing the data to check if the result gets improved or not

#over sampling
data_balanced_over <- ovun.sample(Profit~., data = res.train, method = "over",N = 7188)$data
table(data_balanced_over$Profit)
#under sampling
data_balanced_under <- ovun.sample(Profit~., data = res.train, method = "under", N = 1664, seed = 1)$data
table(data_balanced_under$Profit)
#both
data_balanced_both <- ovun.sample(Profit~., data = res.train, method = "both", p=0.5,N=4426, seed = 1)$data
table(data_balanced_both$Profit)

# Synthetically generating data
data.rose <- ROSE(Profit~., data = res.train, seed = 1)$data

formula2 <- data_balanced_over$Profit~sub_type+schools_in_area+days_on_market+area_type+current_population+population_5_years_ago+
  +public_transit_score+crime_score+culture_score+inspection_type+structural_quality_grade+
  exterior_condition_grade+interior_condition_grade+utilities_grade+damage_and_issue_grade+overall_inspector_score+
  sqft+floors_in_unit+parking+basement+central_hvac+exterior_color+exterior_material+initial_value+bedrooms+bathrooms
rf.over <- randomForest(formula2, data = data_balanced_over, ntree = 301, mtry = 20, 
                        proximity =TRUE, replace = TRUE, sampsize = nrow(data_balanced_over), 
                        importance = TRUE )
formula3 <- data_balanced_under$Profit~sub_type+schools_in_area+days_on_market+area_type+current_population+population_5_years_ago+
  +public_transit_score+crime_score+culture_score+inspection_type+structural_quality_grade+
  exterior_condition_grade+interior_condition_grade+utilities_grade+damage_and_issue_grade+overall_inspector_score+
  sqft+floors_in_unit+parking+basement+central_hvac+exterior_color+exterior_material+initial_value+bedrooms+bathrooms

rf.under <- randomForest(formula3, data = data_balanced_under, ntree = 301, mtry = 20, 
                         proximity =TRUE, replace = TRUE, sampsize = nrow(data_balanced_under), 
                         importance = TRUE )
formula4 <- data_balanced_both$Profit~sub_type+schools_in_area+days_on_market+area_type+current_population+population_5_years_ago+
  +public_transit_score+crime_score+culture_score+inspection_type+structural_quality_grade+
  exterior_condition_grade+interior_condition_grade+utilities_grade+damage_and_issue_grade+overall_inspector_score+
  sqft+floors_in_unit+parking+basement+central_hvac+exterior_color+exterior_material+initial_value+bedrooms+bathrooms

rf.both <- randomForest(formula4, data = data_balanced_both, ntree = 301, mtry = 20, 
                        proximity =TRUE, replace = TRUE, sampsize = nrow(data_balanced_both), 
                        importance = TRUE )

#make predictions on unseen data
pred.rf.over <- predict(rf.over, newdata = res.test)
pred.rf.under <- predict(rf.under, newdata = res.test)
pred.rf.both <- predict(rf.both, newdata = res.test)

#AUC Oversampling
roc.curve(res.test$Profit, pred.rf.over)

#AUC Undersampling
roc.curve(res.test$Profit, pred.rf.under)

#AUC Both
roc.curve(res.test$Profit, pred.rf.both)

confusionMatrix(pred.rf.under, res.test$Profit)

accuracy.meas(res.test$Profit, pred.rf.under)
# Call: 
#   accuracy.meas(response = res.test$Profit, predicted = pred.rf.under)
# 
# Examples are labelled as positive when predicted is greater than 0.5 
# 
# precision: 0.805
# recall: 1.000
# F: 0.446
# Even after sampling, we are achieveing the same results. 

#**********************************************
# Rpart tree to get association rules
tree.rpart <- rpart(formula1, data = res.train, method = "class", parms = list(split = "gini"),
                    control = rpart.control(minsplit = 10), cp = -1)
rpart.plot(tree.rpart)
summary(tree.rpart)

# Variable importance
# initial_value population_5_years_ago     current_population 
# 37                     31                     16 
# crime_score              bathrooms                   sqft 
# 7                      3                      1 
# sub_type        schools_in_area        utilities_grade 
# 1                      1                      1 

# Prune the tree
opt <-which.min(tree.rpart$cptable[,"xerror"])
cp <-tree.rpart$cptable[opt,"CP"]
prune.rpart <- prune(tree.rpart, cp = cp)

rpart.plot(prune.rpart)
summary(prune.rpart)
tree.pred <- predict(prune.rpart, newdata = res.test, type = "class")
confusionMatrix(tree.pred, res.test$Profit)
#**************************************************************************************
# Divide Non-Residential in train and test
set.seed(1234)		# Generate Random Number
trainIndex2 <- sample(1:nrow(enova_comm), size = 0.7*nrow(enova_comm))

# Non-Residential Train Dataset
comm.train <- enova_comm[trainIndex2,]
# Non-Residential Test Dataset
comm.test <- enova_comm[-trainIndex2,] 


#Performing the same steps for Commercial properties
# Stepwise Regression
null = glm(Profit~1, data= comm.train[,-c(28,29)], family = "binomial") 
full = glm(Profit~., data= comm.train[,-c(28,29)], family = "binomial")
step(null, scope=list(lower=null, upper=full), direction="both")

logit1= glm(formula = Profit ~ area_type + population_5_years_ago + current_population + 
              crime_score + zone + initial_value + culture_score + parking + 
              structural_quality_grade + sqft + central_hvac, family = "binomial", 
            data = comm.train[, -c(28, 29)])
summary(logit1)
# AIC: 6178

# function for cutoff probability: Logistic regression
opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]],
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)}

# creating data frame with probabilities and labels
mod2_data<-data.frame(predict(logit1,comm.test, type="response"),comm.test$Profit)
colnames(mod2_data)<-c("predictions","labels")

# calculating the required values for ROC curve
pred<-prediction(mod2_data$predictions,mod2_data$labels)
perf<-performance(pred,"tpr","fpr")

# plotting curve
plot(perf,col="black",lty=3,lwd=3)

# calculating cutoff probability
print(opt.cut(perf, pred))
# cut-off probability: 0.79

pred<-predict(logit1,newdata=comm.test,type="response")
prediction<-data.frame(pred)
predicted<-ifelse(prediction>0.79,1,0)
predicted<-factor(predicted)
confusionMatrix(predicted,comm.test$Profit,positive='1')
# Accuracy: 76%, Sensitivity: 77%, Specificity: 70%

# Random forest to get the error estimate, variable importance and accuracy measures
formula5 <- comm.train$Profit~sub_type+schools_in_area+days_on_market+area_type+current_population+population_5_years_ago+
  +public_transit_score+crime_score+culture_score+inspection_type+structural_quality_grade+
  exterior_condition_grade+interior_condition_grade+utilities_grade+damage_and_issue_grade+overall_inspector_score+
  sqft+floors_in_unit+parking+basement+central_hvac+exterior_color+exterior_material+initial_value


rf.comm <- randomForest(formula5,data = comm.train, ntree = 301, mtry = sqrt(ncol(comm.train)), 
                       proximity =TRUE, replace = TRUE, sampsize = nrow(comm.train), 
                       importance = TRUE )
summary(rf.comm)
print(rf.comm) 
plot(rf.comm)

#tuning the random forest to get best value of mtry
best.mtry2 <- tuneRF(comm.train[,-c(1,4,20,28,29,30)], comm.train[,28],stepFactor = 0.5, 
                    ntreeTry = 301, improve = 0.01)
# best.mtry2
# mtry  OOBError
# 2.OOB     2 0.1766334
# 4.OOB     4 0.1647215
# 8.OOB     8 0.1537721
# 16.OOB   16 0.1538924

rf.comm <- randomForest(formula5,data = comm.train, ntree = 301, mtry = 8, 
                       proximity =TRUE, replace = TRUE, sampsize = nrow(res.train), 
                       importance = TRUE )
summary(rf.res)
print(rf.res) 
plot(rf.res)
layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot(rf.comm, log="y")
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rf.comm$err.rate),col=1:4,cex=0.8,fill=1:4)
table(predict(rf.comm, comm.train, type = "class"), comm.train$Profit, dnn = c("Actual","Predicted"))
#         Predicted
# Actual    0    1
# 0       1234   0
# 1       307 6770
commrf.pred = predict(rf.comm, newdata = comm.test)
table(commrf.pred, comm.test$Profit,dnn = c("Actual", "Predicted"))
#          Predicted
# Actual    0    1
# 0        180  80 
# 1        479  2824

importance(rf.comm, type = 2)
varImpPlot(rf.comm)

#crime_score,current_population,population_5_years_ago,initial_value ans schools in area
#are important predicters for profit and loss for a residential property

plot(margin(rf.comm, comm.test$Profit))

accuracy.meas(comm.test$Profit, commrf.pred, threshold = 0.5)
confusionMatrix(commrf.pred, comm.test$Profit)
# Examples are labelled as positive when predicted is greater than 0.5 
# 
# precision: 0.815
# recall: 1.000
# F: 0.449
roc.curve(comm.test$Profit, commrf.pred, plotit = F)
# Area under the curve (AUC): 0.623

# Decision tree for commercial properties
tree.rpart2 <- rpart(formula5, data = comm.train, method = "class", parms = list(split = "gini"))
rpart.plot(tree.rpart2)
summary(tree.rpart2)

# Prune the tree
opt2 <-which.min(tree.rpart2$cptable[,"xerror"])
cp2 <-tree.rpart2$cptable[opt2,"CP"]
prune.rpart2 <- prune(tree.rpart2, cp = cp2)

rpart.plot(prune.rpart2)
summary(prune.rpart2)
confusionMatrix(predict(prune.rpart2, newdata = comm.train, type = "class"), comm.train$Profit)
confusionMatrix(predict(prune.rpart2, newdata = comm.test, type = "class"), comm.test$Profit)

#**************************************************************************************
# XG Boost
install.packages("xgboost")
library(xgboost)

library(readr)
library(stringr)
library(caret)## to create dummy var
install.packages("Matrix")
library(Matrix)

## converting into numerical vector
sparse_matrix <- sparse.model.matrix(Profit ~ ., data = res.train)
train_label <- res.train[,"Profit"] 
train_matrix<-xgb.DMatrix(data = as.matrix(sparse_matrix),label=train_label)

## for test data
sparse_matrix_test <- sparse.model.matrix(Profit ~ ., data = res.test)
test_label <- res.test[,"Profit"] 
test_matrix<-xgb.DMatrix(data = as.matrix(sparse_matrix_test),label=test_label)

##Parameters
nc<-length(unique(train_label))+1
xgb_params<-list("objective"="multi:softprob","eval_metric"="mlogloss","num_class"=nc)
watchlist<-list(train=train_matrix,test=test_matrix)

# Optimization
control <- trainControl(method="repeatedcv", number = 10, repeats = 2, search = "random")
bst_model1<-train(Profit~.,data=res.train,method="xgbTree", trControl = control)
##nrounds = 971, max_depth = 7, eta = 0.06321334, gamma
##= 3.15369, colsample_bytree = 0.6123002, min_child_weight = 7 and subsample = 0.6687193
bst_model2<-xgb.train(params = list(objective="multi:softprob",eta=0.06321334,max_depth = 7,subsample = 0.6687193,
                                    colsample_bytree = 0.6123002,gamma=3.15369,"num_class"=nc),data = train_matrix,nrounds = 971, watchlist = watchlist)
error<-data.frame(bst_model2$evaluation_log)
class(error)
min(error$test_merror)

##errors
error<-data.frame(bst_model$evaluation_log)
##Feature importance
imp<-xgb.importance(colnames(train_matrix),model = bst_model2)
print(imp)

xgb.plot.importance(imp)

#**************************************************************************************
# Hypothesis 2 : Final Price vs Overall Inspector Score
sum(!complete.cases(enova$overall_inspector_score)) # There are missing values
mean(enova$overall_inspector_score, na.rm = TRUE)
#imputing the missing values with its mean
enova$overall_inspector_score[is.na(enova$overall_inspector_score)] <- round(mean(enova$overall_inspector_score, na.rm = TRUE))
Index <- sample(1:nrow(enova), size = 0.7*nrow(enova))
lm.train <- enova[Index,]
lm.test <- enova[-Index,]
lm.fit <- lm(final_price~overall_inspector_score, data = lm.train)
summary(lm.fit)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             444559.53    4759.09  93.413   <2e-16 ***
#   overall_inspector_score    103.83      84.76   1.225    0.221    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 164800 on 18195 degrees of freedom
# Multiple R-squared:  8.247e-05,	Adjusted R-squared:  2.751e-05 
# F-statistic: 1.501 on 1 and 18195 DF,  p-value: 0.2206

# The High P-value means we cannot reject our null hypothesis
options(scipen = 999)
mean((lm.train$final_price - lm.fit$residuals)^2)
mean((lm.test$final_price - predict.lm(lm.fit, lm.test))^2)

#**************************************************************************************
#**************************************************************************************
