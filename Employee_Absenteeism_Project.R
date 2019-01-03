#remove all the objects stored
rm(list=ls())
#set current working directory
setwd("F:/Edwisor/Data Science/Project Work/Employee-Absenteeism-master")
#Check workin directory path
getwd()
library(readxl)
library(dplyr)
library(corrplot)
library(ggplot2)
library(tidyverse)
library(randomForest)
library(data.table)
library(rpart)
## Read the data
absent_data <- read_excel("Absenteeism_at_work_Project.xls", sheet = 1)
## Changing the continous variables to categorical variables 
##for getting ease performance of data
absent_data$Reason.for.absence = as.factor(absent_data$Reason.for.absence)
absent_data$Month.of.absence = as.factor(absent_data$Month.of.absence)
absent_data$Day.of.the.week = as.factor(absent_data$Day.of.the.week)
absent_data$Seasons = as.factor(absent_data$Seasons)
absent_data$Service.time = as.factor(absent_data$Service.time)
absent_data$Hit.target = as.factor(absent_data$Hit.target)
absent_data$Disciplinary.failure = as.factor(absent_data$Disciplinary.failure)
absent_data$Education = as.factor(absent_data$Education)
absent_data$Son = as.factor(absent_data$Son)
absent_data$Social.drinker = as.factor(absent_data$Social.drinker)
absent_data$Social.smoker = as.factor(absent_data$Social.smoker)
absent_data$Pet = as.factor(absent_data$Pet)
absent_data$Work.load.Average.day  = as.numeric(absent_data$Work.load.Average.day)
## Outlier analysis
outlierKD <- function (dt, var) {
  var_name <- eval(substitute(var), eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean (var_name, na.rm = T)
  par (mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot (var_name, main="With outliers")
  hist (var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}


outlierKD(absent_data,Absenteeism.time.in.hours)# outliers detected and replaced by NA
outlierKD(absent_data,Transportation.expense) #no outliers
outlierKD(absent_data,Distance.from.Residence.to.Work) #no outliers
outlierKD(absent_data,Service.time) #no outliers
outlierKD(absent_data,Age) #no outliers
outlierKD(absent_data,Work.load.Average.day.) # 1 found and replaced with NA
outlierKD(absent_data,Hit.target) # 1 found and replaced with NA
outlierKD(absent_data,Son) # no outliers
outlierKD(absent_data,Pet) # no outliers
outlierKD(absent_data,Weight) # no outliers
outlierKD(absent_data,Height) # no outliers
outlierKD(absent_data,Body.mass.index) #no outliers


################################## Missing value analysis ##################################

missing_val = data.frame(apply(absent_data,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(absent_data)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
write.csv(missing_val, "Miising_perc.csv", row.names = F)

#ggplot analysis
ggplot(data = missing_val[1:3,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
  geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
  ggtitle("Missing data percentage (Train)") + theme_bw()

library(ggplot2)
#actual value =30
#absent_data[1,20]
#absent_data[1,20]= NA
# kNN Imputation=29.84314
#after various calculations, it is found that knn imputation method suits the best for the data. hence here we are applying knn imputation

################################## feature selection ##################################
library(corrgram)
## Correlation Plot - to check multicolinearity between continous variables
corrgram(absent_data[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
absent_data$Absenteeism.time.in.hours = as.factor(absent_data$Absenteeism.time.in.hours)

## Chi-squared Test of Independence-to check the multicolinearity between categorical variables
factor_index = sapply(absent_data,is.factor)
factor_data = absent_data[,factor_index]

for (i in 1:12)
{
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$Absenteeism.time.in.hours,factor_data[,i])))
}

absent_data$Absenteeism.time.in.hours = as.numeric(absent_data$Absenteeism.time.in.hours)

################################### Feature reduction ##################################
## Dimension Reduction
absent_data = subset(absent_data, 
                     select = -c(Weight,Hit.target,Education,Social.smoker,Pet))
#Feature Scaling
#Normality check
qqnorm(absent_data$Absenteeism.time.in.hours )
hist(absent_data$Absenteeism.time.in.hours )
str(absent_data)
#Normalisation
cnames = c("ID","Transportation.expense","Distance.from.Residence.to.Work","Height","Age","Work.load.Average.day","Body.mass.index",
           "Absenteeism.time.in.hours")

for(i in cnames){
  print(i)
  absent_data[,i] = (absent_data[,i] - min(absent_data[,i]))/
    (max(absent_data[,i] - min(absent_data[,i])))
}
########################## Univariate Distribution and Analysis ###############################

# function for univariate analysis for continous variables
#       function inpus:
#       1. dataset - input dataset
#       2. variable - variable for univariate analysis
#       3. variableName - variable title in string 
#       example.   univariate_analysis(absent_data,Absenteeism.time.in.hours,
#                                                   "Absenteeism.time.in.hours")
univariate_analysis <- function(dataset, variable,variableName){
  
  var_name = eval(substitute(variable), eval(dataset))
  
  if(is.numeric(var_name)){
    
    print(summary(var_name))
    ggplot(absent_data, aes(var_name)) + 
      geom_histogram(aes(y=..density..,binwidth=.5,colour="black", fill="white"))+ 
      geom_density(alpha=.2, fill="#FF6666")+
      labs(x = variableName, y = "count") + 
      ggtitle(paste("count of ",variableName)) +
      theme(legend.position = "null")
    
  }else{
    print("This is categorical variable.")
    
  }
  
}


# function for univariate analysis for categorical variables
#       function inpus:
#       1. dataset - input dataset
#       2. variable - variable for univariate analysis
#       3. variableName - variable title in string 
#       example.   univariate_analysis(absent_data,ID,
#                                                   "ID")
univariate_catogrical <- function(dataset,variable, variableName){
  variable <- enquo(variable)
  
  percentage <- dataset %>%
    select(!!variable) %>%
    group_by(!!variable) %>%
    summarise(n = n()) %>%
    mutate(percantage = (n / sum(n)) * 100)
  print(percentage)
  
  dataset %>%
    count(!!variable) %>%
    ggplot(mapping = aes_(x = rlang::quo_expr(variable), 
                          y = quote(n), fill = rlang::quo_expr(variable))) +
    geom_bar(stat = 'identity',
             colour = 'white') +
    labs(x = variableName, y = "count") + 
    ggtitle(paste("count of ",variableName)) +
    theme(legend.position = "bottom") -> p
  plot(p)
  
}
## Sampling
##Systematic sampling
#Function to generate Kth index
sys.sample = function(N,n)
{
  k = ceiling(N/n)
  r = sample(1:k, 1)
  sys.samp = seq(r, r + k*(n-1), k)
}
lis = sys.sample(740, 300) #select the repective rows
# #Create index variable in the data
absent_data$index = 1:740
# #Extract subset from whole data
systematic_data = absent_data[which(absent_data$index %in% lis),]

################################### Model Development ##################################
#Clean the environment
library(DataCombine)

rmExcept("absent_data")
#Divide data into train and test using stratified sampling method
set.seed(1234)
absent_data$description = NULL
library(caret)
train.index = createDataPartition(absent_data$Absenteeism.time.in.hours, p = .80, list = FALSE)
train = absent_data[ train.index,]
test  = absent_data[-train.index,]

#load libraries
library(rpart)
#decision tree analysis
#rpart for regression
fit = rpart(Absenteeism.time.in.hours ~ ., data = train, method = "anova")
#Predict for new test cases
predictions_DT = predict(fit, test[,-16])
#MAPE
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))*100
}

MAPE(test[,16], predictions_DT)

#Random Forest
RF_model = randomForest(Absenteeism.time.in.hours ~ ., train, importance = TRUE, ntree = 1000)
#Extract rules
exec = extractRules(treeList, train[,-16])  # R-executable conditions
ruleExec <- extractRules(treeList,train[,-16],digits=4)

#Make rules more readable:
readableRules = presentRules(exec, colnames(train))
readableRules[1:2,]
#Get rule metrics
ruleMetric = getRuleMetric(exec, train[,-16], train$Absenteeism.time.in.hours)  # get rule metrics
#Predict test data using random forest model
RF_Predictions = predict(RF_model, test[,-16])
#rmse calculation
#install.packages("Metrics")
library(Metrics)
rmse(test$Absenteeism.time.in.hours, RF_Predictions)
#rmse value for random forest is 0.2065729
rmse(test$Absenteeism.time.in.hours, predictions_DT)
#rmse value for decision tree is 0.222542