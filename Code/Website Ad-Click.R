# Problem statement - To perform a logistic regression analysis to arrive at a model that can be used to Predict who is likely going to click 
#                     on the advertisement once the company launch it on their website so that it can result in more revenue generation to the company.


#........................................................................................IMPORTING THE DATASET...................................................................................................................

data <- read.csv(choose.files()) # Importing the data-set in csv format
dim(data)  # Dimension of data-set which is 6657 rows and 14 columns
head(data) # First few rows of the data-set
           # Here the target variable (dependent variable) is Clicked (It is binary in terms of 0(means not clicked) and 1(means clicked))
           # We will predict clicked by using logistic regression model as per the problem statement given

#.........................................................................IDENTIFYING VARIABLE TYPE, ELIMINATION OF GARBAGE VARIABLES AND DATA SUMMARY............................................................................

str(data) # Type of each variable
          # chr - Ad_Topic, Country_Name, City_code, Male, Time_Period, Weekday and Month
          # num - VistID, Time_Spent, Age, Avg_Income, Internet_Usage, Year and Clicked  
          # By seeing at the Year attribute one can easily understand that it will not contribute anything to the model as its value is constant over the whole dataset, so we will eliminate it
data <- subset(data, select = -c(Year))  # Deleting garbage attribute which is year
char_cols <- c("Ad_Topic", "Country_Name", "City_code", "Male", "Time_Period", "Weekday", "Month") # columns which are needed to be changes into factors
for(item in char_cols)  # For loop to change each character variable into factor
{ 
  data[,c(item)] <- as.factor(data[,c(item)])
}  
str(data) # Type of each variable
          # factor - Ad_Topic, Country_Name, City_code, Male, Time_Period, Weekday and Month
          # num - VistID, Time_Spent, Age, Avg_Income, Internet_Usage, Year and Clicked  
summary(data) # General summary of the data
              # Suggests that smallest average income is 13,996.50 and the highest is 79,484.80 which basically means that site visitors are people belonging to different social classes
              # Also suggests that data is about popular websites since users spend between 32 and 91 minutes on these website in one session which like are really big numbers
              # Further suggests that average age of a visitor is 36 years. The youngest user is 19 years old and the oldest is 61 years old which clearly says that site is targeting adult users more
              # Lastly suggests that in terms of gender, site vissit is almost equal

#........................................................................................DATA PRE-PROCESSING.....................................................................................................................

                                                ################### MISSING VALUES #########################
colSums((is.na(data)))  # Checking the presence of missing values
                        # There are no missing values in dataset
# Now to check the presence of outlier we will use boxplot
# Boxplot will be created for only continuous variables as there is no point of having outliers in categorical variables or the target variable which is binary
                                                ################### OUTLIERS #########################
boxplot(data$VistID) # No outliers detected
boxplot(data$Time_Spent) # No outliers detected
boxplot(data$Age) # No outliers detected
boxplot(data$Internet_Usage) # No outliers
boxplot(data$Avg_Income) # Negative outliers detected
quantile(data$Avg_Income,seq(0,1,0.02)) # Quantile to get an idea of what range of data needs to be treated
                                        # Since there are negative outliers so we will change every data (4% of overall data) below 28210.00 to 28210.00
data$Avg_Income <- ifelse(data$Avg_Income < 28210.0,28210.0,data$Avg_Income) # Changing all the data below 28210.00 to 28210.00
boxplot(data$Avg_Income) # No outliers

#...................................................................................EDA - UNIVARIATE AND BIVARIATE ANALYSIS............................................................................................................
.
#univariate analysis
                                              ################### HISTOGRAMS #########################
colsforhist=c("VistID", "Time_Spent", "Age", "Avg_Income", "Internet_Usage") # Columns to plot histograms for
library(RColorBrewer) # Importing library to plot histograms and barplots
par(mfrow=c(3,2)) # Setting parameters
for(hist_cols in colsforhist)  # For loop to plot histograms by using hist function
{ 
  hist(data[,c(hist_cols)],main=paste('Histogram of:',hist_cols),col=brewer.pal(8,"Paired")) 
}    
# No outliers - Prefect for building logistic regression model, However some of the columns like "Avg_Income", "Internet_Usage" and "Time_Spent" are little bit skewed 
# "Internet_usage" is a bimodal distribution, It may affect the model training
# Log transformation will solve these problems
                                              ################### BARPLOTS #########################
ColsForBar =c("Ad_Topic", "Country_Name", "City_code", "Male", "Time_Period", "Weekday","Month", "Clicked") # Columns to plot barplot for
par(mfrow=c(3,3)) # Setting parameters
for(bar_cols in ColsForBar)  # For loop to plot barplots by using barplot function
{
  barplot(table(data[,c(bar_cols)]),main=paste('Barplot of :',bar_cols),col=brewer.pal(8,"Paired"))
}    
# Most of the data are  balanced except "City_code", so good for building logistic regression model
# Dummy variables for the purpose of model training cannot be created because so many categories are present in variables like "Ad_Topic" and "Country_Name"


#Bivariate analysis
# Continuous Vs Continuous --- Scatter plot and correlation matrix
# Since our dependent variable is categorical so we will not be plotting correlation matrix only between independent variable to check whether they are inter correlated or not
                                              ################### SCATTER PLOT #########################
ContinuousCols=c("VistID", "Time_Spent", "Age", "Avg_Income", "Internet_Usage") # Columns to plot scatter
plot(data[,ContinuousCols],col='blue')  # Scatter plot
                                        # As per the scatter plot no independent variable is showing any relation with any one except "Time_Spent" and "Avg_Income"
                                              ################### CORRLEATION MATRIX #########################
CorrData=cor(data[,ContinuousCols ], use = "complete.obs") # Correlation matrix
                                                           # By seeing at the correlation matrix also, it can be said that "Time_Spent" and "Avg_Income" are weakly correlated with each other with a value of ~0.6
                                                           # So by analyzing the accuracy first, one of them can be eliminated by using multi-collinearity check
# Continuous Vs Categorical --- ANOVA test
                                        ################### ANOVA TEST #########################
                                        # Null hypothesis: the target variable and continuous variables are not correlated
                                        # if p>0.05, then Null accepted and we conclude that variables are uncorrelated
                                        # if p<0.05 : there is variation in Clicked with respect to different
colsforanova <- c("VistID", "Time_Spent", "Age", "Avg_Income", "Internet_Usage") # Columns for ANOVA testing
for(anovacols in colsforanova) # For loops for ANOVA testing on each set of categorical and continuous (independent) variable
{
  anovaresult <- summary(aov(Clicked ~ data[,c(anovacols)],data=data)) # ANOVA test
  print(anovacols) # Variable
  print(anovaresult) # ANOVA test result
} 
# This test shows that variable like "VistID" is uncorrelated
# Categorical Vs Categorical --- Chi-square test
                                       ################### CHI-SQUARE TEST #########################
                                       # Null hypothesis: the target variable and continuous variables are not correlated
                                       # if p>0.05, then Null accepted and we conclude that variables are uncorrelated
                                       # if p<0.05 : there is variation in Clicked with respect to different
colsforchisquare <- c("Ad_Topic", "Country_Name", "City_code", "Male", "Time_Period", "Weekday","Month") # Columns for Chi-square testing
for(chisquarecols in colsforchisquare) # For loops for Chi-square testing on each set of categorical and categorical (dependent) variable
{
  chisquare_result <- chisq.test(data$Clicked,data[,c(chisquarecols)]) # Chi-square test
  print(chisquarecols) # Variable
  print(chisquare_result) # Chi-square test result
}  
# This test shows that variable like "WeeKday" and "Month" are uncorrelated

#............................................................................FEATURE SELECTION AND DATA CONVERSION...............................................................................................................

                                        ################### FEATURE SELECTION #########################
# AS per the analysis further features to be rejected are "VistID", "WeeKday" and "Month"
data <- subset(data, select = -c(VistID, Weekday, Month)) # Deleting unwanted columns (features)
                                        ################### DATA CONVERSION #########################
# Since there are like 30 levels in Ad_Topic column, 237 levels in Country_Name column and similarly large large no of levels in other factor columns
# so we will not create dummy variables as it will unnecessarily increase the total no of columns upto 250
# Hence we will convert the factors into numerical data by using as.numeric
fact_cols <- c("Ad_Topic", "Country_Name", "City_code", "Male", "Time_Period") # columns which are needed to be changed into numerical
for(item in fact_cols)  # For loop to change each factor into numerical element so that logistic regression model can be deployed
{ 
  data[,c(item)] <- as.numeric(data[,c(item)])
}  
str(data) # Type of each independent variable -  numeric

#.........................................DATA SPLITTING ........................................................................................................................................................................

library(caTools) # Importing library to split the data-set
library(caret) # Importing library to create dummy variables
set.seed(101) # Setting initialization parameter
split <- sample.split(data$Clicked, SplitRatio = 0.70) # SPlitting the data into 70% (training) and 30% (test) ratio
training <- subset(data, split==T) # Training data - 4660 rows
test <- subset(data, split==F) # Test data - 1997 rows

#.............................................................................MODEL BUILDING.....................................................................................................................................

logit1 <- glm(Clicked~., data=training, family='binomial') # Building logistic regression model
summary(logit1) # According to the summary of model - AIC is high (2003.1) ( More scope of improvement)
#                                                   - There are some non-significant variables like "Ad_Topic", "Country_Name", "Male" and "Time_Period"
#                                                   - SO we will delete these variables and again build our model
data <- subset(data, select = -c(Ad_Topic, Country_Name, Male, Time_Period)) # Deleting non-significant columns
split <- sample.split(data$Clicked, SplitRatio = 0.70) # Again splitting the data into 70% (training) and 30% (test) ratio
training <- subset(data, split==T) # Training data - 4660 rows
test <- subset(data, split==F) # Test data - 1997 rows
logit2 <- glm(Clicked~., data=training, family='binomial') # Building logistic regression model again
summary(logit2) # Now the model does not have any non significant variable with a better AIC of 2029 and it is good to go

#..............................................................................MULTI-COLLINEARITY CHECK..........................................................................................................................

library(car) # Importing car library to do multi-collinearity check
vif(logit2) # Check
# According to this no attribute showing a value greater than 5 so no one of them needs to be eliminated

#..............................................................................MODEL ACCURACY....................................................................................................................................

log_pred <- predict(logit2, newdata = test, type='response') # testing 
pred_thresh_value <- ifelse(log_pred>=0.5,1,0)  # Deciding threshold value and converting result into zeros and ones
cm = table(test$Clicked, pred_thresh_value) # Combined data having both predicted and actual values

# ROC curve which will tell us whether the threshold that we have chosen is good or not
library(ROCR) # Importing the library
rocprediction <- prediction(test$Clicked, pred_thresh_value) # Prediction instance
rocperformance <- performance(rocprediction,'tpr','fpr')
plot(rocperformance, col='red', print.cutoffs.at=seq(0.2, by=.3)) # Plotting ROC which suggests threshold should be 0.5

#error metrics -- Function to calculate Precision
error_metric <- function(cm)
{
  TN <- cm[1,1]
  TP <- cm[2,2]
  FP <- cm[1,2]
  FN <- cm[2,1]
  precision <- (TP)/(TP+FP)
  print(paste("Precision value of the model: ",round(precision,2)))
}

# Accuracy
library(caret) # Importing the library
confusionMatrix(cm) # Confusion matrix suggests that accuracy is 92.79% with - 
                    # Recall/Sensitivity : 0.9168       
                    # Specificity : 0.9423
                    # Balanced accuracy : 0.9296 
library(MLmetrics) # Importing the library
F1S = F1_Score(pred_thresh_value, test$Clicked) # A good F1-Score of 0.9350181
error_metric(cm) # A good precision value of 0.94

#..............................................................................IMPROVING THE MODEL FURTHER.......................................................................................................................

                                                  ################### LOG TRANSFORMATION ####################################
str(data) # By seeing at the structure of data we can see that some column like "Avg_Income" is very large in magnitude than others so we can scale them by log transformations and then can check the model again
          # Also as we have seen while plotting histogram "Avg_Income", "Internet_Usage" and "Time_Spent" are little bit skewed data, so log transformation will take care of that
# For "Avg_Income"
data$Avg_Income <- ifelse(data$Avg_Income == 0,1,data$Avg_Income) # Converting 0 to 1 before applying transformation as presence of zero can give undefined data
data$Avg_Income = log(data$Avg_Income) # Log transformation
# For "Internet_Usage" 
data$Internet_Usage <- ifelse(data$Internet_Usage == 0,1,data$Internet_Usage) # Converting 0 to 1 before applying transformation as presence of zero can give undefined data
data$Internet_Usage = log(data$Internet_Usage) # Log transformation
# For "Time_Spent"
data$Time_Spent <- ifelse(data$Time_Spent == 0,1,data$Time_Spent) # Converting 0 to 1 before applying transformation as presence of zero can give undefined data
data$Time_Spent = log(data$Time_Spent) # Log transformation


                                              ############################# TRAINING THE MODEL AGAIN ##########################
split <- sample.split(data$Clicked, SplitRatio = 0.70) # Again splitting the data into 70% (training) and 30% (test) ratio
training <- subset(data, split==T) # Training data - 4660 rows
test <- subset(data, split==F) # Test data - 1997 rows
logit3 <- glm(Clicked~., data=training, family='binomial') # Building logistic regression model
summary(logit3) # Again the model does not have any non significant variable with an AIC of 1883.1 (Improved) and it is good to go

vif(logit3) # Multi-collinearity check
# According to this no attribute showing a value greater than 5 so no one of them needs to be eliminated

log_pred <- predict(logit3, newdata = test, type='response') # testing 
pred_thresh_value <- ifelse(log_pred>=0.5,1,0)  # Deciding threshold value and converting result into zeros and ones
cm = table(test$Clicked, pred_thresh_value) # Combined data having both predicted and actual values
# Accuracy
confusionMatrix(cm) # Confusion matrix suggests that accuracy is 93.24% (Improved) with - 
                    # Recall/Sensitivity : 0.9146        
                    # Specificity : 0.9565  (Improved)
                    # Balanced Accuracy : 0.9355 (Improved)
F1S = F1_Score(pred_thresh_value, test$Clicked) # A good F1-Score of 0.9395432 (Improved)
error_metric(cm) # A good precision value of 0.96 (Improved)
