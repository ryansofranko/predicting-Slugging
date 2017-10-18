# A pre-defined value is used to reset the random seed so that results are repeatable.
set.seed(42) 

#Load the data
data <- read.csv(file="E:/DataAnalytics/Phillies/slugging.csv", header=T, sep=",")

#Observe data head
head(data)

#Rename Columns for cleaner code
names(data) <- c("PlayerName", "PA", "AB", "AVG", "SLG", "BB", "K.Perc", "BABIP", "GB.FB", "LD.Perc", "GB.Perc", "FB.Perc", "HR.FB", "FullSeason.SLG")
colnames(data)

#Observe data structure
str(data)

#Remove % symbol from fields
data$BB <- gsub("%","",data$BB)
data$K.Perc <- gsub("%","",data$K.Perc)
data$LD.Perc <- gsub("%","",data$LD.Perc)
data$GB.Perc <- gsub("%","",data$GB.Perc)
data$FB.Perc <- gsub("%","",data$FB.Perc)
data$HR.FB <- gsub("%","",data$HR.FB)

#convert factor to numeric 
data$BB <- as.numeric(data$BB)
data$K.Perc <- as.numeric(data$K.Perc)
data$LD.Perc <- as.numeric(data$LD.Perc)
data$GB.Perc <- as.numeric(data$GB.Perc)
data$FB.Perc <- as.numeric(data$FB.Perc)
data$HR.FB <- as.numeric(data$HR.FB)

#Divide by 100 to transform to decimal
data$BB <- data$BB/100
data$K.Perc <- data$K.Perc/100
data$LD.Perc <- data$LD.Perc/100
data$GB.Perc <- data$GB.Perc/100
data$FB.Perc <- data$FB.Perc/100
data$HR.FB <- data$HR.FB/100

#Confirm successful transformation
str(data)

#Data summary, no missing data 
summary(data)

#Split the data for cross-validation
smp_size <- floor(0.75 * nrow(data))
set.seed(42)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]

#Correlation and Exploration
library(corrplot)
coor.frame <- cor(train[ , -c(1)])
coor.frame
corrplot(coor.frame, method = "color")

#Linear Regression on train set 
SLG_Reg_train <- lm(SLG ~ HR.FB + AVG + BABIP + GB.FB + GB.Perc + FB.Perc, data = train)
summary(SLG_Reg_train)

#Test for multicollinearity
library(car)
vif(SLG_Reg_train)
sqrt(vif(SLG_Reg_train)) >2

#Linear Regression on train set removing multicollinear predictors
SLG_Reg_train <- lm(SLG ~ HR.FB + AVG + BABIP + GB.FB, data = train)
summary(SLG_Reg_train)

#Test regression on test set 
SLG_Reg_test <- lm(SLG ~ HR.FB + AVG + BABIP + GB.FB, data = test)
summary(SLG_Reg_test) 

#Applying the model to the entire dataset 
SLG_Reg_Model <- lm(SLG ~ HR.FB + AVG + BABIP + GB.FB, data = data)
summary(SLG_Reg_Model)

#Create PredictedSeason.SLG using regression model
new.data <- data
new.data$PredictedSeason.SLG <- predict(SLG_Reg_Model, newdata = new.data)
head(new.data)

#Regression model for hold out actual values
set.seed(42)
SLG_Reg_Holdout <- lm(FullSeason.SLG ~ HR.FB + AVG + BABIP + GB.FB, data = data)
summary(SLG_Reg_Holdout)

#Plot Predicted vs. Actual SLG
require(lattice)
xyplot(new.data$FullSeason.SLG ~ new.data$PredictedSeason.SLG, data = new.data, type = c("p","r"), col.line = "red", xlab="Predicted SLG", ylab = "Actual SLG", label="Predicted vs. Actual SLG")
