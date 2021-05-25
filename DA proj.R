#loading the library tidyverse
library(tidyverse)
install.packages("ggfortify")
library(caret)
library(broom)
library(ggfortify)
#reading the property sales dataset from file view.csv...
pr_data <- read.csv("view.csv") 
# checking the dataset...
View(pr_data)
head(pr_data)
str(pr_data)
summary(pr_data)

#lets first check all the numerical variables.
int_data <- select(pr_data,c(LotArea,OverallCond,OverallQual,YearBuilt,
                             GrLivArea,FullBath,HalfBath,BedroomAbvGr,
                             KitchenAbvGr,GarageArea,SalePrice))
cor_int_data <- cor(int_data)
cor_int_data
ggcorrplot(cor_int_data, hc.order = TRUE, type = "lower",
           outline.col = "white",lab = TRUE, title = "Correlational Plot")


#putting the dataset into another variable for experimenting...
pr_data1 <- pr_data
str(pr_data1)
#converting fireplace & central air "Y"and "N" to "1" & "0"...
pr_data1 <- pr_data %>% 
  mutate(Fireplace = if_else(Fireplace == "Y",1,0),
         CentralAir = if_else(CentralAir == "Y",1L,0L))

#EDA...

g <- ggplot(pr_data1)
#plot b/w SalePrice and LotArea...it looks like most of the houses has lot size 50,000 sqft.
g + geom_point(aes(SalePrice,LotArea))+
  labs(y="Sale Price", x="Lot Area") + 
  ggtitle("SalePrice vs LotArea") #and most people bought the houses with 
#LotArea below 50,000

#Histogram displaying House Count vs Year Built...
g+geom_histogram(aes(YearBuilt),color="darkblue", fill="lightblue")+
  ggtitle("Histogram  : House Count v/s Year Built")

# Sale Price v/s Building type...
g + geom_boxplot(aes(BldgType,SalePrice,color = BldgType))+
  ggtitle("Box Plot: Building Type v/s Sale Price")

# Sale Price with HouseStyle...
g + geom_bar(aes(HouseStyle)) #shows the Housestyle and count of the houses.  
table(pr_data1$HouseStyle)# count in numerical figures
g + geom_bar(aes(HouseStyle,SalePrice),stat = "identity") #displays the sale price

#sale price with the overall quality...

g + stat_count(aes(OverallQual),fill = "#56B4E9") + 
  ggtitle("Bar Plot: Count v/s Overall Quality")
#Houses with the Overall Quality 5 have been sold the most.
g + geom_col(aes(OverallQual,SalePrice),fill = "#56B4E9") +
  ggtitle("Bar Plot: Overall Quality v/s Sale Price")
#Houses with Overall Quality 7 got the highest total sale price.

#Sale Price Vs Overall Condition
g + geom_col(aes(OverallCond,SalePrice),fill = "#E69F00")+
  ggtitle("Bar Plot: Sale Price v/s Overall Condition")

# sales price and above grade living area square feet...
g+ geom_point(aes(SalePrice,GrLivArea),shape = 1,color = "#56B4E9")+ 
  ggtitle("ScatterPlot : Sale Price v/s GrLivArea")

# sale price and no. of bedrooms above grade
g + geom_point(aes(BedroomAbvGr,SalePrice),shape = 1, color = "brown")+
  ggtitle("ScatterPlot: BedroomAbvGr v/s Sale Price")

#Sale Price vs Garage Area
g + geom_point(aes(GarageArea,SalePrice))

# sale price and kitchen above grade...
g + geom_point(aes( KitchenAbvGr,SalePrice),shape = 1,color = "Chocolate4") +
  ggtitle("ScatterPlot: SalePrice v/s KitchenAbvGr")

#sale price vs sale condition:
g+ geom_col(aes(SaleCondition,SalePrice))
g + geom_bar(aes(SaleCondition))
g + boxplot(aes(SaleCondition,SalePrice))

#===========================================================================================
# to ensure the partition remains constant
set.seed(234) 
# splitting the data into 80% for training and 20% for testing
dataSplit <- sort(sample(nrow(pr_data1), nrow(pr_data1)*0.8)) 
training_dataSet <- pr_data1[dataSplit,]
testing_dataSet <- pr_data1[-dataSplit,]

# model 1 - including all variables
model_reg1 <- lm(SalePrice ~ MSZoning + CentralAir + OverallQual + GrLivArea + HouseStyle + KitchenQual + YearBuilt + 
                   BldgType + LotArea + OverallCond + GarageArea + SaleCondition + BedroomAbvGr + 
                   Fireplace + KitchenAbvGr + KitchenAbvGr + HalfBath + FullBath, data = training_dataSet)
summary(model_reg1)
RSS_model1 <- sum(model_reg1$residuals^2)
RSS_model1
# model 2 - after removing insignificant variables
model_reg2 <- lm(SalePrice ~ MSZoning + CentralAir + OverallQual + GrLivArea + HouseStyle + KitchenQual + YearBuilt + 
                   BldgType + LotArea + OverallCond + GarageArea + SaleCondition + BedroomAbvGr + 
                   Fireplace + KitchenAbvGr + KitchenAbvGr + HalfBath, data = training_dataSet)
summary(model_reg2)
RSS_model2 <- sum(model_reg2$residuals^2)
RSS_model2

model.diag.metrics <- augment(model_reg2)
head(model.diag.metrics)

par(mfrow = c(2, 2))
autoplot(model_reg2,1)
autoplot(model_reg2,2)
autoplot(model_reg2,3)
autoplot(model_reg2,4)


# model 3 - after data transformation of significant variables
# log-transform for size variables and sqrt-transform for count variables
model_reg3 <- lm(log(SalePrice) ~  OverallQual + sqrt(GrLivArea) + HouseStyle + KitchenQual + YearBuilt + 
                   BldgType + sqrt(LotArea) + OverallCond + sqrt(GarageArea) + SaleCondition + sqrt(BedroomAbvGr) + 
                   Fireplace + sqrt(KitchenAbvGr), data = training_dataSet)
summary(model_reg3)
RSS_model3 <- sum(model_reg3$residuals^2)
RSS_model3
# predicting test data from the model 3
predictions <- predict(model_reg3, testing_dataSet)
predictions

# the 3 criterions for checking the tested value accuracy
rootMeanSq <- RMSE(predictions, testing_dataSet$SalePrice)
rootMeanSq

RSS_predModel <- sum((predictions-testing_dataSet$SalePrice)^2)
RSS_predModel

RSq_pred <- R2(predictions, testing_dataSet$SalePrice)
RSq_pred

