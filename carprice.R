####LINEAR REGRESSION ASSIGNMENT- Predicting price of Cars of the automobile company Geely Auto#############

# 1.0- Load libraries and data files

library('readr') # data input
library('tidyr') # data wrangling
library('dplyr') # data manipulation
library('ggplot2') # visualization
library('ggthemes') # visualization
library('corrplot') # visualization
library('lubridate') # date and time
library("purrr")# data manipulation
library('stringr')


#1.1- Reading in the data files

CarPrice <- read_csv("CarPrice_Assignment.csv")
View(CarPrice)

#Checking the summary and str of the data
str(CarPrice) ###205 observations and 26 variables

summary(CarPrice)
#Car_ID is the unique id of each observation

#Checking for duplicate data in the dataset 

sum(duplicated(CarPrice$car_ID)) #No duplicate IDs

#Checking for NA vaues in the dataset

colSums(is.na(CarPrice)) #No Missing data

##Identifying the dependent or target variable- Price
################################################################


################################################################
#1.2- DATA PREPARATION

#We will extract the car brand from car name variable

CarPrice$CarName <- as.factor(gsub("([A-Za-z]+).*", "\\1", CarPrice$CarName))
str(CarPrice$CarName)
levels(CarPrice$CarName)#There are some spelling mistakes in the car brands, we can correct them to further decrease the level 

CarPrice$CarName <- as.factor(gsub("maxda", "mazda",CarPrice$CarName ))
CarPrice$CarName <- as.factor(gsub("toyouta", "toyota",CarPrice$CarName ))
CarPrice$CarName <- as.factor(gsub("vw" , "volkswagen",CarPrice$CarName ))
CarPrice$CarName <- as.factor(gsub("vokswagen" , "volkswagen",CarPrice$CarName ))
CarPrice$CarName <- as.factor(gsub("porcshce" , "porsche",CarPrice$CarName ))
CarPrice$CarName <- as.factor(gsub("nissan" , "Nissan",CarPrice$CarName ))


# creating a dataframe of categorical features
carprice_chr <- CarPrice[, c(2:9, 15,16,18)]

# converting categorical attributes to factor
carprice_fact<- data.frame(sapply(carprice_chr, function(x) factor(x)))
str(carprice_fact) #we see that carname has 22 levels


# creating dummy variables for factor attributes
dummies<- data.frame(sapply(carprice_fact, function(x) data.frame(model.matrix(~x-1,data =carprice_fact))[,-1]))

# For variables having only two levels, 
#fueltype- Gas-1, diesel-0
#aspiration- std-0, turbo-1
#doornumber- two-1, four-0
#enginelocation- front-0, rear-1

# Final dataset
carprice_final<- cbind(CarPrice[,c(10:14,17,19:26)],dummies) 
View(carprice_final) #205 obs. of  69 variables


# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(carprice_final), 0.7*nrow(carprice_final))
# generate the train data set
train = carprice_final[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = carprice_final[-trainindices,]


########Model building
# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1) #Adjusted R-squared:  0.9691 
#######

# Now, using stepAIC where we pass our first model i.e model_1

# Lets load the library in which stepAIC function exists
install.packages("MASS")
library(MASS)
library(car)
# We have a total of 69 variables considered into the model 
 

step <- stepAIC(model_1, direction="both")
#Great, so many iterations have been done through the stepwise command. 
# now we need to know our model equation so lets write the Step command here. 

step
# stepAIC makes multiple calls while checking which variables to keep
# The last call that step makes, contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed. 
# Now store the last model equation of stepwise method into an object called model_2
# The stepAIC removed variables - carlength,boreratio,highwaympg,carheight etc.

# Let's execute this model here, 
model_2 <- lm(formula = price ~ carwidth + curbweight + enginesize + stroke + peakrpm + 
                citympg + symboling.x1 + CarName.xbmw + CarName.xbuick + 
                CarName.xdodge + CarName.xhonda + CarName.xjaguar + CarName.xmazda + 
                CarName.xmercury + CarName.xmitsubishi + CarName.xNissan + 
                CarName.xpeugeot + CarName.xplymouth + CarName.xporsche + 
                CarName.xrenault + CarName.xsaab + CarName.xsubaru + CarName.xtoyota + 
                CarName.xvolkswagen + aspiration + carbody.xhardtop + carbody.xhatchback + 
                carbody.xsedan + carbody.xwagon + drivewheel.xrwd + enginelocation + 
                enginetype.xohc + enginetype.xrotor + cylindernumber.xfive + 
                fuelsystem.x2bbl + fuelsystem.xmpfi, 
              data = train)
# Let us look at the summary of the model
summary(model_2) #Adjusted R-squared:  0.9742, an improvement from model_1


## Let us check for multicollinearity 
# If the VIF is above 2 and p value is high we would remove the variables if they are statistically insignificant

vif(model_2)

# curbweight has a VIF of 26.81 and enginesize has a VIF of 23.35, 
# but their p value is low and also they are significant ,so we will keep them as of now
# carbody.xhatchback has a VIF of 13.56 and carbody.xsedan has a VIF of 13.72,but their p value is low, 
# so we will keep them as of now
#carwidth, citympg, carbody.xwagon have high vif but their significance is also high
#We will inspect the less significant variables having high p-value and high vif such as 'fuelsystem.xmpfi'

#Removing 'fuelsystem.xmpfi'
model_3 <- lm(formula = price ~ carwidth + curbweight + enginesize + stroke + 
                peakrpm + citympg + symboling.x1 + CarName.xbmw + CarName.xbuick + 
                CarName.xdodge + CarName.xhonda + CarName.xjaguar + CarName.xmazda + 
                CarName.xmercury + CarName.xmitsubishi + CarName.xNissan + 
                CarName.xpeugeot + CarName.xplymouth + CarName.xporsche + 
                CarName.xrenault + CarName.xsaab + CarName.xsubaru + CarName.xtoyota + 
                CarName.xvolkswagen + aspiration + carbody.xhardtop + carbody.xhatchback + 
                carbody.xsedan + carbody.xwagon + drivewheel.xrwd + enginelocation + 
                enginetype.xohc + enginetype.xrotor + cylindernumber.xfive + 
                fuelsystem.x2bbl , data = train)

summary(model_3) #Adjusted R-squared:  0.9738, slightly reduced from model_2(0.9742)

# Let us look at the VIFs now
vif(model_3)

# The variables with high VIF have low p-value and high significance, 
#so we will keep them and continue deleting the variable with high vif, high p-value and less significance
#citympg has a vif of 6.23 and low significance, we will remove it and see the results

#deleting citympg
model_4 <- lm(formula = price ~ carwidth + curbweight + enginesize + stroke + 
                peakrpm + symboling.x1 + CarName.xbmw + CarName.xbuick + 
                CarName.xdodge + CarName.xhonda + CarName.xjaguar + CarName.xmazda + 
                CarName.xmercury + CarName.xmitsubishi + CarName.xNissan + 
                CarName.xpeugeot + CarName.xplymouth + CarName.xporsche + 
                CarName.xrenault + CarName.xsaab + CarName.xsubaru + CarName.xtoyota + 
                CarName.xvolkswagen + aspiration + carbody.xhardtop + carbody.xhatchback + 
                carbody.xsedan + carbody.xwagon + drivewheel.xrwd + enginelocation + 
                enginetype.xohc + enginetype.xrotor + cylindernumber.xfive + 
                fuelsystem.x2bbl , data = train)

summary(model_4)#Adjusted R-squared:   0.9738, same as model_3 but with 1 in significant variable less

vif(model_4)

# In the next model, we will remove the variable fuelsystem.x2bbl, having low significance & high vif


model_5 <- lm(formula = price ~ carwidth + curbweight + enginesize + stroke + 
                peakrpm + symboling.x1 + CarName.xbmw + CarName.xbuick + 
                CarName.xdodge + CarName.xhonda + CarName.xjaguar + CarName.xmazda + 
                CarName.xmercury + CarName.xmitsubishi + CarName.xNissan + 
                CarName.xpeugeot + CarName.xplymouth + CarName.xporsche + 
                CarName.xrenault + CarName.xsaab + CarName.xsubaru + CarName.xtoyota + 
                CarName.xvolkswagen + aspiration + carbody.xhardtop + carbody.xhatchback + 
                carbody.xsedan + carbody.xwagon + drivewheel.xrwd + enginelocation + 
                enginetype.xohc + enginetype.xrotor + cylindernumber.xfive , data = train)

summary(model_5)#Adjusted R-squared:  0.9737, almost same adjusted r-squared with 1 vaiable less

vif(model_5)

#In the next model we will remove CarName.xporsche, because it has high vif and less significance  
model_6 <- lm(formula = price ~ carwidth + curbweight + enginesize + stroke + 
                peakrpm + symboling.x1 + CarName.xbmw + CarName.xbuick + 
                CarName.xdodge + CarName.xhonda + CarName.xjaguar + CarName.xmazda + 
                CarName.xmercury + CarName.xmitsubishi + CarName.xNissan + 
                CarName.xpeugeot + CarName.xplymouth +  CarName.xrenault + CarName.xsaab + 
                CarName.xsubaru + CarName.xtoyota +  CarName.xvolkswagen + aspiration + carbody.xhardtop + 
                carbody.xhatchback + carbody.xsedan + carbody.xwagon + drivewheel.xrwd + enginelocation + 
                enginetype.xohc + enginetype.xrotor + cylindernumber.xfive , data = train)

summary(model_6)#Adjusted R-squared: 0.973 almost same adjusted r-squared with 1 vaiable less

vif(model_6)


#In the next model we will remove carbody.xhardtop due to less significance  
model_7 <- lm(formula = price ~ carwidth + curbweight + enginesize + stroke + 
                peakrpm + symboling.x1 + CarName.xbmw + CarName.xbuick + 
                CarName.xdodge + CarName.xhonda + CarName.xjaguar + CarName.xmazda + 
                CarName.xmercury + CarName.xmitsubishi + CarName.xNissan + 
                CarName.xpeugeot + CarName.xplymouth +  CarName.xrenault + CarName.xsaab + 
                CarName.xsubaru + CarName.xtoyota +  CarName.xvolkswagen + aspiration +  
                carbody.xhatchback + carbody.xsedan + carbody.xwagon + drivewheel.xrwd + enginelocation + 
                enginetype.xohc + enginetype.xrotor + cylindernumber.xfive , data = train)

summary(model_7)#Adjusted R-squared:  0.9725
vif(model_7)

#In the next model we will remove carbody.xsedan due to less significance and high vif  
model_8 <- lm(formula = price ~ carwidth + curbweight + enginesize + stroke + 
                peakrpm + symboling.x1 + CarName.xbmw + CarName.xbuick + 
                CarName.xdodge + CarName.xhonda + CarName.xjaguar + CarName.xmazda + 
                CarName.xmercury + CarName.xmitsubishi + CarName.xNissan + 
                CarName.xpeugeot + CarName.xplymouth +  CarName.xrenault + CarName.xsaab + 
                CarName.xsubaru + CarName.xtoyota +  CarName.xvolkswagen + aspiration +  
                carbody.xhatchback + carbody.xwagon + drivewheel.xrwd + enginelocation + 
                enginetype.xohc + enginetype.xrotor + cylindernumber.xfive , data = train)

summary(model_8)#Adjusted R-squared: 0.9718
vif(model_8)


#In the next model we will remove carbody.xhatchback and carbody.xwagon due to low significance

model_9 <- lm(formula = price ~ carwidth + curbweight + enginesize + stroke + 
                peakrpm + symboling.x1 + CarName.xbmw + CarName.xbuick + 
                CarName.xdodge + CarName.xhonda + CarName.xjaguar + CarName.xmazda + 
                CarName.xmercury + CarName.xmitsubishi + CarName.xNissan + 
                CarName.xpeugeot + CarName.xplymouth +  CarName.xrenault + CarName.xsaab + 
                CarName.xsubaru + CarName.xtoyota +  CarName.xvolkswagen + aspiration +  
               drivewheel.xrwd + enginelocation + 
                enginetype.xohc + enginetype.xrotor + cylindernumber.xfive , data = train)

summary(model_9)#Adjusted R-squared:  0.9719, steady adjusted R-squared with 2 more variables less
vif(model_9)



#In the next model we will remove  enginetype.xohc due to high vif and low significance
model_10 <- lm(formula = price ~ carwidth + curbweight + enginesize + stroke + 
                 peakrpm + symboling.x1 + CarName.xbmw + CarName.xbuick + 
                 CarName.xdodge + CarName.xhonda + CarName.xjaguar + CarName.xmazda + 
                 CarName.xmercury + CarName.xmitsubishi + CarName.xNissan + 
                 CarName.xpeugeot + CarName.xplymouth +  CarName.xrenault + CarName.xsaab + 
                 CarName.xsubaru + CarName.xtoyota +  CarName.xvolkswagen + aspiration +  
                 drivewheel.xrwd + enginelocation + enginetype.xrotor + cylindernumber.xfive , data = train)

summary(model_10)#Adjusted R-squared:  0.9708  


vif(model_10)

#In the next model we will remove   symboling.x1 due to low significance
model_11 <- lm(formula = price ~ carwidth + curbweight + enginesize + stroke + 
                 peakrpm + CarName.xbmw + CarName.xbuick + 
                 CarName.xdodge + CarName.xhonda + CarName.xjaguar + CarName.xmazda + 
                 CarName.xmercury + CarName.xmitsubishi + CarName.xNissan + 
                 CarName.xpeugeot + CarName.xplymouth +  CarName.xrenault + CarName.xsaab + 
                 CarName.xsubaru + CarName.xtoyota +  CarName.xvolkswagen + aspiration +  
                 drivewheel.xrwd + enginelocation + enginetype.xrotor + cylindernumber.xfive , data = train)

summary(model_11)#Adjusted R-squared:  0.9697   


vif(model_11)

#In the next model we will remove    CarName.xmercury due to low significance
model_12 <- lm(formula = price ~ carwidth + curbweight + enginesize + stroke + 
                 peakrpm + CarName.xbmw + CarName.xbuick + 
                 CarName.xdodge + CarName.xhonda + CarName.xjaguar + CarName.xmazda + 
                 CarName.xmitsubishi + CarName.xNissan + 
                 CarName.xpeugeot + CarName.xplymouth +  CarName.xrenault + CarName.xsaab + 
                 CarName.xsubaru + CarName.xtoyota +  CarName.xvolkswagen + aspiration +  
                 drivewheel.xrwd + enginelocation + enginetype.xrotor + cylindernumber.xfive , data = train)

summary(model_12)#Adjusted R-squared:  0.9691   


vif(model_12)

#In the next model we will remove     curbweight
model_13 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                 peakrpm + CarName.xbmw + CarName.xbuick + 
                 CarName.xdodge + CarName.xhonda + CarName.xjaguar + CarName.xmazda + 
                 CarName.xmitsubishi + CarName.xNissan + 
                 CarName.xpeugeot + CarName.xplymouth +  CarName.xrenault + CarName.xsaab + 
                 CarName.xsubaru + CarName.xtoyota +  CarName.xvolkswagen + aspiration +  
                 drivewheel.xrwd + enginelocation + enginetype.xrotor + cylindernumber.xfive , data = train)

summary(model_13)#Adjusted R-squared:  0.9678    


vif(model_13)

#In the next model we will remove     cylindernumber.xfive
model_14 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                 peakrpm + CarName.xbmw + CarName.xbuick + 
                 CarName.xdodge + CarName.xhonda + CarName.xjaguar + CarName.xmazda + 
                 CarName.xmitsubishi + CarName.xNissan + 
                 CarName.xpeugeot + CarName.xplymouth +  CarName.xrenault + CarName.xsaab + 
                 CarName.xsubaru + CarName.xtoyota +  CarName.xvolkswagen + aspiration +  
                 drivewheel.xrwd + enginelocation + enginetype.xrotor  , data = train)

summary(model_14)#Adjusted R-squared:  0.967   


vif(model_14)


#In the next model we will remove     CarName.xsaab
model_15 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                 peakrpm + CarName.xbmw + CarName.xbuick + 
                 CarName.xdodge + CarName.xhonda + CarName.xjaguar + CarName.xmazda + 
                 CarName.xmitsubishi + CarName.xNissan + 
                 CarName.xpeugeot + CarName.xplymouth +  CarName.xrenault +  
                 CarName.xsubaru + CarName.xtoyota +  CarName.xvolkswagen + aspiration +  
                 drivewheel.xrwd + enginelocation + enginetype.xrotor  , data = train)

summary(model_15)#Adjusted R-squared: 0.9662    


vif(model_15)

#In the next model we will remove    CarName.xpeugeot
model_16 <- lm(formula = price ~ carwidth + enginesize + stroke + 
                 peakrpm + CarName.xbmw + CarName.xbuick + 
                 CarName.xdodge + CarName.xhonda + CarName.xjaguar + CarName.xmazda + 
                 CarName.xmitsubishi + CarName.xNissan  + CarName.xplymouth +  CarName.xrenault +  
                 CarName.xsubaru + CarName.xtoyota +  CarName.xvolkswagen + aspiration +  
                 drivewheel.xrwd + enginelocation + enginetype.xrotor  , data = train)

summary(model_16)#Adjusted R-squared: 0.9655    



#Best fit model choose is model_9 and the reasons are below,

#After testing all the above models by stepwise eliminating the variables with high vif and/or low significance
#It was found that the model_9 would be a better fit because it has all the significant variables 
#that have p-values less than 0.05. Also it has a high adjusted R-squared value of 0.9719
#The subsequent models(model_10 to model_16) where more variables are eliminated based on low significance, 
#the adjusted R-squared decreased considerably.
#Though the previous models from model_1 to model_8, were having slightly high adjusted R-squared, 
#but they were also having some variables with no significance.


# predicting the results in test dataset
Predict_1 <- predict(model_16,test[,-14])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared