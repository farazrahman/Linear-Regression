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
library('cowplot')

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


#creating a dataframe of numeric variables
carprice_num <- CarPrice[, c(10:14, 17,19:26)]
carprice_numeric<- data.frame(sapply(carprice_num, function(x) as.numeric(x)))


carprice_final <- cbind(carprice_fact, carprice_numeric)


str(carprice_final)
#we see that there are 11 categorical variables
#carname has 22 levels
#14 numeric variables
#carid is not taken into consideration

##Identifying the dependent or target variable- Price

################################################################

#1.3- Exploratory Data Analysis



theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
               legend.position="none")


#Analysis of all categorical variables w.r.t price
plot_grid(ggplot(carprice_final, aes(x=symboling,y = price))+ geom_bar(stat = 'identity'), 
          ggplot(carprice_final, aes(x=CarName,y = price))+ geom_bar(stat = 'identity')+theme1,
          ggplot(carprice_final, aes(x=fueltype,y = price))+ geom_bar(stat = 'identity')+theme1,
          ggplot(carprice_final, aes(x=aspiration,y = price))+ geom_bar(stat = 'identity')+theme1,
          ggplot(carprice_final, aes(x=doornumber,y = price))+ geom_bar(stat = 'identity')+theme1,
          ggplot(carprice_final, aes(x=carbody,y = price))+ geom_bar(stat = 'identity')+theme1,
          align = "h")   
#We find:
#symbolic rating of '0' has more price
#Brand name of the car does affects the price
#Sedans have high price
#standard aspiration has high price as compared to turbo

plot_grid(ggplot(carprice_final, aes(x=drivewheel,y = price))+ geom_bar(stat = 'identity')+theme1,
          ggplot(carprice_final, aes(x=enginelocation,y = price))+ geom_bar(stat = 'identity')+theme1,
          ggplot(carprice_final, aes(x=enginetype,y = price))+ geom_bar(stat = 'identity')+theme1,
          ggplot(carprice_final, aes(x=cylindernumber,y = price))+ geom_bar(stat = 'identity')+theme1,
          ggplot(carprice_final, aes(x=fuelsystem,y = price))+ geom_bar(stat = 'identity')+theme1,
          align = "h")  

#Box plot of all categorical variables w.r.t price
plot_grid(ggplot(carprice_final, aes(x=symboling,y = price))+ geom_boxplot(), 
          ggplot(carprice_final, aes(x=CarName,y = price))+ geom_boxplot()+theme1,
          ggplot(carprice_final, aes(x=fueltype,y = price))+ geom_boxplot()+theme1,
          ggplot(carprice_final, aes(x=aspiration,y = price))+ geom_boxplot()+theme1,
          ggplot(carprice_final, aes(x=doornumber,y = price))+ geom_boxplot()+theme1,
          align = "h") 
#We find:
#outliers in price in symboling rating of 0,1,2,3 
#few car brands have high values such as toyota, honda etec. despite their median price being low
#in case of porsche, the price is too low for one observation
#outliers in the price fueltype 'gas', aspiration type'std', doornumber



plot_grid(ggplot(carprice_final, aes(x=drivewheel,y = price))+ geom_boxplot()+theme1,
          ggplot(carprice_final, aes(x=enginelocation,y = price))+ geom_boxplot()+theme1,
          ggplot(carprice_final, aes(x=enginetype,y = price))+ geom_boxplot()+theme1,
          ggplot(carprice_final, aes(x=cylindernumber,y = price))+ geom_boxplot()+theme1,
          ggplot(carprice_final, aes(x=fuelsystem,y = price))+ geom_boxplot()+theme1,
          align = "h") 

#We find:
#outliers in price of drivewheel categories, enginelocation type 'four', enginetype 
#few outliers in cylindernumber 'four', fuelsystem


#Boxplots for all numeric variables

plot_grid(ggplot(carprice_final, aes(x="",y = wheelbase))+ geom_boxplot(), 
          ggplot(carprice_final, aes(x="",y = carlength))+ geom_boxplot()+theme1,
          ggplot(carprice_final, aes(x="",y = carwidth))+ geom_boxplot()+theme1,
          ggplot(carprice_final, aes(x="",y = carheight))+ geom_boxplot()+theme1,
          ggplot(carprice_final, aes(x="",y = curbweight))+ geom_boxplot()+theme1,
          align = "h") 

#We find:
#outliers in wheelbase, carlength and carwidth

plot_grid(ggplot(carprice_final, aes(x="",y = enginesize))+ geom_boxplot(), 
          ggplot(carprice_final, aes(x="",y = boreratio))+ geom_boxplot()+theme1,
          ggplot(carprice_final, aes(x="",y = stroke))+ geom_boxplot()+theme1,
          ggplot(carprice_final, aes(x="",y = compressionratio))+ geom_boxplot()+theme1,
          ggplot(carprice_final, aes(x="",y = horsepower))+ geom_boxplot()+theme1,
          align = "h") 

#We find:
#outliers in enginesize, stroke, compressionratio and horsepower 

plot_grid(ggplot(carprice_final, aes(x="",y = peakrpm))+ geom_boxplot(), 
          ggplot(carprice_final, aes(x="",y = citympg))+ geom_boxplot()+theme1,
          ggplot(carprice_final, aes(x="",y = highwaympg))+ geom_boxplot()+theme1,
          ggplot(carprice_final, aes(x="",y = price))+ geom_boxplot()+theme1,
          align = "h") 

#We find:
#few outliers in peakrpm, citympg, highwaympg

#scatterplots for all numeric variables w.r.t price

plot_grid(ggplot(carprice_final, aes(y=price,x = wheelbase))+ geom_point()+ geom_smooth(method = "lm", se = TRUE), 
          ggplot(carprice_final, aes(y=price,x = carlength))+ geom_point()+ geom_smooth(method = "lm", se = TRUE)+theme1,
          ggplot(carprice_final, aes(y=price,x = carwidth))+ geom_point()+ geom_smooth(method = "lm", se = TRUE)+theme1,
          ggplot(carprice_final, aes(y=price,x = carheight))+ geom_point()+ geom_smooth(method = "lm", se = TRUE)+theme1,
          ggplot(carprice_final, aes(y=price,x = curbweight))+ geom_point()+ geom_smooth(method = "lm", se = TRUE)+theme1,
          align = "h") 

#We find:
#price increases with increase in wheelbase, carlength, carwidth and curbweight
#increase in carheight has less impact on price, few outliers are also observed

plot_grid(ggplot(carprice_final, aes(y=price,x = enginesize))+ geom_point()+ geom_smooth(method = "lm", se = TRUE), 
          ggplot(carprice_final, aes(y=price,x = boreratio))+ geom_point()+ geom_smooth(method = "lm", se = TRUE)+theme1,
          ggplot(carprice_final, aes(y=price,x = stroke))+ geom_point()+ geom_smooth(method = "lm", se = TRUE)+theme1,
          ggplot(carprice_final, aes(y=price,x = compressionratio))+ geom_point()+ geom_smooth(method = "lm", se = TRUE)+theme1,
          ggplot(carprice_final, aes(y=price,x = horsepower))+ geom_point()+ geom_smooth(method = "lm", se = TRUE)+theme1,
          align = "h") 

#We find:
##price increases with increase in enginesize, boreratio(with few outliers), and horsepower
#stroke and compressionratio(comparatively has less impact) remain almost steady   

plot_grid(ggplot(carprice_final, aes(y=price,x = peakrpm))+ geom_point()+ geom_smooth(method = "lm", se = TRUE), 
          ggplot(carprice_final, aes(y=price,x = citympg))+ geom_point()+ geom_smooth(method = "lm", se = TRUE)+theme1,
          ggplot(carprice_final, aes(y=price,x = highwaympg))+ geom_point()+ geom_smooth(method = "lm", se = TRUE)+theme1,
          align = "h") 

#We find:
#decrease in price when citympg and highwaympg increase
#peakrpm remains almost steady with little amount of decrease in price with increase in peakrpm


# Correlation between numeric variables

corrplot(cor(carprice_final[,unlist(lapply(carprice_final, is.numeric))], use = "complete.obs"), type = "lower", method = "number")

library(GGally)
ggpairs(carprice_final[, unlist(lapply(carprice_final, is.numeric))])


############################################
########Outlier treatment#########

#i. outliers in wheel base, carlength,carwidth
fun <- function(x){
  quantiles <- quantile( x, c(.05, .95 ) )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}

carprice_final$wheelbase <- fun(carprice_final$wheelbase )
carprice_final$carlength <- fun(carprice_final$carlength)
carprice_final$carwidth <- fun(carprice_final$carwidth)
carprice_final$enginesize <- fun(carprice_final$enginesize)
carprice_final$stroke <- fun(carprice_final$stroke)
carprice_final$compressionratio <- fun(carprice_final$compressionratio)
carprice_final$horsepower <- fun(carprice_final$horsepower)
carprice_final$peakrpm <- fun(carprice_final$peakrpm)
carprice_final$citympg <- fun(carprice_final$citympg)
carprice_final$highwaympg <- fun(carprice_final$highwaympg)


ggplot(carprice_final, aes(x="",y = wheelbase))+ geom_boxplot()
ggplot(carprice_final, aes(x="",y = carlength))+ geom_boxplot()
ggplot(carprice_final, aes(x="",y = enginesize))+ geom_boxplot()
################################################################
str(carprice_final)

# creating dummy variables for factor attributes having 2 levels
levels(carprice_final$fueltype)<-c(1,0)
carprice_final$fueltype <- as.numeric(levels(carprice_final$fueltype))[carprice_final$fueltype]

levels(carprice_final$aspiration)<-c(1,0)
carprice_final$aspiration <- as.numeric(levels(carprice_final$aspiration))[carprice_final$aspiration]

levels(carprice_final$doornumber)<-c(1,0)
carprice_final$doornumber <- as.numeric(levels(carprice_final$doornumber))[carprice_final$doornumber]

levels(carprice_final$enginelocation)<-c(1,0)
carprice_final$enginelocation <- as.numeric(levels(carprice_final$enginelocation))[carprice_final$enginelocation]

str(carprice_final)

#creating dummy variables from variables having more than two levels

#symboling
dummy_1 <- data.frame(model.matrix( ~symboling, data = carprice_final))
View(dummy_1)
dummy_1 <- dummy_1[,-1]

#carname
dummy_2 <- data.frame(model.matrix( ~CarName, data = carprice_final))
View(dummy_2)
dummy_2 <- dummy_2[,-1]

#carbody
dummy_3 <- data.frame(model.matrix( ~carbody, data = carprice_final))
View(dummy_3)
dummy_3 <- dummy_3[,-1]

#drivewheel
dummy_4 <- data.frame(model.matrix( ~drivewheel, data = carprice_final))
View(dummy_4)
dummy_4 <- dummy_4[,-1]

#enginetype
dummy_5 <- data.frame(model.matrix( ~enginetype, data = carprice_final))
View(dummy_5)
dummy_5 <- dummy_5[,-1]

#cylindernumber
dummy_6 <- data.frame(model.matrix( ~cylindernumber, data = carprice_final))
View(dummy_6)
dummy_6 <- dummy_6[,-1]

#fuelsystem
dummy_7 <- data.frame(model.matrix( ~fuelsystem, data = carprice_final))
View(dummy_7)
dummy_7 <- dummy_7[,-1]

str(carprice_final[,-c(1,2,6,7,9,10,11)])

# Final dataset
# Combine the dummy variables to the main data set, after removing the original categorical  columns
carprice_final <- cbind(carprice_final[,-c(1,2,6,7,9,10,11)], dummy_1, dummy_2, dummy_3, dummy_4, dummy_5, dummy_6, dummy_7)
View(carprice_final)
str(carprice_final)#205 obs. of  69 variables

# Since, this is a very small dataset, we should not divide the dataset into training and test data set
#Instead we will use the full dataset for training the model and 
#Evaluate the predicted price w.r.t actual price to arrive at a better model

#set the seed to 100, let's run it 
set.seed(100)

#Assigning the carprice final to the train data
train <- carprice_final


########Model building
# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1) #Adjusted R-squared:  0.9472  
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


# Let's execute this model here, 
model_2 <- lm(formula = price ~ fueltype + enginelocation + wheelbase + carlength + carwidth + 
                carheight + curbweight + compressionratio + horsepower + 
                peakrpm + highwaympg + symboling0 + symboling1 + CarNamebmw + 
                CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamemazda + CarNamemercury + CarNamemitsubishi + 
                CarNameNissan + CarNameplymouth + CarNameporsche + CarNamerenault + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcv + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl, 
              data = train)
# Let us look at the summary of the model
summary(model_2) #Adjusted R-squared:  0.9528 

## Let us check for multicollinearity 
# If the VIF is above 2 and p value is high we would remove the variables if they are statistically insignificant
sort(vif(model_2))

#There are few variables such as compressionratio,curbweight,carlength,fueltype etc. have very high multicollinearity
#They have high vif but their p values are less and are significant, we will keep them as of now
#The variables with high vif and high p value are,  
#After model_2, carwidth has high multicollinearity and no significance 
#but symboling0 has highest p value amongst the insignificant variables 

#Removing symboling0 


model_3 <- lm(formula = price ~ fueltype + enginelocation + wheelbase + carlength + carwidth + 
                carheight + curbweight + compressionratio + horsepower + 
                peakrpm + highwaympg +  symboling1 + CarNamebmw + 
                CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamemazda + CarNamemercury + CarNamemitsubishi + 
                CarNameNissan + CarNameplymouth + CarNameporsche + CarNamerenault + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcv + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl, 
              data = train)
summary(model_3) #Adjusted R-squared:  0.9526 

sort(vif(model_3))

#After model_3, carwidth has highest p-value, high vif

#Removing carwidth
model_4 <- lm(formula = price ~ fueltype + enginelocation + wheelbase + carlength +  
                carheight + curbweight + compressionratio + horsepower + 
                peakrpm + highwaympg +  symboling1 + CarNamebmw + 
                CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamemazda + CarNamemercury + CarNamemitsubishi + 
                CarNameNissan + CarNameplymouth + CarNameporsche + CarNamerenault + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcv + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl, 
              data = train)
summary(model_4) #Adjusted R-squared:  0.9524 

sort(vif(model_4))


#After model_4, enginetypeohcv  has highest p-value

#Removing enginetypeohcv 
model_5 <- lm(formula = price ~ fueltype + enginelocation + wheelbase + carlength +  
                carheight + curbweight + compressionratio + horsepower + 
                peakrpm + highwaympg +  symboling1 + CarNamebmw + 
                CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamemazda + CarNamemercury + CarNamemitsubishi + 
                CarNameNissan + CarNameplymouth + CarNameporsche + CarNamerenault + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohc +  
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl, 
              data = train)
summary(model_5) #Adjusted R-squared:  0.9521 

sort(vif(model_5))


#After model_5, CarNamesubaru has highest p-value

#Removing CarNamesubaru 
model_6 <- lm(formula = price ~ fueltype + enginelocation + wheelbase + carlength +  
                carheight + curbweight + compressionratio + horsepower + 
                peakrpm + highwaympg +  symboling1 + CarNamebmw + 
                CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamemazda + CarNamemercury + CarNamemitsubishi + 
                CarNameNissan + CarNameplymouth + CarNameporsche + CarNamerenault + 
                CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohc +  
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl, 
              data = train)
summary(model_6) #Adjusted R-squared:  0.9518 

sort(vif(model_6))

#After model_6, CarNameporsche has highest p-value
#Removing CarNameporsche 

model_7 <- lm(formula = price ~ fueltype + enginelocation + wheelbase + carlength +  
                carheight + curbweight + compressionratio + horsepower + 
                peakrpm + highwaympg +  symboling1 + CarNamebmw + 
                CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamemazda + CarNamemercury + CarNamemitsubishi + 
                CarNameNissan + CarNameplymouth + CarNamerenault + 
                CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohc +  
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl, 
              data = train)
summary(model_7) #Adjusted R-squared:  0.9512 

sort(vif(model_7))


#After model_7, enginetypedohcv has highest p-value
#Removing enginetypedohcv 

model_8 <- lm(formula = price ~ fueltype + enginelocation + wheelbase + carlength +  
                carheight + curbweight + compressionratio + horsepower + 
                peakrpm + highwaympg +  symboling1 + CarNamebmw + 
                CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamemazda + CarNamemercury + CarNamemitsubishi + 
                CarNameNissan + CarNameplymouth + CarNamerenault + 
                CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypel + enginetypeohc +  
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl, 
              data = train)
summary(model_8) #Adjusted R-squared:  0.9508 #All models are significant with levels one * to three *.

sort(vif(model_8))


#After model_8, symboling1 has highest p-value with *
#Removing symboling1  

model_9 <- lm(formula = price ~ fueltype + enginelocation + wheelbase + carlength +  
                carheight + curbweight + compressionratio + horsepower + 
                peakrpm + highwaympg +   CarNamebmw + 
                CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamemazda + CarNamemercury + CarNamemitsubishi + 
                CarNameNissan + CarNameplymouth + CarNamerenault + 
                CarNametoyota + CarNamevolkswagen + CarNamevolvo + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypel + enginetypeohc +  
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl, 
              data = train)
summary(model_9) #Adjusted R-squared:   0.9498  #All models are significant with levels *** to *.
#Adjusted R-squared has decreased from 0.9508 to 0.9498



#Best fit model choose is model_8 and the reasons are below,

#After testing all the above models by stepwise eliminating the variables with high vif and/or low significance
#It was found that the model_8 would be a better fit because it has all the significant variables 
#that have p-values which are less than 0.05. Also it has a high adjusted R-squared value of 0.9508
#The subsequent model where more variables are eliminated based on low significance, 
#the adjusted R-squared decreased considerably.
#Though the previous models , were having slightly high adjusted R-squared, 
#but they were also having some variables with no significance.


# predicting the results in test dataset
Predict_1 <- predict(model_8,train[,-18])
train$predict_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(train$price,train$predict_price)
rsquared <- cor(train$price,train$predict_price)^2
rsquared

#Calculate the error between predicted price and actual price

error <- data.frame(train$price-train$predict_price)

train <- cbind(CarPrice$car_ID, train, error)

ggplot(train, aes(x = price, y = error)) + geom_point() + geom_smooth(method = "lm", se = TRUE)

ggplot(train, aes(x = CarPrice$car_ID, y = error)) + geom_point() + geom_smooth(method = "lm", se = TRUE)


#Plotting the actual and predicted price and see the overlapping

ggplot(train)+ aes(x = CarPrice$car_ID)+ geom_point(aes(y = price))+geom_line(aes(y = price))+
  geom_point(aes(y=predict_price), color = "purple") + geom_line(aes(y=predict_price), color = "purple")
