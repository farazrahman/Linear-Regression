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
library(cowplot)

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


#Barcharts of all categorical variables w.r.t price
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
#outliers in symboling rating of 0,1,2,3 
#few car brands have high outliers such as toyota, audi, porsche
#outliers in fueltype 'gas', aspiration type'std', doornumber



plot_grid(ggplot(carprice_final, aes(x=drivewheel,y = price))+ geom_boxplot()+theme1,
          ggplot(carprice_final, aes(x=enginelocation,y = price))+ geom_boxplot()+theme1,
          ggplot(carprice_final, aes(x=enginetype,y = price))+ geom_boxplot()+theme1,
          ggplot(carprice_final, aes(x=cylindernumber,y = price))+ geom_boxplot()+theme1,
          ggplot(carprice_final, aes(x=fuelsystem,y = price))+ geom_boxplot()+theme1,
          align = "h") 

#We find:
#outliers in drivewheel, enginelocation type 'four', enginetype 
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
#outliers in price 
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


################################################################


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
summary(model_1) #Adjusted R-squared:  0.9516  
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
model_2 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                curbweight + enginesize + boreratio + stroke + compressionratio + 
                peakrpm + highwaympg + symboling.x3 + CarName.xbmw + CarName.xbuick + 
                CarName.xchevrolet + CarName.xdodge + CarName.xhonda + CarName.xisuzu + 
                CarName.xjaguar + CarName.xmazda + CarName.xmercury + CarName.xmitsubishi + 
                CarName.xNissan + CarName.xpeugeot + CarName.xplymouth + 
                CarName.xporsche + CarName.xrenault + CarName.xsubaru + CarName.xtoyota + 
                CarName.xvolkswagen + CarName.xvolvo + fueltype + aspiration + 
                carbody.xhardtop + carbody.xhatchback + carbody.xsedan + 
                carbody.xwagon + enginelocation + enginetype.xdohcv + enginetype.xl + 
                enginetype.xohcv + enginetype.xrotor + cylindernumber.xfive + 
                cylindernumber.xsix + fuelsystem.x2bbl + fuelsystem.xmpfi, 
              data = train)
# Let us look at the summary of the model
summary(model_2) #Adjusted R-squared:  0.9558 

## Let us check for multicollinearity 
# If the VIF is above 2 and p value is high we would remove the variables if they are statistically insignificant

sort(vif(model_2))

#There are few variables such as compressionratio,curbweight,carlength,fueltype etc. have very high multicollinearity
#They have high vif but their p values are less and are significant, we will keep them as of now
#The variables with high vif and high p value are,  
#After model_2, amongst the less significant variables, enginetype.xdohcv has highest p value
#will remove this because

#Removing enginetype.xdohcv 


model_2 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                curbweight + enginesize + boreratio + stroke + compressionratio + 
                peakrpm + highwaympg + symboling.x3 + CarName.xbmw + CarName.xbuick + 
                CarName.xchevrolet + CarName.xdodge + CarName.xhonda + CarName.xisuzu + 
                CarName.xjaguar + CarName.xmazda + CarName.xmercury + CarName.xmitsubishi + 
                CarName.xNissan + CarName.xpeugeot + CarName.xplymouth + 
                CarName.xporsche + CarName.xrenault + CarName.xsubaru + CarName.xtoyota + 
                CarName.xvolkswagen + CarName.xvolvo + fueltype + aspiration + 
                carbody.xhardtop + carbody.xhatchback + carbody.xsedan + 
                carbody.xwagon + enginelocation + enginetype.xl + 
                enginetype.xohcv + enginetype.xrotor + cylindernumber.xfive + 
                cylindernumber.xsix + fuelsystem.x2bbl + fuelsystem.xmpfi, 
              data = train)

summary(model_2) #Adjusted R-squared:  0.9556 

vif(model_2)

#After model_2, symboling.x3 has highest p-value, though the vif is more, we can remove it based on p-value

#Removing symboling.x3


model_3 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                curbweight + enginesize + boreratio + stroke + compressionratio + 
                peakrpm + highwaympg + CarName.xbmw + CarName.xbuick + 
                CarName.xchevrolet + CarName.xdodge + CarName.xhonda + CarName.xisuzu + 
                CarName.xjaguar + CarName.xmazda + CarName.xmercury + CarName.xmitsubishi + 
                CarName.xNissan + CarName.xpeugeot + CarName.xplymouth + 
                CarName.xporsche + CarName.xrenault + CarName.xsubaru + CarName.xtoyota + 
                CarName.xvolkswagen + CarName.xvolvo + fueltype + aspiration + 
                carbody.xhardtop + carbody.xhatchback + carbody.xsedan + 
                carbody.xwagon + enginelocation + enginetype.xl + 
                enginetype.xohcv + enginetype.xrotor + cylindernumber.xfive + 
                cylindernumber.xsix + fuelsystem.x2bbl + fuelsystem.xmpfi, 
              data = train)

summary(model_3) #Adjusted R-squared:  0.9556 - Remains same

vif(model_3)


#After model_3, stroke has highest p-value, we can remove it based on p-value

#Removing stroke



model_4 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                curbweight + enginesize + boreratio + compressionratio + 
                peakrpm + highwaympg + CarName.xbmw + CarName.xbuick + 
                CarName.xchevrolet + CarName.xdodge + CarName.xhonda + CarName.xisuzu + 
                CarName.xjaguar + CarName.xmazda + CarName.xmercury + CarName.xmitsubishi + 
                CarName.xNissan + CarName.xpeugeot + CarName.xplymouth + 
                CarName.xporsche + CarName.xrenault + CarName.xsubaru + CarName.xtoyota + 
                CarName.xvolkswagen + CarName.xvolvo + fueltype + aspiration + 
                carbody.xhardtop + carbody.xhatchback + carbody.xsedan + 
                carbody.xwagon + enginelocation + enginetype.xl + 
                enginetype.xohcv + enginetype.xrotor + cylindernumber.xfive + 
                cylindernumber.xsix + fuelsystem.x2bbl + fuelsystem.xmpfi, 
              data = train)

summary(model_4) #Adjusted R-squared:  0.9555 - Remains same

vif(model_4)

#After model_4, CarName.xporsche has highest p-value, we can remove it based on p-value

#Removing CarName.xporsche

model_5 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                curbweight + enginesize + boreratio + compressionratio + 
                peakrpm + highwaympg + CarName.xbmw + CarName.xbuick + 
                CarName.xchevrolet + CarName.xdodge + CarName.xhonda + CarName.xisuzu + 
                CarName.xjaguar + CarName.xmazda + CarName.xmercury + CarName.xmitsubishi + 
                CarName.xNissan + CarName.xpeugeot + CarName.xplymouth + 
                CarName.xrenault + CarName.xsubaru + CarName.xtoyota + 
                CarName.xvolkswagen + CarName.xvolvo + fueltype + aspiration + 
                carbody.xhardtop + carbody.xhatchback + carbody.xsedan + 
                carbody.xwagon + enginelocation + enginetype.xl + 
                enginetype.xohcv + enginetype.xrotor + cylindernumber.xfive + 
                cylindernumber.xsix + fuelsystem.x2bbl + fuelsystem.xmpfi, 
              data = train)

summary(model_5) #Adjusted R-squared:  0.9552 - Remains same

vif(model_5)

#After model_5, fuelsystem.xmpfi  has highest p-value, and a high vif as well we can remove it based on p-value

#Removing fuelsystem.xmpfi 

model_6 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                curbweight + enginesize + boreratio + compressionratio + 
                peakrpm + highwaympg + CarName.xbmw + CarName.xbuick + 
                CarName.xchevrolet + CarName.xdodge + CarName.xhonda + CarName.xisuzu + 
                CarName.xjaguar + CarName.xmazda + CarName.xmercury + CarName.xmitsubishi + 
                CarName.xNissan + CarName.xpeugeot + CarName.xplymouth + 
                CarName.xrenault + CarName.xsubaru + CarName.xtoyota + 
                CarName.xvolkswagen + CarName.xvolvo + fueltype + aspiration + 
                carbody.xhardtop + carbody.xhatchback + carbody.xsedan + 
                carbody.xwagon + enginelocation + enginetype.xl + 
                enginetype.xohcv + enginetype.xrotor + cylindernumber.xfive + 
                cylindernumber.xsix + fuelsystem.x2bbl, 
              data = train)

summary(model_6) #Adjusted R-squared:  0.955 - Remains same

sort(vif(model_6))

#After model_6, carbody.xsedan   has highest p-value, and a high vif as well we can remove it

#Removing carbody.xsedan  

model_7 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                curbweight + enginesize + boreratio + compressionratio + 
                peakrpm + highwaympg + CarName.xbmw + CarName.xbuick + 
                CarName.xchevrolet + CarName.xdodge + CarName.xhonda + CarName.xisuzu + 
                CarName.xjaguar + CarName.xmazda + CarName.xmercury + CarName.xmitsubishi + 
                CarName.xNissan + CarName.xpeugeot + CarName.xplymouth + 
                CarName.xrenault + CarName.xsubaru + CarName.xtoyota + 
                CarName.xvolkswagen + CarName.xvolvo + fueltype + aspiration + 
                carbody.xhardtop + carbody.xhatchback + 
                carbody.xwagon + enginelocation + enginetype.xl + 
                enginetype.xohcv + enginetype.xrotor + cylindernumber.xfive + 
                cylindernumber.xsix + fuelsystem.x2bbl, 
              data = train)

summary(model_7) #Adjusted R-squared:  0.9544 - Remains almost same

sort(vif(model_7))


#After model_7, highwaympg has a high vif, but the p-value of carbody.xwagon  is high so we will remove it

#Removing carbody.xwagon    

model_8 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                curbweight + enginesize + boreratio + compressionratio + 
                peakrpm + highwaympg + CarName.xbmw + CarName.xbuick + 
                CarName.xchevrolet + CarName.xdodge + CarName.xhonda + CarName.xisuzu + 
                CarName.xjaguar + CarName.xmazda + CarName.xmercury + CarName.xmitsubishi + 
                CarName.xNissan + CarName.xpeugeot + CarName.xplymouth + 
                CarName.xrenault + CarName.xsubaru + CarName.xtoyota + 
                CarName.xvolkswagen + CarName.xvolvo + fueltype + aspiration + 
                carbody.xhardtop + carbody.xhatchback + 
               enginelocation + enginetype.xl + 
                enginetype.xohcv + enginetype.xrotor + cylindernumber.xfive + 
                cylindernumber.xsix + fuelsystem.x2bbl, 
              data = train)

summary(model_8) #Adjusted R-squared:  0.9546 - Remains almost same

sort(vif(model_8))


#After model_8, highwaympg has a high vif, but the p-value of carbody.xhardtop  is high so we will remove it

#Removing carbody.xhardtop   

model_9 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                curbweight + enginesize + boreratio + compressionratio + 
                peakrpm + highwaympg + CarName.xbmw + CarName.xbuick + 
                CarName.xchevrolet + CarName.xdodge + CarName.xhonda + CarName.xisuzu + 
                CarName.xjaguar + CarName.xmazda + CarName.xmercury + CarName.xmitsubishi + 
                CarName.xNissan + CarName.xpeugeot + CarName.xplymouth + 
                CarName.xrenault + CarName.xsubaru + CarName.xtoyota + 
                CarName.xvolkswagen + CarName.xvolvo + fueltype + aspiration + 
                carbody.xhatchback + enginelocation + enginetype.xl + 
                enginetype.xohcv + enginetype.xrotor + cylindernumber.xfive + 
                cylindernumber.xsix + fuelsystem.x2bbl, 
              data = train)

summary(model_9) #Adjusted R-squared:  0.9547 - increases w.r.t model_8

sort(vif(model_9))


#After model_9, highwaympg has the highest p-value so we will remove it

#Removing highwaympg   

model_10 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                curbweight + enginesize + boreratio + compressionratio + 
                peakrpm + CarName.xbmw + CarName.xbuick + 
                CarName.xchevrolet + CarName.xdodge + CarName.xhonda + CarName.xisuzu + 
                CarName.xjaguar + CarName.xmazda + CarName.xmercury + CarName.xmitsubishi + 
                CarName.xNissan + CarName.xpeugeot + CarName.xplymouth + 
                CarName.xrenault + CarName.xsubaru + CarName.xtoyota + 
                CarName.xvolkswagen + CarName.xvolvo + fueltype + aspiration + 
                carbody.xhatchback + enginelocation + enginetype.xl + 
                enginetype.xohcv + enginetype.xrotor + cylindernumber.xfive + 
                cylindernumber.xsix + fuelsystem.x2bbl, 
              data = train)

summary(model_10) #Adjusted R-squared:  0.9543 - remains almost same

sort(vif(model_10))


#After model_10, aspiration has the highest p-value so we will remove it

#Removing aspiration   

model_11 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                 curbweight + enginesize + boreratio + compressionratio + 
                 peakrpm + CarName.xbmw + CarName.xbuick + 
                 CarName.xchevrolet + CarName.xdodge + CarName.xhonda + CarName.xisuzu + 
                 CarName.xjaguar + CarName.xmazda + CarName.xmercury + CarName.xmitsubishi + 
                 CarName.xNissan + CarName.xpeugeot + CarName.xplymouth + 
                 CarName.xrenault + CarName.xsubaru + CarName.xtoyota + 
                 CarName.xvolkswagen + CarName.xvolvo + fueltype +  
                 carbody.xhatchback + enginelocation + enginetype.xl + 
                 enginetype.xohcv + enginetype.xrotor + cylindernumber.xfive + 
                 cylindernumber.xsix + fuelsystem.x2bbl, 
               data = train)

summary(model_11) #Adjusted R-squared:  0.9539 - remains almost same

sort(vif(model_11))



#After model_11, CarName.xmercury has the highest p-value so we will remove it

#Removing CarName.xmercury    

model_12 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                 curbweight + enginesize + boreratio + compressionratio + 
                 peakrpm + CarName.xbmw + CarName.xbuick + 
                 CarName.xchevrolet + CarName.xdodge + CarName.xhonda + CarName.xisuzu + 
                 CarName.xjaguar + CarName.xmazda + CarName.xmitsubishi + 
                 CarName.xNissan + CarName.xpeugeot + CarName.xplymouth + 
                 CarName.xrenault + CarName.xsubaru + CarName.xtoyota + 
                 CarName.xvolkswagen + CarName.xvolvo + fueltype +  
                 carbody.xhatchback + enginelocation + enginetype.xl + 
                 enginetype.xohcv + enginetype.xrotor + cylindernumber.xfive + 
                 cylindernumber.xsix + fuelsystem.x2bbl, 
               data = train)

summary(model_12) #Adjusted R-squared:  0.9531 - remains almost same

sort(vif(model_12))

#After model_12, cylindernumber.xsix has the highest p-value so we will remove it

#Removing cylindernumber.xsix    

model_13 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                 curbweight + enginesize + boreratio + compressionratio + 
                 peakrpm + CarName.xbmw + CarName.xbuick + 
                 CarName.xchevrolet + CarName.xdodge + CarName.xhonda + CarName.xisuzu + 
                 CarName.xjaguar + CarName.xmazda + CarName.xmitsubishi + 
                 CarName.xNissan + CarName.xpeugeot + CarName.xplymouth + 
                 CarName.xrenault + CarName.xsubaru + CarName.xtoyota + 
                 CarName.xvolkswagen + CarName.xvolvo + fueltype +  
                 carbody.xhatchback + enginelocation + enginetype.xl + 
                 enginetype.xohcv + enginetype.xrotor + cylindernumber.xfive + 
                 fuelsystem.x2bbl, 
               data = train)

summary(model_13) #Adjusted R-squared:  0.9521 - remains almost same

sort(vif(model_13))

#After model_13, CarName.xjaguar has the highest p-value so we will remove it

#Removing CarName.xjaguar     

model_14 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                 curbweight + enginesize + boreratio + compressionratio + 
                 peakrpm + CarName.xbmw + CarName.xbuick + 
                 CarName.xchevrolet + CarName.xdodge + CarName.xhonda + CarName.xisuzu + 
                 CarName.xmazda + CarName.xmitsubishi + 
                 CarName.xNissan + CarName.xpeugeot + CarName.xplymouth + 
                 CarName.xrenault + CarName.xsubaru + CarName.xtoyota + 
                 CarName.xvolkswagen + CarName.xvolvo + fueltype +  
                 carbody.xhatchback + enginelocation + enginetype.xl + 
                 enginetype.xohcv + enginetype.xrotor + cylindernumber.xfive + 
                 fuelsystem.x2bbl, 
               data = train)

summary(model_14) #Adjusted R-squared:  0.9515 - remains almost same

sort(vif(model_14))


#After model_14, CarName.xvolvo  has the highest p-value so we will remove it

#Removing CarName.xvolvo     

model_15 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                 curbweight + enginesize + boreratio + compressionratio + 
                 peakrpm + CarName.xbmw + CarName.xbuick + 
                 CarName.xchevrolet + CarName.xdodge + CarName.xhonda + CarName.xisuzu + 
                 CarName.xmazda + CarName.xmitsubishi + 
                 CarName.xNissan + CarName.xpeugeot + CarName.xplymouth + 
                 CarName.xrenault + CarName.xsubaru + CarName.xtoyota + 
                 CarName.xvolkswagen + fueltype +  
                 carbody.xhatchback + enginelocation + enginetype.xl + 
                 enginetype.xohcv + enginetype.xrotor + cylindernumber.xfive + 
                 fuelsystem.x2bbl, 
               data = train)

summary(model_15) #Adjusted R-squared:  0.9507 - remains almost same

vif(model_15)


#After model_15, wheelbase has the highest p-value so we will remove it

#Removing wheelbase       

model_16 <- lm(formula = price ~ carlength + carwidth + carheight + 
                 curbweight + enginesize + boreratio + compressionratio + 
                 peakrpm + CarName.xbmw + CarName.xbuick + 
                 CarName.xchevrolet + CarName.xdodge + CarName.xhonda + CarName.xisuzu + 
                 CarName.xmazda + CarName.xmitsubishi + 
                 CarName.xNissan + CarName.xpeugeot + CarName.xplymouth + 
                 CarName.xrenault + CarName.xsubaru + CarName.xtoyota + 
                 CarName.xvolkswagen + fueltype +  
                 carbody.xhatchback + enginelocation + enginetype.xl + 
                 enginetype.xohcv + enginetype.xrotor + cylindernumber.xfive + 
                 fuelsystem.x2bbl, 
               data = train)

summary(model_16) #Adjusted R-squared:  0.9505 - remains almost same

vif(model_16)

#After model_16, fuelsystem.x2bbl has the highest p-value so we will remove it

#Removing fuelsystem.x2bbl       

model_17 <- lm(formula = price ~ carlength + carwidth + carheight + 
                 curbweight + enginesize + boreratio + compressionratio + 
                 peakrpm + CarName.xbmw + CarName.xbuick + 
                 CarName.xchevrolet + CarName.xdodge + CarName.xhonda + CarName.xisuzu + 
                 CarName.xmazda + CarName.xmitsubishi + 
                 CarName.xNissan + CarName.xpeugeot + CarName.xplymouth + 
                 CarName.xrenault + CarName.xsubaru + CarName.xtoyota + 
                 CarName.xvolkswagen + fueltype +  
                 carbody.xhatchback + enginelocation + enginetype.xl + 
                 enginetype.xohcv + enginetype.xrotor + cylindernumber.xfive,data = train)

summary(model_17) #Adjusted R-squared:  0.9495 - slight decrease

vif(model_17)



#After model_17, carheight has the highest p-value so we will remove it

#Removing carheight      

model_18 <- lm(formula = price ~ carlength + carwidth +  
                 curbweight + enginesize + boreratio + compressionratio + 
                 peakrpm + CarName.xbmw + CarName.xbuick + 
                 CarName.xchevrolet + CarName.xdodge + CarName.xhonda + CarName.xisuzu + 
                 CarName.xmazda + CarName.xmitsubishi + 
                 CarName.xNissan + CarName.xpeugeot + CarName.xplymouth + 
                 CarName.xrenault + CarName.xsubaru + CarName.xtoyota + 
                 CarName.xvolkswagen + fueltype +  
                 carbody.xhatchback + enginelocation + enginetype.xl + 
                 enginetype.xohcv + enginetype.xrotor + cylindernumber.xfive,data = train)

summary(model_18) #Adjusted R-squared:  0.9488 - slight decrease

vif(model_18)


#After model_18, CarName.xisuzu has the highest p-value so we will remove it

#Removing CarName.xisuzu      

model_19 <- lm(formula = price ~ carlength + carwidth +  
                 curbweight + enginesize + boreratio + compressionratio + 
                 peakrpm + CarName.xbmw + CarName.xbuick + 
                 CarName.xchevrolet + CarName.xdodge + CarName.xhonda + 
                 CarName.xmazda + CarName.xmitsubishi + 
                 CarName.xNissan + CarName.xpeugeot + CarName.xplymouth + 
                 CarName.xrenault + CarName.xsubaru + CarName.xtoyota + 
                 CarName.xvolkswagen + fueltype +  
                 carbody.xhatchback + enginelocation + enginetype.xl + 
                 enginetype.xohcv + enginetype.xrotor + cylindernumber.xfive,data = train)

summary(model_19) #Adjusted R-squared:  0.9478 - slight decrease

vif(model_19)

#After model_19, CarName.xchevrolet has the highest p-value so we will remove it

#Removing CarName.xchevrolet      

model_20 <- lm(formula = price ~ carlength + carwidth +  
                 curbweight + enginesize + boreratio + compressionratio + 
                 peakrpm + CarName.xbmw + CarName.xbuick + 
                  CarName.xdodge + CarName.xhonda + 
                 CarName.xmazda + CarName.xmitsubishi + 
                 CarName.xNissan + CarName.xpeugeot + CarName.xplymouth + 
                 CarName.xrenault + CarName.xsubaru + CarName.xtoyota + 
                 CarName.xvolkswagen + fueltype +  
                 carbody.xhatchback + enginelocation + enginetype.xl + 
                 enginetype.xohcv + enginetype.xrotor + cylindernumber.xfive,data = train)

summary(model_20) #Adjusted R-squared:  0.9467 - slight decrease

vif(model_20)



#After model_20, enginetype.xl has the highest p-value so we will remove it

#Removing enginetype.xl       

model_21 <- lm(formula = price ~ carlength + carwidth +  
                 curbweight + enginesize + boreratio + compressionratio + 
                 peakrpm + CarName.xbmw + CarName.xbuick + 
                 CarName.xdodge + CarName.xhonda + 
                 CarName.xmazda + CarName.xmitsubishi + 
                 CarName.xNissan + CarName.xpeugeot + CarName.xplymouth + 
                 CarName.xrenault + CarName.xsubaru + CarName.xtoyota + 
                 CarName.xvolkswagen + fueltype +  
                 carbody.xhatchback + enginelocation +  
                 enginetype.xohcv + enginetype.xrotor + cylindernumber.xfive,data = train)

summary(model_21) #Adjusted R-squared:  0.9463 - slight decrease


#After model_21, carbody.xhatchback has the highest p-value so we will remove it

#Removing carbody.xhatchback       

model_22 <- lm(formula = price ~ carlength + carwidth +  
                 curbweight + enginesize + boreratio + compressionratio + 
                 peakrpm + CarName.xbmw + CarName.xbuick + 
                 CarName.xdodge + CarName.xhonda + 
                 CarName.xmazda + CarName.xmitsubishi + 
                 CarName.xNissan + CarName.xpeugeot + CarName.xplymouth + 
                 CarName.xrenault + CarName.xsubaru + CarName.xtoyota + 
                 CarName.xvolkswagen + fueltype +  
                  enginelocation +  
                 enginetype.xohcv + enginetype.xrotor + cylindernumber.xfive,data = train)

summary(model_22) #Adjusted R-squared:  0.945 - slight decrease












summary(model_12)


#Best fit model choose is model_12 and the reasons are below,

#After testing all the above models by stepwise eliminating the variables with high vif and/or low significance
#It was found that the model_12 would be a better fit because it has all the significant variables 
#that have p-values are less than 0.05. Also it has a high adjusted R-squared value of 0.9531
#The subsequent models where more variables are eliminated based on low significance, 
#the adjusted R-squared decreased considerably.
#Though the previous models , were having slightly high adjusted R-squared, 
#but they were also having some variables with no significance.


# predicting the results in test dataset
Predict_1 <- predict(model_12,train[,-14])
train$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(train$price,train$test_price)
rsquared <- cor(train$price,train$test_price)^2
rsquared