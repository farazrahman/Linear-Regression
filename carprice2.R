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

### Data Preparation & Exploratory Data Analysis

# Understanding the structure of the collated file
str(telecom) #7043 obs. of 21 variables;

# tenure, MonthlyCharges, TotalCharges are continuous
# SeniorCitizen need to be changed from integer to categorical


# Barcharts for categorical features with stacked telecom information
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")



plot_grid(ggplot(telecom, aes(x=PhoneService,fill=Churn))+ geom_bar(), 
          ggplot(telecom, aes(x=Contract,fill=Churn))+ geom_bar()+bar_theme1,
          ggplot(telecom, aes(x=PaperlessBilling,fill=Churn))+ geom_bar()+bar_theme1,
          ggplot(telecom, aes(x=PaymentMethod,fill=Churn))+ geom_bar()+bar_theme1,
          ggplot(telecom, aes(x=gender,fill=Churn))+ geom_bar()+bar_theme1,
          ggplot(telecom, aes(x=factor(SeniorCitizen),fill=Churn))+ geom_bar()+bar_theme1,
          align = "h")   

plot_grid(ggplot(telecom, aes(x=Partner,fill=Churn))+ geom_bar()+bar_theme1,
          ggplot(telecom, aes(x=Dependents,fill=Churn))+ geom_bar()+bar_theme1,
          ggplot(telecom, aes(x=MultipleLines,fill=Churn))+ geom_bar()+bar_theme1,
          ggplot(telecom, aes(x=InternetService,fill=Churn))+ geom_bar()+bar_theme1,
          align = "h") 

plot_grid(ggplot(telecom, aes(x=OnlineSecurity,fill=Churn))+ geom_bar()+bar_theme1,
          ggplot(telecom, aes(x=OnlineBackup,fill=Churn))+ geom_bar()+bar_theme1,
          ggplot(telecom, aes(x=DeviceProtection,fill=Churn))+ geom_bar()+bar_theme1,
          ggplot(telecom, aes(x=TechSupport,fill=Churn))+ geom_bar()+bar_theme1,
          ggplot(telecom, aes(x=StreamingTV,fill=Churn))+ geom_bar()+bar_theme1,
          ggplot(telecom, aes(x=StreamingMovies,fill=Churn))+ geom_bar()+bar_theme1,
          align = "h") 

#reveals strong contrast for telecom wrt Contract,InternetServices, (not availing) OnlineSecurity and Techsupport and PaymentMethod,
#moderate wrt whether SeniorCitizen, having partner, OnlineBackup and DeviceProtection

# Histogram and Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(telecom, aes(tenure))+ geom_histogram(binwidth = 10),
          ggplot(telecom, aes(x="",y=tenure))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(telecom, aes(MonthlyCharges))+ geom_histogram(binwidth = 20),
          ggplot(telecom, aes(x="",y=MonthlyCharges))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(telecom, aes(TotalCharges))+ geom_histogram(),
          ggplot(telecom, aes(x="",y=TotalCharges))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

#No outliers in numeric variables

# Boxplots of numeric variables relative to telecom status
plot_grid(ggplot(telecom, aes(x=Churn,y=tenure, fill=Churn))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(telecom, aes(x=Churn,y=MonthlyCharges, fill=Churn))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(telecom, aes(x=Churn,y=TotalCharges, fill=Churn))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

# Shorter tenure sees more telecom

# Correlation between numeric variables
library(GGally)
ggpairs(telecom[, c("tenure", "MonthlyCharges", "TotalCharges")])

#As expected, tenure and TotalCharges are highly correlated (corr 0.83)

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






















#Best fit model choose is model_9 and the reasons are below,

#After testing all the above models by stepwise eliminating the variables with high vif and/or low significance
#It was found that the model_9 would be a better fit because it has all the significant variables 
#that have p-values less than 0.05. Also it has a high adjusted R-squared value of 0.9719
#The subsequent models(model_10 to model_16) where more variables are eliminated based on low significance, 
#the adjusted R-squared decreased considerably.
#Though the previous models from model_1 to model_8, were having slightly high adjusted R-squared, 
#but they were also having some variables with no significance.


# predicting the results in test dataset
Predict_1 <- predict(model_15,train[,-14])
train$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(train$price,train$test_price)
rsquared <- cor(train$price,train$test_price)^2
rsquared