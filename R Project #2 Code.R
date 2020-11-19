###########################
##########Project #2#######
#####Model Simulations#####
######By Eduardo Garza#####
###########################


alldata<-Stats_R_DATA_Project_2_#assigned my data set to the name 'alldata'
attach(Stats_R_DATA_Project_2_)#provisional code in case I need to specify to R that I only want to pull data from this data set
library(ggplot2)#loads the `ggplot` package
attach(alldata)#provisional code in case I need to specify to R that I only want to pull data from this data set
#This incorporates a nonlinear (polynomial) transformation of the DOWJones variable in my dataset
alldata$DOWJones2<-alldata$DOWJones^2#this adds an additional column to my `alldata` which is `DOWJones` squared in case I want to implement a quadratic model later on
alldata$DOWJones3<-alldata$DOWJones^3#this adds another additional column to my `alldata` which is `DOWJones` cubed in case I want to implement a cubic model later on


####DATA PARTITIONING PROCESS###
#(START)#


#fraction of sample to be used for training (70% for training and 30% for testing)
p<-.7

#selects number of observations (rows) in the dataframe
obs_count<-dim(alldata)[1]

#number of observations to be selected for the training partition of my data set
training_size <- floor(p * obs_count)
#the floor() function rounds down to the nearest integer
training_size#allows me to check the size of my training size (should be 40 for as a result of my original data size)

#setting the seed allows me to make my partition reproducible
set.seed(1234)

#this creates a vector with the shuffled row numbers of the original dataset and will allow me to prepare to partition my data in the optimal 70:30 split
train_ind <- sample(obs_count, size = training_size)


Testing <- alldata[-train_ind, ] #creates new dataframe for my testing data, pulling data from my original dataset
Training <- alldata[train_ind, ] #creates new dataframe for my training data, pulling data from my original dataset

dim(Testing)#checks the dimensions of my newly formed testing data
dim(Training)#checks the dimensions of my newly formed training data
#noting that my original dataset had 58 observations and following along with the ideal 70:30 split, I should have 40 obs for my training and 18 obs for my testing data

#(END)#



#Preparing to plot and partition my data
plot(`GOOGL ADJ CLOSE` ~ DOWJones, alldata) #this allows me to plot my entire data sat from `alldata`
plot(`GOOGL ADJ CLOSE`~ DOWJones, Testing, col ='blue') #this plots my In-Sample testing data
plot(`GOOGL ADJ CLOSE` ~ DOWJones, Training, col ='red') #this plots my In-Sample training data
points(Testing$DOWJones, Testing$`GOOGL ADJ CLOSE`, col='blue') #this plots my Out-of-Sample testing data
points(Training$DOWJones, Training$`GOOGL ADJ CLOSE`, col='red', pch=3) #this plots my Out-of-Sample training data

#Building the FIRST MODEL out of my training data
Mod1 <- lm(`GOOGL ADJ CLOSE` ~ DOWJones, Training)
summary(Mod1)#gives the summary statistics of my 'Mod1'

#Prediction generating for my training data 
PRED_1_TRN <- predict(Mod1, Training) #generate predictions on the (in-sample) training data
View(PRED_1_TRN)#allows me to view the data in 'PRED_1_TRN'
View(Mod1$fitted.values)#so I can test the error between my prediction and testing data

#Generates predictions on my test data for initial benchmarking
PRED_1_TST <- predict(Mod1, Testing) #generate predictions on the (out-of-sample) testing data
View(PRED_1_TST)#allows me to view the data in 'PRED_1_TST'

#Computing the Root Mean Squared Error for both my training and test data
RMSE_1_TRN<-sqrt(sum((PRED_1_TRN-Training$`GOOGL ADJ CLOSE`)^2)/length(PRED_1_TRN))  #computes in-sample/training data error
RMSE_1_TST<-sqrt(sum((PRED_1_TST-Testing$`GOOGL ADJ CLOSE`)^2)/length(PRED_1_TST)) #computes out-of-sample/testing data error 

RMSE_1_TRN#this just runs my RMSE_1_TRN
RMSE_1_TST#this just runs my RMSE_1_TST

#Here I plot my model in 2-D against both my training and test data
x_grid <- seq(0,8,.1) #creats grid of my x axis values
prediction_1 <- predict(Mod1, list(DOWJones = x_grid))
plot(Training$`GOOGL ADJ CLOSE` ~ Training$DOWJones, col='blue')
lines(x_grid, prediction_1, col='yellow', lwd=3)
points(Testing$`GOOGL ADJ CLOSE` ~ Testing$DOWJones, col='red', pch=3)

#Creating my SECOND MODEL now including a second variable which is 'S and P 500'
Mod2 <- lm(`GOOGL ADJ CLOSE` ~ DOWJones + SP_500, Training) 
summary(Mod2)#gives me the summary statistics of my 'Mod2'

PRED_2_TRN <- predict(Mod2, Training) #generate predictions on the (in-sample) training data with the additon of a second variable that is 'S and P 500'
View(PRED_2_TRN)#allows me to view the data in 'PRED_2_TRN'
View(Mod2$fitted.values)#so you can test error between prediction 2 and testing data


#Here I generate predictions on my test data for benchmarking
PRED_2_TST <- predict(Mod2, Testing) #this generates predictions on my out-of-sample testing data
View(PRED_2_TST)#allows me to view the data in 'PRED_2_TST'

#Here I calculate Root Mean Squared Error for both my training and test data
RMSE_2_TRN<-sqrt(sum((PRED_2_TRN-Training$`GOOGL ADJ CLOSE`)^2)/length(PRED_2_TRN))  #computes in-sample/training data error
RMSE_2_TST<-sqrt(sum((PRED_2_TST-Testing$`GOOGL ADJ CLOSE`)^2)/length(PRED_2_TST)) #computes out-of-sample/testing data error 

#Running the code below, as mentioned in `Mod1` just allows me to run my new RMSE models and makes it easer, visualy, to compare
RMSE_2_TRN
RMSE_2_TST

#Here I plot my second model that now includes my `SP_500` variable 
x_grid <- seq(0,8,.1) #creats grid of my x axis values
prediction_2 <- predict(Mod2, list(DOWJones=x_grid, SP_500 = x_grid))
plot(Training$`GOOGL ADJ CLOSE` ~ Training$DOWJones, col='blue')
lines(x_grid, prediction_2, col='yellow', lwd=3)
points(Testing$`GOOGL ADJ CLOSE` ~ Testing$DOWJones, col='red', pch=3)


#Here I create my THRID MODEL (linear)
Mod3 <- lm(`GOOGL ADJ CLOSE` ~ DOWJones + DOWJones2,Training)
summary(Mod3) #generates summary diagnostic output
View(Mod3)

#Here I generate predictons for my training data
PRED_3_TRN<-predict(Mod3,Training)
View(PRED_3_TRN)#allows me to view the data in 'PRED_3_TRN'
View(Mod3$fitted.values)#so you can test error between prediction 3 and testing data

#Here I create test data for benchmarking for my 3rd model
PRED_3_TST <- predict(Mod3, Testing) #generate predictions on the (out-of-sample) testing data

#I compute the Root Mean Squared Error for both my training and test data
RMSE_3_TRN<-sqrt(sum((PRED_3_TRN-Training$`GOOGL ADJ CLOSE`)^2)/length(PRED_3_TRN))#this computes in-sample/training data error
RMSE_3_TST<-sqrt(sum((PRED_3_TST-Testing$`GOOGL ADJ CLOSE`)^2)/length(PRED_3_TST))#this computes out-of-sample/testing data error 

#Running both lines of code allows me to run my newly created RMSE models for `Mod3`
RMSE_3_TRN
RMSE_3_TST

#Here I plot my third  model that now includes my squared DowJones (`DOWJones2`) variable 
x_grid <- seq(0,8,.1) #creats grid of my x axis values
prediction_3 <- predict(Mod3, list(DOWJones=x_grid, DOWJones2=x_grid^2))
plot(Training$`GOOGL ADJ CLOSE` ~ Training$DOWJones, col='blue')
lines(x_grid, prediction_3, col='yellow', lwd=3)
points(Testing$`GOOGL ADJ CLOSE` ~ Testing$DOWJones, col='red', pch=3)

#Creating my FOURTH MODEL now including my `SP_500`+ `DOWJones2`variables
Mod4 <- lm(`GOOGL ADJ CLOSE` ~ DOWJones + SP_500 +DOWJones2,Training )
summary(Mod4) #generates summary diagnostic output
View(Mod4)

#Here I generate predictions for my training data
PRED_4_TRN<-predict(Mod4,Training)
View(PRED_4_TRN)#allows me to view the data in 'PRED_2_TRN'
View(Mod4$fitted.values)#so you can test error between prediction 2 and testing data

#Here I create test data for benchmarking for my 4th model
PRED_4_TST <- predict(Mod4, Testing) #generate predictions on the (out-of-sample) testing data

#Computing the Root Mean Squared Error for both my training and test data
RMSE_4_TRN<-sqrt(sum((PRED_4_TRN-Training$`GOOGL ADJ CLOSE`)^2)/length(PRED_4_TRN))  #computes in-sample/training data error
RMSE_4_TST<-sqrt(sum((PRED_4_TST-Testing$`GOOGL ADJ CLOSE`)^2)/length(PRED_4_TST)) #computes out-of-sample/testing data error 

RMSE_4_TRN
RMSE_4_TST

x_grid <- seq(0,8,.1) #creats grid of my x axis values
prediction_4 <- predict(Mod4, list(DOWJones=x_grid, SP_500 = x_grid, DOWJones2=x_grid^2))
plot(Training$`GOOGL ADJ CLOSE` ~ Training$DOWJones, col='blue')
lines(x_grid, prediction_4, col='yellow', lwd=3)
points(Testing$`GOOGL ADJ CLOSE` ~ Testing$DOWJones, col='red', pch=3)

######################################
###########Comparing my Models###########
######################################

#COMPARISON OF IN-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_TRN #MODEL WITH ONLY LINEAR TERM
RMSE_2_TRN #MODEL WITH LINEAR AND QUADRATIC TERM
RMSE_3_TRN#MODEL WITH LINEAR, QUADRATIC, AND CUBIC TERM
RMSE_4_TRN #LOGARITHMIC MODEL

#COMPARISON OF OUT-OF-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_TST #MODEL WITH ONLY LINEAR TERM
RMSE_2_TST #MODEL WITH LINEAR AND QUADRATIC TERM
RMSE_3_TST #MODEL WITH LINEAR, QUADRATIC, AND CUBIC TERM
RMSE_4_TST #LOGARITHMIC MODEL

########################################################
###PLOTTING THE REGRESSION MODELS AGAINST ONE ANOTHER###
########################################################

x_grid <- seq(0,8,.1) #CREATES GRID OF X-AXIS VALUES
plot(Training$`GOOGL ADJ CLOSE` ~ Training$DOWJones, col='blue')

lines(x_grid, prediction_1, col='yellow', lwd=3) #PLOTS Mod1
lines(x_grid, prediction_2, col='black', lwd=3) #PLOTS Mod2
lines(x_grid, prediction_3, col='blue', lwd=3) #PLOTS Mod3
lines(x_grid, prediction_4, col='red', lwd=3) #PLOTS Mod4
points(Testing$`GOOGL ADJ CLOSE` ~ Testing$DOWJones, col='red', pch=3)




##VISUALIZING THE RELATIONSHIP BY TRANSMISSION TYPE
ggplot(alldata, aes(x = DOWJones, y = `GOOGL ADJ CLOSE`, color = `Month `)) + 
  geom_point()


