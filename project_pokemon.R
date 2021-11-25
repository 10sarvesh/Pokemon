#selecting and importing data

pok=read.csv("https://raw.githubusercontent.com/10sarvesh/Pokemon/main/pokemon.csv",header = TRUE)  #importing the dataset into a variable pok
df_pok=data.frame(pok)  #assigning the variable to dataframe 
dim(df_pok)     #shows (no. of observations,no. of features)
View(df_pok)  
summary(df_pok)   #summary of original dataset
df_pok1=df_pok[-c(25,30,31,33,38,40)]   #dropping columns that are not required
View(df_pok1)
summary(df_pok1)   #summary of new modified dataset


#data cleaning

#checking if there are any duplicated entries

duplicated(df_pok1)          #gives us boolean values for duplicate entries
which(duplicated(df_pok1))   #returns the row number or index having duplicate

#by finding summary(dataset) we can see which features have missing values

#visualizing the missing values

library(mice)
md.pattern(df_pok1,plot=TRUE,rotate.names = TRUE)

#replacing the missing values with mean values of resp. attributes

df_pok1$height_m[is.na(df_pok1$height_m)]=mean(df_pok1$height_m,na.rm = TRUE)
df_pok1$weight_kg[is.na(df_pok1$weight_kg)]=mean(df_pok1$weight_kg,na.rm = TRUE)
df_pok1$percentage_male[is.na(df_pok1$percentage_male)]=mean(df_pok1$percentage_male,na.rm = TRUE)

#boxplot to find outliers and replacing them with (0.05,0.95) quantiles

outlier_attack=boxplot(df_pok1$attack)$out                  #plotting the diagram
outlier_attack                                              #displays outliers(values)
q_attack=quantile(df_pok1$attack,c(0.05,0.95))              #creating quantile of [0.05,0.95] C.I.
q_attack                                                    #displays the quantile limits
q_attack[1]=df_pok1$attack[df_pok1$attack<q_attack[1]]      #replacing values less than lower quantile with lower quantile
q_attack[2]=df_pok1$attack[df_pok1$attack>q_attack[2]]      #replacing values more than upper quantile with upper quantile

outlier_defence=boxplot(df_pok1$defense)$out
outlier_defence
q_defence=quantile(df_pok1$defense,c(0.05,0.95))
q_defence
q_defence[1]=df_pok1$defense[df_pok1$defense<q_defence[1]]
q_defence[2]=df_pok1$defense[df_pok1$defense>q_defence[2]]

outlier_hp=boxplot(df_pok1$hp)$out
outlier_hp
q_hp=quantile(df_pok1$hp,c(0.05,0.95))
q_hp
q_hp[1]=df_pok1$hp[df_pok1$hp<q_hp[1]]
q_hp[2]=df_pok1$hp[df_pok1$hp>q_hp[2]]

outlier_spat=boxplot(df_pok1$sp_attack)$out
outlier_spat
q_spat=quantile(df_pok1$sp_attack,c(0.05,0.95))
q_spat
q_spat[1]=df_pok1$sp_attack[df_pok1$sp_attack<q_spat[1]]
q_spat[2]=df_pok1$sp_attack[df_pok1$sp_attack>q_spat[2]]

outlier_spdf=boxplot(df_pok1$sp_defense)$out
outlier_spdf
q_spdf=quantile(df_pok1$sp_defense,c(0.05,0.95))
q_spdf
q_spdf[1]=df_pok1$sp_defense[df_pok1$sp_defense<q_spdf[1]]
q_spdf[2]=df_pok1$sp_defense[df_pok1$sp_defense>q_spdf[2]]

outlier_speed=boxplot(df_pok1$speed)$out
outlier_speed
q_speed=quantile(df_pok1$speed,c(0.05,0.95))
q_speed
q_speed[1]=df_pok1$speed[df_pok1$speed<q_speed[1]]
q_speed[2]=df_pok1$speed[df_pok1$speed>q_speed[2]]


#data transformation

#converting the column 'abilities' from categorical to numeric 

df_pok1$abilities=factor(df_pok1$abilities)       #creating a vector to store all categorical values
df_pok1$abilities=as.numeric(df_pok1$abilities)   #converting them to numeric type
View(df_pok1)

#rescaling the data in the range [0,4] using rescale function

library(scales)
df_pok1$abilities=rescale(df_pok1$abilities,to=c(0,4))   
df_pok1$attack=rescale(df_pok1$attack,to=c(0,4))
df_pok1$base_egg_steps=rescale(df_pok1$base_egg_steps,to=c(0,4))
df_pok1$base_happiness=rescale(df_pok1$base_happiness,to=c(0,4))
df_pok1$base_total=rescale(df_pok1$base_total,to=c(0,4))
df_pok1$capture_rate=rescale(df_pok1$capture_rate,to=c(0,4))
df_pok1$defense=rescale(df_pok1$defense,to=c(0,4))
df_pok1$experience_growth=rescale(df_pok1$experience_growth,to=c(0,4))
df_pok1$height_m=rescale(df_pok1$height_m,to=c(0,4))
df_pok1$hp=rescale(df_pok1$hp,to=c(0,4))
df_pok1$percentage_male=rescale(df_pok1$percentage_male,to=c(0,4))
df_pok1$sp_attack=rescale(df_pok1$sp_attack,to=c(0,4))
df_pok1$sp_defense=rescale(df_pok1$sp_defense,to=c(0,4))
df_pok1$speed=rescale(df_pok1$speed,to=c(0,4))
df_pok1$weight_kg=rescale(df_pok1$weight_kg,to=c(0,4))

View(df_pok1)

#saving the cleaned data to csv file

write.csv(df_pok1,file="D:\\BDA\\Sem 2\\Enabling Technologies\\Project\\archive\\pokemon_cleaned.csv",row.names = FALSE)


#################################################################################################################


#Here we have 2 target variables:'type1' and 'is-legendary' and all others are feature variables  

df_pok1=read.csv("D:\\BDA\\Sem 2\\Enabling Technologies\\Project\\archive\\pokemon_cleaned.csv",header=TRUE)
summary(df_pok1)
str(df_pok1)                 #gives the overall of structure of data other than statistical summary
dim(df_pok1)
unique(df_pok1$type1)        #returns all unique values of pokemon type

#creating train and test

train=df_pok1[1:535,]        #assigning the training data: 66% of entire dataset
summary(train)
test=df_pok1[536:801,]       #assigning test data: remaining 34% of dataset
summary(test)

#predicting 'type'of pokemon

test_ft1=test[-c(33,35)]     #assigning the feature variables to predict 'type'
test_ft1                     
test_tgt1=test$type1         #assigning the target variable 'type'
test_tgt1

#Naive Bayes Classifier

df_pok1$type1=as.factor(df_pok1$type1)     #encoding 'type1' into category
library(e1071)
library(caret)
model_type=naiveBayes(train$type1~.,data = train[-c(35)])   #applying Naive Bayes model on the training data
model_type
pred_type=predict(model_type,newdata = test_ft1)         #predicting the 'type' on test data
pred_type

#creating confusion matrix

cm_type=table(test_tgt1,pred_type)        #creating confusion matrix to compare target and predicted values
confusionMatrix(cm_type)



#predicting 'is legendary' or 'not'
#is_legendary:1 not_is_legendary:0

test_ft2=test[-c(35)]        #assigning the feature variables to predict 'is_legendary' or 'not'
test_ft2
test_tgt2=test$is_legendary  #assigning the target variable 'is_legendary'
test_tgt2

model_leg=naiveBayes(train$is_legendary~.,data = train)   #applying Naive Bayes model on the training data
model_leg
pred_leg=predict(model_leg,test_ft2)                          #predicting the 'is_legendary' or not on test data
pred_leg

cm_leg=table(test_tgt2,pred_leg)          #creating confusion matrix to compare target and predicted values
confusionMatrix(cm_leg)
