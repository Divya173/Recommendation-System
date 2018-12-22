#loading necessary library & packages
library(ggplot2)
library(dplyr)
library(data.table)
install.packages("recommenderlab")
library(recommenderlab)


#Setting working directory-

setwd("C:/Users/divyanair/Downloads")

# Importing the dataset
beer <- read.csv("beer_data.csv")
#475984 observation with 3 variables

summary(beer)

str(beer)


#------------------------------------------------------------------------------------
# *************DATA UNDERSTANDING , DATA PREPARATION & CLEANING*******************
#------------------------------------------------------------------------------------





#converting case of the username
beer$review_profilename <- as.factor(tolower(beer$review_profilename))

#checking NA's
na_check <- sapply(beer,function(x) sum(is.na(x)))
na_check #There are no NA's

#Unique ratings
sort(unique(beer$review_overall))
#[1] 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0


#There are ratings of 0, which represent missing values
nrow(beer[which(beer$review_overall==0),]) #6 rows with 0 rating
beer<- beer %>% filter(beer$review_overall != 0)
beer <- beer[which(beer$review_overall!=0),]

#Rearranging the columns such that user column is first and the beer column is second
beer <- beer[,c(2,1,3)]


str(beer) #'data.frame' of 475978 obs. of 3 variables:


#Checking blank values
blank_value <- function(x){
  check <- sapply(x,function(x) length(which(x =="")))
  return (check)
}

blank_value1 <- blank_value(beer)
blank_value1 #100 blank cells


#Removing the rows which has blank values
beer[beer==""]<-NA
beer <- beer[complete.cases(beer),]

#Checking if blanks eliminated
blank_value2 <- blank_value(beer)
blank_value2 # no blanks

#REmoving duplicate values (one user has given multiple ratings for same beer brand)
beer <- beer %>% distinct(review_profilename,beer_beerid, .keep_all = TRUE)

#Creating derived metric by generating unique ID's corresponding to each unique user.
#i.e.  first column as the User ID column and the second column as the item column

setDT(beer)[, user_id := .GRP, by = review_profilename] # Unique user ID's
beer_cleaned <- beer[,c(4,2,3)] # new dataframe after generating unique id.


#-----------------------------------------------------------------------------------------------------------
# *********************ASSIGNMENT QUESTIONS ****************************************************************
#-----------------------------------------------------------------------------------------------------------

#1.Choose only those beers that have at least N number of reviews,
#Figure out an appropriate value of N using EDA; this may not have one correct answer, 
#but you shouldn't choose beers having extremely low number of ratings

# number of unique beer ID's
length(unique(beer_cleaned$beer_beerid)) #40302

combined_beer <- group_by(beer_cleaned,beer_beerid) %>% summarise(number_of_reviews=n())
summary(combined_beer)

#Retaining  beerID's with average number of reviews greater than >12
combined_beer_UBCF <- combined_beer[which(combined_beer$number_of_reviews >12),]
summary(combined_beer_UBCF)

#Lets aggregate the number of reviews per user and then check the statistics
user_review_average <- group_by(beer_cleaned,user_id) %>% summarise(no_of_Reviews=n())
summary(user_review_average)

#Retaining User ID's with an average number of reviews greater than 21
user_review_average_filtered <- user_review_average[which(user_review_average$no_of_Reviews >21),]
summary(user_review_average_filtered)

#creating a new dataframe  based on thresholds/Cut off genearted (N)as above  for number of Beer and user reviews
beer_maindf <- beer_cleaned[(beer_cleaned$beer_beerid %in% combined_beer_UBCF$beer_beerid)& (beer_cleaned$user_id %in% user_review_average_filtered$user_id),]
str(beer_maindf)



beer_maindf$user_id <- as.factor(beer_maindf$user_id)
beer_maindf$beer_beerid <- as.factor((beer_maindf$beer_beerid))
beer_maindf$review_overall <- as.integer(beer_maindf$review_overall) # I have assumed that ratings are integers from 1 to 5 instead of floating numbers.

#Converting to dataframe
beer_maindf <- as.data.frame(beer_maindf)

#2. The second objective of the data preparation phase is to Convert this data frame to a "realratingMatrix" before we build the collaborative filtering models

#Converting dataframe to realratingmatrix
beer_rrm <- as(beer_maindf, "realRatingMatrix") #converting to real rating matrix
class(beer_rrm)

# [1] "realRatingMatrix"
# attr(,"package")
# [1] "recommenderlab"

#Checking the realrating matrix
dimnames(beer_rrm)
rowCounts(beer_rrm)
colCounts(beer_rrm)
rowMeans(beer_rrm)


#------------------------------------------------------------------------------------------
# ******************Data exploration*******************************************************
#------------------------------------------------------------------------------------------
#1.Determine how similar the first ten users are with each other and visualise it

#***For users****

similar_users <- similarity(beer_rrm[1:10, ],
                            method = "cosine",
                            which = "users")
#Similarity matrix
as.matrix(similar_users)

#Value 0 represents similarity & 1 represents dissimilarity

#Viewing similarity matrix in heat map format
image(as.matrix(similar_users), main = "User similarity")

#Colour red shows high similarity & yellow shows low similarity, 
#white represent blank values as not all user have rated all beer brands.

#Conclusion-****Based on heatmap  user based has more similarity for first 10 users than items based ****

#2.Compute and visualise the similarity between the first 10 beers

# *******For Items**************

#Calculating similarity
similar_items <- similarity(beer_rrm[,1:10 ],
                            method = "cosine",
                            which = "items")
#Generating matrix
as.matrix(similar_items)

#Value 0 represents similarity & 1 represents dissimilarity

#Viewing Matrix in heat map format
image(as.matrix(similar_items), main = "Item similarity")

#Colour red shows high similarity & yellow shows low similarity, 
#white represent blank values as not all user have rated all beer brands.


#3.What are the unique values of ratings?

#Converting to vector as per realratingmatrix 

unique_ratings <- as.vector(beer_rrm@data)
sort(unique(unique_ratings) )

# 0 1 2 3 4 5 are the unique values of rating

#The ratings are integers in the range 0-5. 
#Checking frequency of ratings in table format

ratings_tabular <- table(unique_ratings)
ratings_tabular

# unique_ratings
# 0 1 2 3 4 5
# 22160533 3840 17729 95007 190807 16780

# A rating equal to 0 represents a missing value, removing them 

unique_ratings <- unique_ratings[unique_ratings != 0]

#frequency plot of the ratings
unique_ratings <- factor(unique_ratings)

# Viewing  distribution using qplot
qplot(unique_ratings) + ggtitle("Distribution of the ratings")

#Most of the ratings are above 3 and the most common rating is 4

#4 Visualise the rating values and notice:

#a.The average beer ratings
beer_ratings_average <- colMeans(beer_rrm)
beer_ratings_average
qplot(beer_ratings_average) + stat_bin(binwidth = 0.05) + ggtitle("Distribution of the average movie rating")

#The highest value lies in the range between  3.8 to 4

beer_rating <- as.data.frame(group_by(beer_maindf,beer_beerid) %>% summarise(avg_beer_rating=mean(review_overall)))
summary(beer_rating)

# the average beer rating is 3.5

#b: The average user ratings
user_ratings_average <- rowMeans(beer_rrm)
user_ratings_average


#Visualizing using qplot

qplot(user_ratings_average)+ stat_bin(binwidth = 0.05) + ggtitle("Distribution of the average user rating")

user_rating <- group_by(beer_maindf,user_id) %>% summarise(avg_user_rating=mean(review_overall))
summary(user_rating$avg_user_rating)

#the average rating given to beers by an user is 3.6

hist(user_rating$avg_user_rating, breaks=100, col="red")

#c: The average number of ratings given to the beers

beer_ratings_averagenumber <- rowCounts(beer_rrm)

#Visualizing using qplot

qplot(beer_ratings_averagenumber)+ stat_bin(binwidth = 10) + ggtitle("Distribution of the average number of beer rating")

beer_ratingsnumber <- group_by(beer_maindf,beer_beerid) %>% summarise(no_of_ratings =n())

summary(beer_ratingsnumber)

#The average number of ratings for beers is 55

##Visualising 
hist(beer_ratingsnumber$no_of_ratings, breaks=10, col="red") 
#Histogram shows plot is skewed towards left

#d )The average number of ratings given by the users :

average_num_user_ratings <- colCounts(beer_rrm)
qplot(average_num_user_ratings)+ stat_bin(binwidth = 10) + ggtitle("Distribution of the average number of rating by user")

#lets group by user ID's and summarise by number of ratings
ratings_by_users <- group_by(beer_maindf,user_id) %>% summarise(no_of_ratings =n())
summary(ratings_by_users)

# Plotting histogram
hist(ratings_by_users$no_of_ratings, breaks=10, col="red") #skewed towards left


#---------------------------------------------------------------------------------------------
# *********************Recommendation models*****************************************
#---------------------------------------------------------------------------------------------

# models available inside realrating matrix

recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
# names(recommender_models)# 9 types of models

#description of recommendation system algorithms
lapply(recommender_models, "[[", "description")

#For our case study UBCF & IBCF is required.

#checking the parameters of these two models
recommender_models$IBCF_realRatingMatrix$parameters



#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#*******To  optimize the run time of the algorithm & to avoid memory error & Cholmod Error , subsetting the data further
#Filtering  beers that has got 30 ratings , Users who have rated at least 25 beers and by beer ratings >3 

beer_rrm <- beer_rrm[rowCounts(beer_rrm) >25,colCounts(beer_rrm) >30 ]
beer_maindf <- beer_maindf[which(beer_maindf$review_overall >3),]

beer_rrm <- as(beer_maindf, "realRatingMatrix") #converting from dataframe to real rating matrix


######******************Divide your data into training and testing datasets***********************

#Experiment with 'split' and 'cross-validation' evaluation schemes#
#--------------------------------------------------------------------------------------------------------
#We have to  find the best value of K and split to pick a model which has least RMSE value, 
#A good model has low RMSE.

#generating function for  K=1 to K=5 and splitting  the data for training  sequence from 0.6 to 0.8 with intervals of 0.05

#Initializing blank DF for storing accuracy measures for UBCF & IBCF

UBCF_error <- data.frame()
IBCF_error <- data.frame()

#Splitting training sequence

train_split <- seq(0.6,0.80,by=0.05)
# sequence for K
k_split <- seq(1,5)


#created an evaluation scheme which splits the users
    #into a training set and test set for values indicated by K(cross validation)
    #For the test items (given argument inside evaluation scheme) will be given to the
    #recommender algorithm and the other items will be held out for computing the error
    #With goodRating=4 all items with actual user rating of greater or equal to 4 is  considered
   

    ##############Building the recommendation models###########################3
#*******Takes 30 mins to get executed*********

for (i in train_split){
  for (j in k_split){
    #scheme declaration
    set.seed(100)
    scheme <- evaluationScheme(beer_rrm, method = "split", train = i,
                               k = j, given=12 , goodRating = 4)
    
    
    
    UBCF_recommend<-Recommender(getData(scheme, "train"), "UBCF")
    IBCF_recommend<- Recommender(getData(scheme, "train"), "IBCF")
    # Making Predictions
    UBCF_prediction <- predict(UBCF_recommend, getData(scheme, "known"), type="ratings")
    IBCF_prediction<- predict(IBCF_recommend, getData(scheme, "known"), type="ratings")
    #Evaluating the performance parameters
    error_UBCF_prediction<-calcPredictionAccuracy(UBCF_prediction, getData(scheme, "unknown"))
    error_IBCF_prediction<-calcPredictionAccuracy(IBCF_prediction, getData(scheme, "unknown"))
    #Storing the result in a dataframe
    UBCF_error1 <- cbind(data.frame(iteration=paste("split_",i,"k_",j)),t(data.frame(error_UBCF_prediction)))
    UBCF_error <- rbind(UBCF_error1,UBCF_error)
    IBCF_error1 <- cbind(data.frame(iteration=paste("split_",i,"k_",j)),t(data.frame(error_IBCF_prediction)))
    IBCF_error <- rbind(IBCF_error1,IBCF_error)
  }
}


#rownames reset
rownames(IBCF_error) <- NULL
rownames(UBCF_error) <- NULL

#performance measures for both UBCF and IBCF
View(UBCF_error)
View(IBCF_error)

#Finding minimum RMSE value out of the 25 iterations 
#and taking the iteration values which got the min value of RMSE

min(UBCF_error$RMSE) # 0.2849448
UBCF_error$iteration[UBCF_error$RMSE==min(UBCF_error$RMSE)] # split_ 0.6 k_ 3
min(IBCF_error$RMSE) # 0.3983118
IBCF_error$iteration[IBCF_error$RMSE==min(IBCF_error$RMSE)] # split_ 0.6 k_5


#----------------------------------------------------------------------------------------
# ****************Plotting ROC for  UBCF and IBCF***************************
#-------------------------------------------------------------------------------------



#Visualizing  the ROC plot
#by generating the algorithms for both UBCF and IBCF 
#based on thresholds for K and split values
#Defining scheme , i.e split=0.6 and k=3

set.seed(100)
scheme <- evaluationScheme(beer_rrm, method = "split", train = .6,
                           k = 3, given=12 , goodRating = 4)
scheme

#####Defining alogorithms for both UBCF & IBCF#####

algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=30, minRating=4)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
  ))
)


# running algorithms&  predicting next n beers
#Takes 5 min for execution

results <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15, 20))
class(results)


# plotting ROC curve
plot(results, annotate = 1:4, legend="topleft")


#### Based on both ROC plot and the the RMSE values we got ,UBCF is a better model than the IBCF .

#------------------------------------------------------------------------------------------------------------

#recommending  top 5 beers to users "cokes", "genog" & "giblet"

#Identifying the user ID's for user names from the unique id generated for user names.

user_ID<- c("cokes","genog","giblet") #Vector of users for which prediction has to be done
reviewer_id <- c()
for (i in 1:length(profilename_vec)){
  reviewer_id<- c(reviewer_id,beer$user_id[beer$review_profilename == profilename_vec[i]][1])
}
reviewer_id


#[1] 440 1092 747, 
#i.e cokes=440,genog=1092,giblet=747


#-----------------------------------------------------------------------------------------------------------------------
#Prediction
#-----------------------------------------------------------------------------------------------------------------------
#1.Building individual recommendation models for UBCF and IBCF based on the threshold value for split & Cross validation (K)
#2. Predict top 5 beer's to the users using both UBCF and IBCF

#--------------------------------------------------------------------------------------------------------
#Predicting using the UBCF
#--------------------------------------------------------------------------------------------------------

#UBCF with split=0.6 and k= 3
#Using  scheme used for generating ROC  i.e split=0.75 and k=4

UBCF_recommend<-Recommender(getData(scheme, "train"), "UBCF")

#Making predictions using UBCF
# recommended the top 5 beers to the 3 specified users using UBCF using the best threshold parameters
# of k and split,

# recommending top 5 items for for cokes , user ID=440
cokes_recommend <- predict(UBCF_recommend, beer_rrm["440",], n=5)

# to display them
as(cokes_recommend, "list")
#$`440`
#[1]"1010"  "29619" "47658" "22227" "5281"

# recommending top 5 items for for genog , user ID =1092
genog_recommend <- predict(UBCF_recommend, beer_rrm["1092",], n=5)
# to display them
as(genog_recommend, "list")
#$`1092`
#[1] "1708"  "34420" "1545"  "6549"  "1256" 

# recommending top 5 items for for giblet , user ID =747
giblet_recommended <- predict(UBCF_recommend, beer_rrm["747",], n=5)
# to display them
as(giblet_recommended, "list")
# $`747`
# [1] "5" "6" "7" "10" "14"

 #Recommendation for users based on IBCF

#--------------------------------------------------------------------------------------------------------
#Predicting using the IBCF
#--------------------------------------------------------------------------------------------------------
# building  IBCF with split=0.6 and k= 5
#generating scheme 
set.seed(100)
scheme <- evaluationScheme(beer_rrm, method = "split", train = .6,
                           k = 5, given=12 , goodRating = 4)
#Building recommender model using IBCF
IBCF_recommend <- Recommender(getData(scheme, "train"), "IBCF")
#Making predictions using UBCF
# recommending top 5 items for for cokes , user ID=440
cokes_recommend_ibcf <- predict(IBCF_recommend, beer_rrm["440",], n=5)
# to display them
as(cokes_recommend_ibcf, "list")
#$`440`
#[1]"821"   "916"   "6088"  "17492" "18093"
# recommending top 5 items for for genog , user ID =1092
genog_recommend_ibcf <- predict(IBCF_recommend, beer_rrm["1092",], n=5)
# to display them
as(genog_recommend_ibcf, "list")
#$`1092`
#[1] "133" "134" "219" "228" "593"
# recommending top 5 items for for giblet , user ID =747
giblet_recommended_ibcf <- predict(IBCF_recommend, beer_rrm["747",], n=5)
# to display them
as(giblet_recommended_ibcf,"list")
# character(0) implying user is not found in matrix

#----------------------- CONCLUSION ------------------------------------------------------


# UBCF model outperforms the IBCF model
# recommend the top 5 beers to the users based on UBCF model, we have also made predictions based on IBCF.



