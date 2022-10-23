###########################
###### Random Forest ######
###########################
#### Income Predictions####
###########################

# install.packages("randomForest")
# install.packages("gbm")
# install.packages("xgboost")
# install.packages("ranger")
# install.packages("caret")

library(randomForest)
library(gbm)
library(xgboost)
library(ranger)
library(caret)

maindf <- gd.full

set.seed(1)

rf.df <- maindf %>% 
  dplyr::select(
    subid,
    sex,
    age_dec,
    scale_age,
    race,
    year,
    scale_income,
    raw_income,
    COMB_WEIGHT) %>%
  filter_at(
    vars(
      subid,
      sex,
      age_dec,
      scale_age,
      race,
      year,
      scale_income,
      raw_income,
      COMB_WEIGHT
    ),
    all_vars(!is.na(.))
  ) #select variables first and then!!! do filter_at


#maindf %>% write.csv("Gallup_Daily_2017_clean.csv")
#derived variables have been added already
#recode done----------------------------------------------------------------------------------------------------------------

#create training dataframe
df.train <- rf.df %>% dplyr::select(scale_age, #used raw age in version 1.0
                                     sex,
                                     race, 
                                     scale_income,
                                     year
                                     #raw_income #used raw income in version 1.0
                                     )
head(df.train)
summary(df.train)


#try imputing variables
#impute missing values
# library(Hmisc)
# df.train.hmisc <- data.frame(Map(function(x) Hmisc::impute(x, 'random'), df.train)) #default is median
# ?Hmisc::impute


#run random forest
rf <-
  ranger(
    scale_income ~ ., #used to be raw_income
    data = df.train,
    #df.train[complete.cases(df.train),], 
    case.weights = rf.df$COMB_WEIGHT,
    num.trees = 500,
    mtry = 3,
    min.node.size = 500
  ) 

length(rf$predictions) #check length of predictions; joining data later
#rf.predictions.scaled <- scale(rf$predictions) #possible that you don't need this step bc you input scaled income already

#create final df with rf predicted income
final.df <-
  cbind(
    rf.df,
    data.frame(rf$predictions))
head(final.df)
#check correlations
cor(final.df$rf.predictions,final.df$scale_income) #r=0.324


#write files
final.df %>% write.csv("Gallup_Daily_income_pred_final.csv")
# final.df %>% write_rds("Gallup_Daily_final_2017_imputed_rds.rds")


gallup_rf_income <-
  final.df %>% 
  dplyr::select(
    subid,
    rf.predictions
  )

#write_csv(gallup_rf_income, "D:/data/gallup/results/ml_predictions/gallup_rf_income_preds.csv")
write_csv(gallup_rf_income, "gallup_rf_id_predincome.csv")
