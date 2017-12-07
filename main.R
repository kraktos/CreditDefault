

############### README  ##########################

# The project has couple of files to modularise the functionalities
# 1. config.yml:  has the configurations needed 
# 2. data.R:      all data related manipulations and reads
# 3. init.R:      loads all the libraries and sets some options
# 4. model.R:     actual ML code implementation
# 5. RA_sol.R:    main file which should be run and calls the above files
# 
# Unzip and set working directory to the unzipped directory.
# set the training and test files paths in the config.yml
# This program generates few plots, 
# Prints AUC for few algorithms



############### LOAD THE MODULES AND LIBRARIES #######################

# some system constants
source("init.R", local = TRUE)

# load the data files. having all data manipulation tasks
source("data.R", local = TRUE)

# the actual model training funtions are in this module
source("model.R", local = TRUE)

# read the configuration file, once loaded all the configs can be accessed by
# using '$' on the following object
configurations <- get.configurations(path = "config.yml")

############### LOAD THE DATA  ##########################

# load the training data and the test data
train.df <-
  get.data(path = configurations$training, seperator = ",")
test.df <- get.data(path = configurations$testing, seperator = ",")

############### GENERIC TRANSFORM ##########################

# drop columns which are more than allowed.NA.levelpercentage of empty values
train.df.temp <-
  drop.columns(train.df, allowed.NA.level = configurations$NA.level)

# Drop highly correlated numerical features
drop <-
  drop.correlated.features(train.df.temp, threshold = configurations$corr.thresh)
train.df.temp <- train.df.temp[,!colnames(train.df.temp) %in% drop]

# generate a test dataframe with same columns
test.df.temp <-
  test.df[, colnames(test.df) %in% colnames(train.df.temp)]

############### CUSTOM TRANSFORM ##########################

# transform the cleaner data set for feature generation.
train.df.temp <- feature.generate(train.df.temp)
test.df.temp <- feature.generate(test.df.temp)

# dropping the malformed rows
test.df.temp <- subset(test.df.temp,!is.na(annual_inc))

############### FEATURE SCALE ##########################

#Preprocessing and Imputing
preProcValues <-
  preProcess(train.df.temp, method = c("medianImpute", "center", "scale"))
train.df.temp <- predict(preProcValues, train.df.temp)
test.df.temp <- predict(preProcValues, test.df.temp)

# This drastically effects the training time, since, label-encoding factors, is better
# compared to one-hot encoding in terms of feature space expansion.

# convert factors to numeric
character_vars <- lapply(train.df.temp, class) == "factor"
train.df.temp[, character_vars] <-
  lapply(train.df.temp[, character_vars], as.numeric)

character_vars <- lapply(test.df.temp, class) == "factor"
test.df.temp[, character_vars] <-
  lapply(test.df.temp[, character_vars], as.numeric)

# make target back to factor
train.df.temp$l_state <- as.factor(train.df.temp$l_state)
levels(train.df.temp$l_state) <- c("Default", "Fully.Paid")

test.df.temp$l_state <- as.factor(test.df.temp$l_state)
levels(test.df.temp$l_state) <- c("Default", "Fully.Paid")


############### CLASS IMBALANCE CHECK ##########################

prop.table(table(train.df.temp$l_state)) * 100

############### DATA PARTITION ##########################

# drop NAs in target
train.df.temp <- subset(train.df.temp,!is.na(l_state))
train.df.temp <- train.df.temp[complete.cases(train.df.temp),]

# split train-test
tr_idx <-
  createDataPartition(train.df.temp$l_state, p = 0.8, list = FALSE)
training <- train.df.temp[tr_idx,]
testing <- train.df.temp[-tr_idx,]

# unblock to train on a smaller dataset
#training <- head(training, 50000)


# ADD WEIGHTS as one solution to the imbalance
# also try up/down sampling or SMOTE
# Create model weights
# Current implementaion allows either sample weights or UP sampling.
model_weights <- ifelse(training$l_state == "Default",
                        (1 / table(training$l_state)[1]) * 0.5,
                        (1 / table(training$l_state)[2]) * 0.5)

############### MODEL TRAINING ##########################

print(paste0("Models training,, "))

# LogReg Model ############
model_glm <-
  get.model.factory(data = training,
                    type = "glm",
                    model_weights = NA)

# GBM Model ############
model_gbm <- get.model.factory(data = training,
                               type = "gbm",
                               model_weights = NA)

# extrem GB ############
model_xgb <- get.model.factory(data = training,
                               type = "xgb",
                               model_weights = NA)

# RF Model ############
model_rf <- get.model.factory(data = training,
                              type = "rf",
                              model_weights = NA)


############### MODEL PERFORMANCE ##########################

# ONE  VIEW, report the AUC and plot the ROC curve for each #####
model_list <- list(
  gbm = model_gbm,
  rf = model_rf,
  logreg = model_glm,
  xgb = model_xgb
)

# print the AUC scores
model_list_roc <- model_list %>%
  map(test_roc, data = testing)

model_list_roc %>%
  map(auc)

print(paste0("ROC Curves plots.. "))
# view the ROC
view.data(model_list_roc)

print(paste0("Variable Importance plot... "))
# plot the variable importance
plot(varImp(model_gbm, scale = TRUE))

############### Q-A ##########################

#2. Do you face any problem and how would you solve them?
# The data is heavily unbalanced. One can us saple class weights to weight the under- 
#represented class highly
# OR we can 'up' or 'down' sampling.

#6. What are the assumptions and limitations of your model?
# Did not check for corelation between the categorical columns.
# No exhaustive tuning for hyper parameters for the training.

#7. This model should be an easy implementation, if you would have a higher 
#budget and more time, what could you provide in addition to this approach?
# Firstly, perform more of  features engineering and improve
# Try k-fold validations on larger ensembles.
# Try stacked ensemble or ANN based aproaches.
