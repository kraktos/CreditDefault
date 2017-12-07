# ALl functionalities needed for init phase of the application


# List of all libraries needed across the application
library(dplyr)
library(purrr) 
library(pROC) # for AUC calculations
library(stringr) # string manipulations
library(RANN) 
library(doMC) # for parallel training
library(ggplot2) # for fancy plots
library(caret) # ML library of choice
library(yaml)
library(GGally)

# register all the cores to do prallel training
# use pnly 33% of cores
registerDoMC(cores = parallel::detectCores()/2)

# set data display options, avoiding exponent forms
options(scipen = 999)

# load the config file (YAML) and return the configuration object
get.configurations <- function(path){
  configurations <- yaml.load_file(path)
  return(configurations)
}
