###############################################################
# Data Science - Assignment 1 (Regression)
# Autumn semester 2022
###############################################################

## Task description

# Your customer is Lending Club, a peer-to-peer credit marketplace in the US. It connects
# private lenders (people who hand out loans) with loan applicants. Your task is to provide a regression
# model that gives a recommendation to the lenders about a suitable interest rate for a given applicant.
# (the dataset was provided by the professor)

######## THE FINAL MODEL ########

## Libraries used

# if needed use install.packages("package_name")
library(dplyr) # select and filter functions
library(zoo) # yearmon function
library(lubridate) # for dates
library(caret) # preprocessing
library(ranger) # random forest
library(randomForest) # random forest
library(imputeMissings) # imputing with median/mode
library(vip) # variable importance
library(Hmisc) # cut2()

## Read data

# prerequisite: original csv file is already unzipped and copied to the working directory
final_data <- read.csv("LCdata.csv", sep = ";", header = TRUE, na.strings = c("", "NA", "n/a"))
# separator is not comma but semicolon, there is a header, empty cells and strings NA or n/a are coded as "real" NAs

## Remove joint account applications
# there are so less joint accounts that after all we decided to delete these rows and apply business rules later on
final_data <- final_data[final_data$application_type != "JOINT", ] # delete data rows where application status is joint

## Set a threshold for sparsity and remove sparse data points
final_data <- final_data[rowSums(is.na(final_data)) < 27, ] # delete data rows that have more than 27 NAs
rownames(final_data) <- NULL # reset row names in order to have consecutive index numbers

## Remove features that were not used for the final model

final_data <- select(final_data, -c(collection_recovery_fee,
                                    installment,
                                    issue_d,
                                    last_pymnt_amnt,
                                    last_pymnt_d,
                                    loan_status,
                                    next_pymnt_d,
                                    out_prncp,
                                    out_prncp_inv,
                                    pymnt_plan,
                                    recoveries,
                                    term,
                                    total_pymnt,
                                    total_pymnt_inv,
                                    total_rec_int,
                                    total_rec_late_fee,
                                    total_rec_prncp,
                                    id,
                                    member_id,
                                    url,
                                    title,
                                    desc,
                                    emp_title,
                                    open_acc_6m,
                                    open_il_6m,
                                    open_il_12m,
                                    open_il_24m,
                                    mths_since_rcnt_il,
                                    total_bal_il,
                                    il_util,
                                    open_rv_12m,
                                    open_rv_24m,
                                    max_bal_bc,
                                    all_util,
                                    inq_fi,
                                    total_cu_tl,
                                    inq_last_12m,
                                    zip_code,
                                    policy_code,
                                    addr_state,
                                    last_credit_pull_d,
                                    tot_coll_amt,
                                    loan_amnt,
                                    funded_amnt_inv,
                                    application_type,
                                    annual_inc_joint,
                                    verification_status_joint,
                                    dti_joint))

## Preprocessing

## simplify emp_length (ordered)
final_data$emp_length_simple <- final_data$emp_length
final_data$emp_length_simple[grepl("< 1 year", final_data$emp_length_simple)] <- "less than 1"
final_data$emp_length_simple[grepl("1 year|2 years|3 years", final_data$emp_length_simple)] <- "1 to 3 years"
final_data$emp_length_simple[grepl("4 year|5 years|6 years", final_data$emp_length_simple)] <- "4 to 6 years"
final_data$emp_length_simple[grepl("7 year|8 years|9 years", final_data$emp_length_simple)] <- "7 to 9 years"
final_data$emp_length_simple <- addNA(final_data$emp_length_simple) # turn NA into an extra level
levels(final_data$emp_length_simple) # "1 to 3 years" "10+ years" "4 to 6 years" "7 to 9 years" "less than 1"  NA 
levels(final_data$emp_length_simple) <- c("1 to 3 years", "10+ years", "4 to 6 years", "7 to 9 years", "less than 1", "missing") # rename levels
final_data$emp_length_simple <- factor(final_data$emp_length_simple, levels =
                                     c("missing", "less than 1", "1 to 3 years", "4 to 6 years", "7 to 9 years", "10+ years"))
# reorder levels so that 10+ year is the highest level
levels(final_data$emp_length_simple) # new levels "missing" "less than 1"  "1 to 3 years" "4 to 6 years" "7 to 9 years" "10+ years" 
final_data$emp_length <- NULL # remove original variable

## home_ownership (ordered)
final_data$home_ownership[grepl("ANY|NONE", final_data$home_ownership)] <- "OTHER"
# categories "ANY" and "NONE" don't have many values, assign them to category "OTHER"
final_data$home_ownership <- factor(final_data$home_ownership, levels = c("OTHER", "RENT", "MORTGAGE", "OWN"))
# change to factor and reorder levels

## Create a new variable y_since_earliest_cr_line (numeric)
final_data$earliest_cr_line <- as.yearmon(final_data$earliest_cr_line, format = "%b-%Y") # use zoo package to convert to "yearmon" class
final_data$earliest_cr_line <- as.Date(final_data$earliest_cr_line) # convert  to date class (day in the format "01" is added to each entry by default)
start <- as.Date(final_data$earliest_cr_line)
end <- as.Date("2012-11-01") # count difference to the max value
final_data$y_since_earliest_cr_line <- lubridate::time_length(difftime(end, start), "years")
final_data$earliest_cr_line <- NULL
# remove objects that are not anymore needed
rm(start) 
rm(end) 

## verification_status (ordered)
final_data$verification_status <- factor(final_data$verification_status, levels = c("Not Verified", "Verified", "Source Verified"))
# change to factor and reorder levels

## revol_util
final_data$revol_util[final_data$revol_util > 180] <- 193.0 # replace with third highest value (193.0)

## tot_cur_bal
final_data$tot_cur_bal[final_data$tot_cur_bal > 5000000] <- 4447397 # replace with second highest value (4447397)

## total_rev_hi_lim
final_data$total_rev_hi_lim[final_data$total_rev_hi_lim > 3000000] <- 2013133 # replace with second highest value (2013133)

## purpose (not for the final model!)
# final_data$purpose[grepl("educational|major_purchase|medical|vacation|wedding|\\<car\\>", final_model$purpose)] <- "purchase" # combine categories
# final_data$purpose[grepl("home_improvement|house|moving|renewable_energy", final_model$purpose)] <- "home" # combine categories

## Transform mths_since_last_delinq into a factor
final_data$factor_last_delinq <- cut2(final_data$mths_since_last_delinq, g = 4) # create four quantile groups
final_data$factor_last_delinq <- addNA(final_data$factor_last_delinq) # turn NA into an extra level
levels(final_data$factor_last_delinq) # "[ 0, 16)" "[16, 32)" "[32, 51)" "[51,188]" NA
levels(final_data$factor_last_delinq) <- c("0-15", "16-31", "32-50", "51-188", "missing") # rename levels
levels(final_data$factor_last_delinq) # new levels "0-15"    "16-31"   "32-50"   "51-188"  "missing"
final_data$mths_since_last_delinq <- NULL # remove original variable

## Transform mths_since_last_record into a factor
final_data$factor_last_record <- cut2(final_data$mths_since_last_record, g = 4) # create four quantile groups
final_data$factor_last_record <- addNA(final_data$factor_last_record) # turn NA into an extra level
levels(final_data$factor_last_record) # "[ 0, 52)" "[52, 71)" "[71, 93)" "[93,129]" NA 
levels(final_data$factor_last_record) <- c("0-51", "52-70", "71-92", "93-129", "missing") # rename levels
levels(final_data$factor_last_record) # new levels "0-51"    "52-70"   "71-92"   "93-129"  "missing"
final_data$mths_since_last_record <- NULL # remove original variable

## Transform mths_since_last_major_derog into a factor
final_data$factor_last_derog <- cut2(final_data$mths_since_last_major_derog, g = 4) # create three quantile groups
final_data$factor_last_derog <- addNA(final_data$factor_last_derog) # turn NA into an extra level
levels(final_data$factor_last_derog) #"[ 0, 28)" "[28, 45)" "[45, 62)" "[62,188]" NA  
levels(final_data$factor_last_derog) <- c("0-27", "28-44", "45-61", "62-188", "missing") # rename levels
levels(final_data$factor_last_derog) # new levels "0-27"    "28-44"   "45-61"   "62-188"  "missing"
final_data$mths_since_last_major_derog <- NULL # remove original variable

## Impute missing values using median/mode (imputeMissings package)
final_data$collections_12_mths_ex_med <- impute(final_data$collections_12_mths_ex_med, object = NULL, method = "median/mode")
final_data$tot_cur_bal <- impute(final_data$tot_cur_bal, object = NULL, method = "median/mode")
final_data$total_rev_hi_lim <- impute(final_data$total_rev_hi_lim, object = NULL, method = "median/mode")
final_data$revol_util <- impute(final_data$revol_util, object = NULL, method = "median/mode")
final_data$revol_bal <- impute(final_data$revol_bal, object = NULL, method = "median/mode")

## Final dataset
names(final_data) # matches with tree_based_models script
str(final_data) # 797728 obs. of  24 variables
colSums(is.na(final_data)) # no NAs

## Save as csv file
write.csv(final_data, "1b_final_data.csv")

## randomForest with ranger package
model <- ranger(int_rate ~ ., data = final_data, mtry = 8, num.trees = 1000, max.depth = 23,
                 importance = "impurity", respect.unordered.factors = "order", seed = 1)
print(model) # MSE 10.91 (this is a high number - but with the given dataset no better results were possible!)

## Check variable importance
variableimportance <- vip(model)

## Save the trained model
saveRDS(model, file = "1a_final_model")
