###########################
#       Libraries         #
###########################
library(ElemStatLearn)
library(reshape2)
library(ggplot2)

rm(list=ls())
###########################
#       Dataset           #
###########################
data(marketing)
?marketing


###########################
#  Initial Exploration    #
###########################
names(marketing)
str(marketing)


##################################
#  Factoring Categorical Data    #
##################################

# Factor unordered categorical data
# but dont factor ordered categorical data

# Non ordered: sex, marital, occupation
#              dual_income, status,
#              home_type, ethnic, language

marketing$Sex = factor(marketing$Sex,
                       labels = c("Male", "Female"),
                       ordered = FALSE)

marketing$Marital = factor(marketing$Marital,
                           labels = c("Married", "Living",
                                      "Divorced", "Widowed",
                                      "Single"),
                           ordered = FALSE)

marketing$Occupation = factor(marketing$Occupation,
                              labels = c("Professional", "Sales",
                                         "Factory", "Clerical",
                                         "Homemaker", "Student",
                                         "Military", "Retired",
                                         "Unemployed"),
                              ordered = FALSE)

marketing$Dual_Income = factor(marketing$Dual_Income,
                               labels = c("Not Married",
                                          "Yes", "No"),
                               ordered = FALSE)

marketing$Status = factor(marketing$Status,
                          labels = c("Own", "Rent",
                                     "WithFamily"),
                          ordered = FALSE)

marketing$Home_Type = factor(marketing$Home_Type,
                             labels = c("House", "Condo",
                                        "Apartment", "Mobile",
                                        "Home"),
                             ordered = FALSE)

marketing$Ethnic = factor(marketing$Ethnic,
                          labels = c("American Indian", "Asian",
                                     "Black", "East Indian", "Hispanic",
                                     "Pacific", "White", "Other"),
                          ordered = FALSE)

marketing$Language = factor(marketing$Language,
                            labels = c("English", "Spanish", "Other"),
                            ordered = FALSE)
                             

str(marketing)
############################
#   Address Missing Values #
############################
Fix = list(NAIVE   = 0L,
           REGRESS = 1L)
fixType = Fix$NAIVE

if(fixType == Fix$NAIVE) {
  
} else {
  stop("Fix not implemented yet.")
}

# How many NA entries?
predictors=marketing[-1]
checkForNA=function(predictors){
  for(i in 1:ncol(predictors)){
    na_count=sum(is.na(predictors[,i]))
    output=paste(names(predictors[i]),":",na_count)
    print(output)
  }
}
checkForNA(predictors)

# Remove row if there is any missing data 
marketing_naRemoved<-marketing[complete.cases(marketing),]
predictors_naRemoved=marketing_naRemoved[-1]
#checkForNA(predictors_naRemoved)



###############################
# Dataset structure after fix #
###############################
dim(marketing)


###############################
# Summary Statistics          #
###############################

hist(marketing$Income)
#pairs(marketing)
summary(marketing)

