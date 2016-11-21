###########################
#       Libraries         #
###########################
library(ElemStatLearn)
library(reshape2)
library(ggplot2)


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

upper_bounds = c(0, 10000, 15000,
                 20000, 25000, 30000,
                 40000, 50000, 75000)
incomes = c("< 10,000", "< 15,000", "< 20,000",
            "< 25,000", "< 30,000", "< 40,000",
            "< 50,000", "< 75,000", "75,000")
marketing$Income = factor(marketing$Income,
                          levels = incomes,
                          ordered = TRUE)

marketing$Sex = factor(marketing$Sex,
                       labels = c("Male", "Female"),
                       ordered = FALSE)

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


###############################
# Dataset structure after fix #
###############################
dim(marketing)


###############################
# Summary Statistics          #
###############################

