# Missing data
rm(list=ls())
library(ElemStatLearn)
data(marketing)

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
checkForNA(predictors_naRemoved)


# Replace missing values with the mean
