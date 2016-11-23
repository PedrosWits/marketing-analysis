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


# TODO: Replace missing values with the mean


# Factor the appropriate variables
marketing_factored=marketing_naRemoved

marketing_factored$Sex=factor(as.numeric(marketing_factored$Sex))
marketing_factored$Marital=factor(as.numeric(marketing_factored$Marital))
marketing_factored$Occupation=factor(as.numeric(marketing_factored$Occupation))
marketing_factored$Dual_Income=factor(as.numeric(marketing_factored$Dual_Income))
marketing_factored$Status=factor(as.numeric(marketing_factored$Status))
marketing_factored$Home_Type=factor(as.numeric(marketing_factored$Home_Type))
marketing_factored$Ethnic=factor(as.numeric(marketing_factored$Ethnic))
marketing_factored$Language=factor(as.numeric(marketing_factored$Language))

# Some plots



