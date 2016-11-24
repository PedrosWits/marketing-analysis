######################################################################
#                      Frequentist Approach                          #
######################################################################

rm(list=ls())
source("exploration.R")

# Best Subset Selection
library(leaps)

predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

regfit.best=regsubsets(Income~.,data=marketing_naRemoved,nvmax=14)

k=10
p=ncol(marketing_naRemoved)-1
set.seed(1)
folds=sample(rep(1:k,length=nrow(marketing_naRemoved)))
cv.errors=matrix(NA,k,p)

for (i in 1:k) {
  best.fit = regsubsets(Income ~ ., data = marketing_naRemoved[folds != i, ], nvmax = p)
  for (j in 1:p) {
    pred = predict(best.fit, marketing_naRemoved[folds == i, ], id = j)
    cv.errors[i, j] = mean((marketing_naRemoved$Income[folds == i] - pred)^2)
  }
}

mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
pdf("frequentist_subsetselection.pdf")
plot(mean.cv.errors,type='b',main="Best Subset Selection")
dev.off()
reg.best=regsubsets(Income~.,data=marketing_naRemoved,nvmax=14)
coef(reg.best,7)
mean.cv.errors[7]


# Define x and y + train and test sets
library(glmnet)
set.seed(1)

x=model.matrix(Income~.,-1,data=marketing_naRemoved)[,-1]
y=marketing_naRemoved$Income

train=sample(1:nrow(x),nrow(x)/2)
test=(-train)

# Lasso
lasso.mod=glmnet(x,y,alpha=1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
out=glmnet(x,y,alpha=1, lambda=10^seq(10, -2, length=100))

plot(cv.out,main="Lasso")
lasso.coef=predict(out,type="coefficients",s=bestlam)

# Residuals
residuals = lasso.pred-y.test
par(mfrow = c(2,2))
hist(residuals)
qqnorm(residuals)
hist(y.test)
hist(lasso.pred)
# Comments: Residuals are more or less normally distributed (see histogram + q-q plot)
#           However, the most common category (0,1) is massively under-predicted
#           while mid categories are over-predicted.


# Ordinal logit
m <- polr(as.ordered(y) ~ x, data = marketing_naRemoved, Hess=TRUE)
summary(m)
confint(m)


# PCR
library(pls)
pcr.fit=pcr(Income~.,data=marketing_naRemoved,scale=TRUE,validation="CV")
summary(pcr.fit)
#pdf("frequentist_pcr.pdf")
validationplot(pcr.fit,val.type="MSEP",main="PCR Method")
#dev.off()
pcr.pred=predict(pcr.fit,x[test,],ncomp=1)
mean((pcr.pred-y.test)^2)


# GLM

fit<-glm(Income~.,data=marketing_naRemoved)
summary(fit)


##############################
#   Source exploration File  #
##############################
current_dir = dirname(sys.frame(1)$ofile)
source(paste(current_dir, "exploration.R", sep="/"))



