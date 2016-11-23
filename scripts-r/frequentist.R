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


# Ridge Regression
library(glmnet)
x=model.matrix(Income~.,-1,data=marketing_naRemoved)[,-1]
y=marketing_naRemoved$Income
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
pdf("frequentist_ridgeregression.pdf")
plot(cv.out)
dev.off()
bestlam=cv.out$lambda.min
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
out=glmnet(x,y,alpha=0)
ridge.coef=predict(out,type="coefficients",s=bestlam)[1:14,]
ridge.coef



# Lasso
lasso.mod=glmnet(x,y,alpha=1)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
out=glmnet(x,y,alpha=1,lambda=grid)
pdf("frequentist_lasso.pdf")
plot(cv.out,main="Lasso")
dev.off()
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:14,]
lasso.coef



# PCR
library(pls)
pcr.fit=pcr(Income~.,data=marketing_naRemoved,scale=TRUE,validation="CV")
summary(pcr.fit)
pdf("frequentist_pcr.pdf")
validationplot(pcr.fit,val.type="MSEP",main="PCR Method")
dev.off()
pcr.pred=predict(pcr.fit,x[test,],ncomp=13)
mean((pcr.pred-y.test)^2)


# GLM

fit<-glm(Income~.,data=marketing_naRemoved)
summary(fit)


##############################
#   Source exploration File  #
##############################
current_dir = dirname(sys.frame(1)$ofile)
source(paste(current_dir, "exploration.R", sep="/"))



