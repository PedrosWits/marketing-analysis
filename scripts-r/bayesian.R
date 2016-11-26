##############################
#   Clear Global Env         #
##############################
rm(list=ls())

##############################
#   Source exploration File  #
##############################
#current_dir = dirname(sys.frame(1)$ofile)
#source(paste(current_dir, "exploration.R", sep="/"))

######################################################################
#                         Bayesian Approach                          #
######################################################################

library(ElemStatLearn)

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

marketingRaw = marketing
marketing = marketing[complete.cases(marketing),]


#-----------------------------------------------------------------------------
# Gibbs Sampling on full data set
#-----------------------------------------------------------------------------
library(rjags)
mf=model.frame(Income~.,data=marketing)
Y=model.response(mf)
X=model.matrix(Income~.,mf)
n=nrow(X)
p=ncol(X)
data=list(n=n,p=p,Y=Y,X=X)

modelstring="model{
  for(i in 1:n){
    Y[i]~dnorm(Ymean[i],tau)
    Ymean[i]<-beta0+inprod(X[i,],beta)
  }
  tau~dgamma(1,0.001)
  beta0~dnorm(0,0.0001)
  for(j in 1:p){
    beta[j]~dnorm(0,0.001)
  }
}
"
init=list(tau=1,beta=rep(0,p))
model=jags.model(textConnection(modelstring),data=data,inits=init)
update(model,n.iter=1000)
output=coda.samples(model=model,variable.names=c("beta0","beta","tau"),n.iter=10000,thin=1)

save(output,file="FULL_GibbsSamplingOutput.rdata")
sink("FULL_GibbsSamplingOutput.txt")
summary(output)
sink()
pdf("FULL_GibbsSamplingOutput.pdf")
plot(output)
dev.off()
pdf("FULL_GibbsSamplingAutocorr.pdf")
autocorr.plot(output)
dev.off()
pdf("FULL_GibbsSamplingCrosscorr.pdf")
crosscorr.plot(output)
dev.off()

#-----------------------------------------------------------------------------
# Gibbs sampling on training set
#-----------------------------------------------------------------------------
set.seed(1337)
train=sample(1:nrow(marketing), nrow(marketing)*0.5)
test=(-train)

mf=model.frame(Income~.,data=marketing[train,])
Y=model.response(mf)
X=model.matrix(Income~.,mf)
n=nrow(X)
p=ncol(X)
data=list(n=n,p=p,Y=Y,X=X)

modelstring="model{
  for(i in 1:n){
    Y[i]~dnorm(Ymean[i],tau)
    Ymean[i]<-beta0+inprod(X[i,],beta)
  }
  tau~dgamma(1,0.001)
  beta0~dnorm(0,0.0001)
  for(j in 1:p){
  beta[j]~dnorm(0,0.001)
  }
}
"
init=list(tau=1,beta=rep(0,p))
model=jags.model(textConnection(modelstring),data=data,inits=init)
update(model,n.iter=1000)
output=coda.samples(model=model,variable.names=c("beta0","beta","tau"),n.iter=10000,thin=1)
save(output,file="TRAIN_GibbsSamplingOutput.rdata")
sink("TRAIN_GibbsSamplingOutput.txt")
summary(output)
sink()
pdf("TRAIN_GibbsSamplingOutput.pdf")
plot(output)
dev.off()
pdf("TRAIN_GibbsSamplingAutocorr.pdf")
autocorr.plot(output)
dev.off()
pdf("TRAIN_GibbsSamplingCrosscorr.pdf")
crosscorr.plot(output)
dev.off()
pdf("TRAIN_GibbsSamplingPairsOutput.pdf")
pairs(as.matrix(output),pch=".")
dev.off()

trainOutput=as.matrix(output)

# Make some predictions using the trained-model and see what the error is

testMF=model.frame(Income~.,data=marketing[test,])
Y=model.response(testMF)
X=model.matrix(Income~.,testMF)
n=nrow(X)
p=ncol(X)

gibbsPredict=function(modelOutput,x){
  modelMatrix=as.matrix(modelOutput)
  modelCoeff=as.vector(colMeans(modelMatrix))
  beta0=modelCoeff[ncol(modelMatrix)-1]
  beta=modelCoeff[1:(ncol(modelMatrix)-2)]
  y=numeric(nrow(x))
  y=beta0+x%*%beta
}

marketingGibbsPredict=gibbsPredict(trainOutput,X)
mean((marketingGibbsPredict-marketing[test,]$Income)^2)

#-----------------------------------------------------------------------------
# Bayesian variable selection with random effects on TRAINING SET
#-----------------------------------------------------------------------------
set.seed(1337)
train=sample(1:nrow(marketing), nrow(marketing)*0.5)
test=(-train)

mf=model.frame(Income~.,data=marketing[train,])
Y=model.response(mf)
X=model.matrix(Income~.,mf)
n=nrow(X)
p=ncol(X)
data=list(n=n,p=p,Y=Y,X=X)

init=list(tau=1,taub=1,pind=0.5,alpha=0,betaT=rep(0,p),ind=rep(0,p))
modelstring="
model {
  for (i in 1:n) {
    mean[i]<-alpha+inprod(X[i,],beta)
    Y[i]~dnorm(mean[i],tau)
  }
  for (j in 1:p) {
    ind[j]~dbern(pind)
    betaT[j]~dnorm(0,taub)
    beta[j]<-ind[j]*betaT[j]
  }
  alpha~dnorm(0,0.0001)
  tau~dgamma(1,0.001)
  taub~dgamma(1,0.001)
  pind~dbeta(2,8)
}
"
model=jags.model(textConnection(modelstring),
                 data=data,inits=init)
update(model,n.iter=1000)
output=coda.samples(model=model,
                    variable.names=c("alpha","beta","ind","tau","taub","pind"),
                    n.iter=10000,thin=1)

save(output,file="TRAIN_GibbsVariableSelection.rdata")
sink("TRAIN_GibbsVarSelect.txt")
summary(output)
sink()
pdf("TRAIN_GibbsVarSelect.pdf")
plot(output)
dev.off()
pdf("TRAIN_GibbsVarSelectAutocorr.pdf")
autocorr.plot(output)
dev.off()
pdf("TRAIN_GibbsSamplingCrosscorr.pdf")
crosscorr.plot(output)
dev.off()

#-----------------------------------------------------------------------------
# Predict some data using the gibbs variable selection model
#-----------------------------------------------------------------------------
summary(output)
trainVarSelectOutput=as.matrix(output)


testMF=model.frame(Income~.,data=marketing[test,])
Y=model.response(testMF)
X=model.matrix(Income~.,testMF)
n=nrow(X)
p=ncol(X)


gibbsVarSelectPredict=function(modelOutput,x){
  modelMatrix=as.matrix(modelOutput)
  modelCoeff=as.vector(colMeans(modelMatrix))
  alpha=modelCoeff[1]
  beta=modelCoeff[2:37]
  ind=modelCoeff[38:73]
  pind=modelCoeff[74]
  tau=modelCoeff[75]
  taub=modelCoeff[76]
  y=numeric(nrow(x))
  y=alpha+x%*%(beta*ind)
}

marketingGibbsVarSelectPredict=gibbsVarSelectPredict(trainVarSelectOutput,X)
mean((marketingGibbsVarSelectPredict-marketing[test,]$Income)^2)



