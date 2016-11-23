##############################
#   Clear Global Env         #
##############################
rm(list=ls())

##############################
#   Source exploration File  #
##############################
current_dir = dirname(sys.frame(1)$ofile)
source(paste(current_dir, "exploration.R", sep="/"))

######################################################################
#                         Bayesian Approach                          #
######################################################################

# Clear environment and load from exploration.R
rm(list=ls())
source("exploration.R")


# Gibbs Sampling
library(rjags)
n=nrow(marketing_naRemoved)
p=ncol(marketing_naRemoved[-1])
Y=marketing_naRemoved$Income
X=marketing_naRemoved[-1]
data=list(n=n,p=p,Y=Y,X=X)



modelstring="model{
  for(i in 1:n){
    Y[i]~dnorm(Ymean[i],tau)
    Ymean[i]<-b0+b[1]*X[i,1]+b[2]*X[i,2]+b[3]*X[i,3]+b[4]*X[i,4]+b[5]*X[i,5]+b[6]*X[i,6]+b[7]*X[i,7]+b[8]*X[i,8]+b[9]*X[i,9]+b[10]*X[i,10]+b[11]*X[i,11]+b[12]*X[i,12]+b[13]*X[i,13]
  }
  tau~dgamma(1,0.001)
  b0~dnorm(1,0.0001)
  for(j in 1:p){
    b[j]~dnorm(0,0.001)
  }
}
"
init=list(tau=1,b=rep(0,p))
model=jags.model(textConnection(modelstring),data=data,inits=init)
update(model,n.iter=1000)
output=coda.samples(model=model,variable.names=c("b0","b","tau"),n.iter=10000,thin=1)
pdf("rjags_output.pdf")
plot(output)
dev.off()
pdf("rjags_autocorr.pdf")
autocorr.plot(output)
dev.off()
summary(output)
effectiveSize(output)
HPDinterval(output)
pdf("rjags_crosscorr.pdf")
crosscorr.plot(output)
dev.off()
pdf("rjags_pairsoutput.pdf")
pairs(as.matrix(output),pch=".")
dev.off()


outputMatrix=as.matrix(output)
pdf("rjags_hist.pdf")
par(mfrow=c(3,5))
for(i in 1:13){
  title=paste("Beta_",i,sep="")
  hist(outputMatrix[,i],main=title,xlab="")
}
hist(outputMatrix[,14],main="Beta_0",xlab="")
hist(outputMatrix[,15],main="Tau",xlab="")
dev.off()


<<<<<<< HEAD
# Bayesian variable selection with random effects (ripped straight from Darrens blog)

data=list(Y=Y,X=X,n=n,p=p)
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
print(summary(output))
pdf("bayesian_variableselection.pdf")
plot(output)
dev.off()





#--------------------------------------------------------------------

