---
title: "in class 10"
author: "Benjamin Smith"
date: "12/2/2019"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```



Goals:

- To fit a mixture model to hourly wages
- To cluster subjects using a mixture model according to their wages
- And to examine the association between the clusters and education, sex and race.

(1) Use the wages data set to fit a mixture models with 2, 3 and 5 components. Plot in one graph the three fitted mixtures. Add one more line with a non-parametric density fit (tmp<-density(z) fits a density to variable z, $tmp\$x$ and $tmp\$y$ will give you the x-y coordinates for a density plot, try $lines(x=tmp\$x,y=tmp\$y)$ to add lines to an existing plot).

(2) Report proportion of male, average years of education and proportions of whites in each of the clusters, for each of the mixtures.


```{r}

wages <- read.table("wages.txt", header = T)


fitMixture=function(y,nComp,nIter=100){ 
 n=length(y)
 PROBS=matrix(nrow=n,ncol=nComp,0)
 
 # assigning observations to groups based on quantiles
 tmp=quantile(y,c(0,1:nComp/nComp))
 z=as.integer(cut(y,breaks=tmp))
 for(i in 1:n){
  PROBS[i,z[i]]=1
 }  
   
 mu=rep(NA,nComp)
 SD=rep(NA,nComp)
 alpha=rep(NA,nComp)
 
 ## Iterations
 for( i in 1:nIter){
	# M-step maximizes a weighted log-likelihood 
	K=sum(PROBS)
	for(j in 1:nComp){
		Nj=sum(PROBS[,j])		
		mu[j]=sum(y*PROBS[,j])/Nj		
		eHat=(y-mu[j])*sqrt(PROBS[,j])		
		vHat=sum(eHat^2)/Nj
		SD[j]=sqrt(vHat)
		alpha[j]=sum(PROBS[,j])/K
	}

	# E-step finds the probability that each observation belongs to each group	
	for(j in 1:nComp){
		PROBS[,j]=dnorm(y,mean=mu[j],sd=SD[j])*alpha[j]
	}
	# normalization 
	tmp=rowSums(PROBS)
	for(j in 1:nComp){
		PROBS[,j]=PROBS[,j]/tmp
	}		   
 }
 ANS=list(MEANS=mu,SD=SD,alpha=alpha,PROBS=PROBS)
 return(ANS)
}


##########
# calculate the mixture distribution 

y=wages$Wage # assigning y to wages$Wage is less typing and changing the given code
mu0=mean(y)
sd0=sd(y)


fm2=fitMixture(y,nComp=2)
fm3=fitMixture(y,nComp=3)
fm5=fitMixture(y,nComp=5)

exp2 <- apply(fm2$PROBS, 1, which.max)
exp3 <- apply(fm3$PROBS, 1, which.max)
exp5 <- apply(fm5$PROBS, 1, which.max)

### there is probably an easier way to find the mean for each distribution

mean(wages$Education[exp2==1])
mean(wages$Education[exp2==2])
table(exp2,wages$Sex)
table(exp2,wages$Black)

mean(wages$Education[exp3==1])
mean(wages$Education[exp3==2])
mean(wages$Education[exp3==3])
table(exp3,wages$Sex)
table(exp3,wages$Black)

mean(wages$Education[exp5==1])
mean(wages$Education[exp5==2])
mean(wages$Education[exp5==3])
mean(wages$Education[exp5==4])
mean(wages$Education[exp5==5])
table(exp5,wages$Sex)
table(exp5,wages$Black)

##########

 mixtureDensity=function(x,mu,sd,prob){
   n=length(x)
   f=rep(0,n)
   nComp=length(mu)
   for(i in 1:nComp){
     f=f+prob[i]*dnorm(x,mean=mu[i],sd=sd[i]) 
    }
   return(f)
 }

######
## more important changed stuff here


 tmp <- density(y) # z means y in this case 


 x=seq(from=min(y),to=max(y),length=1000)
 # f_true=mixtureDensity(x,mu=mu0,sd=sd0,prob=prob0) # the true density
 f_ML2=mixtureDensity(x,mu=fm2$MEANS,sd=fm2$SD,prob=fm2$alpha) # the density evaluated at the ML estimates of the parameters
 f_ML3=mixtureDensity(x,mu=fm3$MEANS,sd=fm3$SD,prob=fm3$alpha) # the density evaluated at the ML estimates of the parameters
 f_ML5=mixtureDensity(x,mu=fm5$MEANS,sd=fm5$SD,prob=fm5$alpha) # the density evaluated at the ML estimates of the parameters
# plot(f_true~x,col=2,type='l')
 plot(x=x,y=f_ML2,col=2,type = 'l', ylim = c(0,0.25))
 lines(x=x,y=f_ML3,col=3,lty=2)
 lines(x=x,y=f_ML5,col=4,lty=3)
 lines(x=tmp$x,y=tmp$y,col=5,lty=4)

 

```