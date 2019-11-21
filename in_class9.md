---
title: "in class 9"
author: "Benjamin Smith"
date: "11/20/2019"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}

  wages=read.table('~/R_work/stat_comp/wage.txt',header=T)
  n=nrow(wages)
  nTst=100
  set.seed(195021) 
  tst=sample(1:n,size=nTst)
  TRN.DATA=wages[-tst,]
  TST.DATA=wages[tst,]
  
  fm0=lm(Wage~1,data=TRN.DATA) # our 'baseline' model
  fmA=lm(Wage~.,data=TRN.DATA) # note: Wage~. means regress Wage on all the other variables in 'data'
  yHat0=predict(fm0,newdata=TST.DATA)
  yHatA=predict(fmA,newdata=TST.DATA)

  PRSS0=sum((TST.DATA$Wage-yHat0)^2)
  PRSSA=sum((TST.DATA$Wage-yHatA)^2)
  (R2.tst=(PRSS0-PRSSA)/PRSS0)

  # R-sq. in the training sample
  trnSS0=sum(residuals(fm0)^2)
  trnSSA=sum(residuals(fmA)^2)
  (R2.trn= (trnSS0-trnSSA)/trnSS0)
  summary(fmA)

```

```{r}

n=nrow(wages)
nTst=100
nRep=1000
R2.TST=rep(NA,nRep)
 
for(i in 1:nRep){
  tst=sample(1:n,size=nTst)
  TRN.DATA=wages[-tst,]
  TST.DATA=wages[tst,]
 
  fm0=lm(Wage~1,data=TRN.DATA) # our 'baseline' model
  fmA=lm(Wage~.,data=TRN.DATA) # note: Wage~. means regress Wage on all the other variables in 'data'
  yHat0=predict(fm0,newdata=TST.DATA)
  yHatA=predict(fmA,newdata=TST.DATA)

  PRSS0=sum((TST.DATA$Wage-yHat0)^2)
  PRSSA=sum((TST.DATA$Wage-yHatA)^2)
  R2.TST[i]=(PRSS0-PRSSA)/PRSS0
}
 
hist(R2.TST,30);abline(v=quantile(R2.TST,prob=c(.025,.5,.975)),col=2,lwd=2,lty=2)

```

```{r}

n=nrow(wages)
nFolds=5
folds=rep(1:nFolds,ceiling(n/nFolds))[1:n] # this gives approximately balanced counts per fold
R2.TST=rep(NA,nFolds)
 
for(i in 1:nFolds){
  folds=sample(folds, size=n,replace=F) # randomizing the fold assigment
  tst=which(folds==i)
  TRN.DATA=wages[-tst,]
  TST.DATA=wages[tst,]
 
  fm0=lm(Wage~1,data=TRN.DATA) # our 'baseline' model
  fmA=lm(Wage~.,data=TRN.DATA) # note: Wage~. means regress Wage on all the other variables in 'data'
  yHat0=predict(fm0,newdata=TST.DATA)
  yHatA=predict(fmA,newdata=TST.DATA)

  PRSS0=sum((TST.DATA$Wage-yHat0)^2)
  PRSSA=sum((TST.DATA$Wage-yHatA)^2)
  R2.TST[i]=(PRSS0-PRSSA)/PRSS0
}
R2.TST 

```

### Model Comparison using AIC/BIC/Adjusted R-2 and out-of-sample prediction R-sq.



Consider these two competing hypotheses:   H1: `Wage~Sex+Education+Experience`, H2: `Wage~.`


  - Fit the two models to the full data set, obtain R-sq., adjusted R-sq., AIC, BIC and a p-value from an F-test.
  
```{r}

  wages=read.table('wage.txt',header=T)
  n=nrow(wages)
  nTst=100
  set.seed(195021) 
  tst=sample(1:n,size=nTst)
  TRN.DATA=wages[-tst,]
  TST.DATA=wages[tst,]
  fm0=lm(Wage~1, data = TRN.DATA) # our 'baseline' model
  fm1=lm(Wage~Sex+Education+Experience,data=TRN.DATA)
  fm2=lm(Wage~.,data=TRN.DATA) # note: Wage~. means regress Wage on all the other variables in 'data'
  yHat0=predict(fm0,newdata=TST.DATA)
  yHat1=predict(fm1,newdata=TST.DATA)
  yHat2=predict(fm2,newdata=TST.DATA)

  PRSS0=sum((TST.DATA$Wage-yHat0)^2)
  PRSS1=sum((TST.DATA$Wage-yHat1)^2)
  PRSS2=sum((TST.DATA$Wage-yHat2)^2)
  (R2.tst1=(PRSS0-PRSS1)/PRSS0)
  (R2.tst2=(PRSS0-PRSS2)/PRSS0)

  # R-sq. in the training sample
  trnSS0=sum(residuals(fm0)^2)
  trnSS1=sum(residuals(fm1)^2)
  trnSS2=sum(residuals(fm2)^2)
  (R2.trn1= (trnSS0-trnSS1)/trnSS0)
  (R2.trn2= (trnSS0-trnSS2)/trnSS0)
  R2.trn1=summary(fm1)$r.squared
  R2.adj.trn1=summary(fm1)$adj.r.squared
  R2.trn2=summary(fm2)$r.squared
  R2.adj.trn2=summary(fm2)$adj.r.squared
  
  AIC(fm1)
  BIC(fm1)
  AIC(fm2)
  BIC(fm2)
  
  anova(fm1,fm2)

```
- Conduct 1000 training-testing evaluations (nTesting=150) to estimate prediciton R-sq. for H1 and H2.
- Report a table with AIC,BIC,Training R-sq., Training adj-Rsq. and prediction r-sq. for each of the models.
- Which model do you choose? Why?
```{r}


n=nrow(wages)
nTst=150
nRep=1000
R2.TST1=rep(NA,nRep)
R2.TST2=rep(NA,nRep)

for(i in 1:nRep){
  tst=sample(1:n,size=nTst)
  TRN.DATA=wages[-tst,]
  TST.DATA=wages[tst,]
 
  fm0=lm(Wage~1,data=TRN.DATA) # our 'baseline' model
  fmA=lm(Wage~Sex+Education+Experience,data=TRN.DATA) # note: Wage~. means regress Wage on all the other variables in 'data'
  yHat0=predict(fm0,newdata=TST.DATA)
  yHatA=predict(fmA,newdata=TST.DATA)

  PRSS0=sum((TST.DATA$Wage-yHat0)^2)
  PRSSA=sum((TST.DATA$Wage-yHatA)^2)
  R2.TST1[i]=(PRSS0-PRSSA)/PRSS0
}

hist(R2.TST1,30);abline(v=quantile(R2.TST1,prob=c(.025,.5,.975)),col=2,lwd=2,lty=2)

for(i in 1:nRep){
  tst=sample(1:n,size=nTst)
  TRN.DATA=wages[-tst,]
  TST.DATA=wages[tst,]
 
  fm0=lm(Wage~1,data=TRN.DATA) # our 'baseline' model
  fmA=lm(Wage~.,data=TRN.DATA) # note: Wage~. means regress Wage on all the other variables in 'data'
  yHat0=predict(fm0,newdata=TST.DATA)
  yHatA=predict(fmA,newdata=TST.DATA)

  PRSS0=sum((TST.DATA$Wage-yHat0)^2)
  PRSSA=sum((TST.DATA$Wage-yHatA)^2)
  R2.TST2[i]=(PRSS0-PRSSA)/PRSS0
}
 
hist(R2.TST2,30);abline(v=quantile(R2.TST2,prob=c(.025,.5,.975)),col=2,lwd=2,lty=2)

res <- cbind(c(mean(R2.TST1),mean(R2.TST2)), c(AIC(fm1), AIC(fm2)), c(BIC(fm1), BIC(fm2)), c(R2.trn1, R2.trn2), c(R2.adj.trn1, R2.adj.trn2))

colnames(res) <- c("pred r-sq", "AIC", "BIC", "train r-sq", "train adj. r-sq")

res

mean(R2.TST1)
mean(R2.TST2)

```