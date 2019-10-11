---
title: "Practice exam 1"
author: "Benjamin Smith"
date: "10/9/2019"
---

```{r}

# load crab data from the appropriate file
# this assumes the file is in the current working directory

crab <- read.table('crab.txt') 

require(MASS)

# look at the column names of birthwt and make sure it loaded correctly
head(birthwt)

# attach the birthwt data easy access. no need to use 'birthwt$' every time now. 
# I do this for better readability
attach(birthwt)

# define bwt2 as a binary variable 
bwt2 <- ifelse(bwt<2400, 1, 0)

# alternate form:
# bwt2 <- as.numeric(bwt<2400)
# because changing a bolean to numeric is T-> 1 and F -> 0

glm(bwt2~ age + race + smoke, family = binomial(link = logit))

NegLoglik <- function(X,y,b){
  eta <- X %*% b
  p=exp(eta)/(1+exp(eta))
  Loglik <- sum(ifelse(y==1, log(p), log(1-p)))
  return(-Loglik)
}

y = bwt2

X = cbind(1, age, race, smoke)

b.ini = c(0,0,0,0)
optim(par=b.ini, fn=NegLoglik, X=X, y=y)

detach(birthwt)

```


```{r}
n = 1

while (n<=6){
  print((-1/n)^n)
  n = n+1
}

```


```{r}

head(crab)

attach(crab)

PosiNegLoglik <- function(X,y,b){
  loglambda <- exp(X %*% b)
  Loglik <- sum(y*-exp(lambda))
  return(-Loglik)
}

y = V6 #V6 in this data is the column that contains integer values from 1 to ~15

X = cbind(1, V4, V5 ) # in this case, V4 and V5 are the only non-integer comumns. 

b.ini = c(0,0,0) # initial beta values for optimization

optim(par=b.ini, fn=PosiNegLoglik, X=X, y=y)


```


```{r}

set.seed(12345)

COXPH_C <- function(x){
  beta <- c(1,1)
  Z <- cbind(runif(1, 0, 10), runif(1, 0, 5))
  t <- exp(-0.5*x*exp(Z%*%beta))
  return(t)
}

COXPH_C(1e-4)

COXPH_C(13)

## changing the seed is a much different result

set.seed(12)

COXPH_C <- function(x){
  beta <- c(1,1)
  Z <- cbind(runif(1, 0, 10), runif(1, 0, 5))
  t <- exp(-0.5*x*exp(Z%*%beta))
  return(t)
}

COXPH_C(1e-4)

COXPH_C(13)

```
