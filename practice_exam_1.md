---
title: "Practice exam 1"
author: "Benjamin Smith"
date: "10/9/2019"
---

```{r}

crab <- read.table('crab.txt')

require(MASS)

head(birthwt)

attach(birthwt)

bwt2 <- ifelse(bwt<2400, 1, 0)

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

attach(crab)

PosiNegLoglik <- function(X,y,b){
  lambda <- exp(X %*% b)
  Loglik <- sum(y*log(lambda)-lambda)
  return(-Loglik)
}

y = V6

X = cbind(1, V4, V5 )

b.ini = c(0,0,0)

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