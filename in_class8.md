---
title: "in class 8"
author: "Benjamin Smith"
date: "11/18/2019"
output: html_document
---

Using the gout produce 95% bootstrap CI for the following:

- Male/Female odds ratio
- B/W odds ratio 
- Probability of developing gout for each of the following cases
      - W/F/55
      - B/F/55
      - W/M/55
      - B/M/55

1. Create a bootstrap sample of the data (e.g., `tmpData`).
2. Fit a logistic regression model to the bootstrap sample (fm=glm(gout2~sex+race+age,data=Y,family='binomial'))
3. From the fitted model calculate the odds and risk probabilities listed above. 
4. Repeat 1-3 5,000 times, each time with a different bootstrap sample. Store the odds and risk probabilities in a matrix or data frame.
5. Compute 95% CI by applying the quantile function to the bootstrap estimates of odds and risk probabilities. 


```{r}

GOUT <- read.table('goutData.txt', header = T)

colnames(GOUT)

GOUT$gout <-  as.numeric(GOUT$gout == 'Y')
GOUT$sex <-  as.numeric(GOUT$sex == 'F')
GOUT$race <-  as.numeric(GOUT$race == 'B')

```

```{r}

nRep = 5000
n = nrow(GOUT)

resMat <- matrix(NA, nRep, ncol(GOUT)-1)
resMat2 <- matrix(NA, nRep, 4)

Z = cbind(1, c(1,1,0,0), 55, c(0,1,0,1))
colnames(Z) <- c("int", "sex", "age", "race")
rownames(Z) <- c("W/F/55", "B/F/55", "W/M/55", "B/M/55")

set.seed(19700101)

i = 1

for (i in 1:nRep){
  TMP <- GOUT[sample(1:nrow(GOUT), size = nrow(GOUT), replace = T),]
  fm <- glm(gout~sex + age + race, data = TMP, family = binomial(link = "logit"))
  resMat[i,] <- exp(summary(fm)$coef[2:ncol(GOUT)-1,1])
  LP <- Z%*%coef(fm)
  resMat2[i,] <- exp(LP)/(1+exp(LP))
}

print(c("M/F odds ratio", "B/W odds ratio"))
colMeans(resMat[,c(1,3)])
print(c("W/F/55", "B/F/55", "W/M/55", "B/M/55"))
colMeans(resMat2)

quantile(resMat[,1], c(0.025, 0.975)) # sex 
quantile(resMat[,3], c(0.025, 0.975)) # race

quantile(resMat2[,1], c(0.025, 0.975)) # W/F/55 
quantile(resMat2[,2], c(0.025, 0.975)) # B/F/55
quantile(resMat2[,3], c(0.025, 0.975)) # W/M/55
quantile(resMat2[,4], c(0.025, 0.975)) # B/M/55

```

