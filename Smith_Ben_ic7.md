


Using the [wages](https://github.com/gdlc/STAT_COMP/blob/master/wages.txt) datatset generate a plot of wages versus income, by sex, and att 95% confidence bands to each of the regressions,.

**Reading the data**

```{r}

## Reading the data set
wages <- read.table("wages.txt", header = T, stringsAsFactors = F)

  # Pediction equation
   ED=6:18
  Z=cbind(1,ED,0,0,0,0,0,4,0) # male, north, not married, non-union, white, 4 yr of experience
  head(Z)

nRep = 50000
  
yHat <- matrix(NA, nRep, length(ED))
  
for (i in 1:nRep){
  set.seed(19700101+i)
  TMP <- wages[sample(1:nrow(wages), size = nrow(wages), replace = T),]
  y = TMP$Wage
  X = TMP[,1:ncol(TMP)-1]
  fm <- lsfit(y = y, x = X, intercept = T)
  yHat[i,]=Z%*%coef(fm)
  
}
  
plot(colMeans(yHat)~ED, type = "l", ylim = c(-7,25))
lines((sd(yHat)*1.96+colMeans(yHat))~ED, col = 2)
lines((-sd(yHat)*1.96+colMeans(yHat))~ED, col = 2)

library(plotrix)
plotCI(y = colMeans(yHat), x = ED, ui = sd(yHat)*1.96+colMeans(yHat), li = -sd(yHat)*1.96+colMeans(yHat))

```


**TASK**
   - Generate 5000 bootstrap samples of yHat (store them in a matrix)
   - For each value of ED, estimate a 95% CI for yHat
   - plot the predicted equation (suing fm0, see code above)
   - add, to each of the predicted point a vertical line with the estimated 95% CI 

