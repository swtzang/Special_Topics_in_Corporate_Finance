# https://freakonometrics.hypotheses.org/12729
set.seed(1234)

E=rnorm(240)
X=cumsum(E)
plot(X,type="l")

# Dickey-Fuller test
lags=0
z=diff(X)
n=length(z)
z.diff=embed(z, lags+1)[,1]
z.lag.1=X[(lags+1):n]
summary(lm(z.diff~0+z.lag.1 ))

summary(lm(z.diff~0+z.lag.1 ))$coefficients[1,3]

# using library urca to do unit root test----
library(urca)
summary(ur.df(X))
adf.test(X)

df=ur.df(X,type="none",lags=0)
df

qnorm(c(.01,.05,.1)/2)

summary(df)
# Performs the KPSS unit root test, where the Null hypothesis is stationarity
test <- ur.kpss(X)
class(test)
test@teststat
summary(test)

# If the statistics exceed (are greater than) those values (-0.4925>-2.58, 1% critical values), 
# then the series is not stationnary, 
# since we cannot reject the assumption that varph_i-1=0. 
# So we might conclude that there is a unit root.

# using library tseries to do unit root test----
library(tseries)
adf.test(X,k=0)


1-adf.test(X,k=0)$p.value
#
df@testreg$coefficients[4]
#

# Augmented DF test ----
lags=1
z=diff(X)
n=length(z)
z.diff=embed(z, lags+1)[,1]
z.lag.1=X[(lags+1):n]
k=lags+1
z.diff.lag = embed(z, lags+1)[, 2:k]
summary(lm(z.diff~0+z.lag.1+z.diff.lag ))

#
df=ur.df(X,type="none",lags=1)
summary(df)

#
adf.test(X,k=1)



# Georgakopoulos, Quantitative Trading with R----
# chapter 5
# Get SPY data and let's confirm that it is non-stationary
require(quantmod)
getSymbols("SPY")
spy <- SPY$SPY.Adjusted
# Use the default settings
require(urca)
test <- ur.kpss(as.numeric(spy))
# The output is an S4 object
class(test)

# Extract the test statistic
test@teststat
# Look at the critical values
test@cval

#
spy_returns <- na.omit(diff(log(spy)))
plot(spy_returns)
write.csv(spy_returns, "spy_ret.csv")

# KPSS unit root test
test_returns <- ur.kpss(as.numeric(spy_returns))
test_returns@teststat

test_returns@cval
# Dickey-Fuller test
df1=ur.df(spy_returns, type="none", lags=0)
df1
summary(df1)
# Augmented DF test
adf=ur.df(spy_returns,type="none",lags=1)
summary(adf)
#
library(AER)
data("PepperPrice")
plot(PepperPrice, plot.type = "single", col = 1:2)
legend("topleft", c("black", "white"), bty = "n",
           col = 1:2, lty = rep(1,2))
#
library(urca)
library(vars)

data(EuStockMarkets)
Assets <-  as.zoo(EuStockMarkets)
AssetsM <-aggregate(Assets, as.yearmon, tail, 1)
head(AssetsM)
## Applying unit root tests for subsample
AssetsMsub <- window(AssetsM, start=start(AssetsM), end="Jun 1996")
## Levels
ADF <-lapply(AssetsMsub, ur.df, type="drift", selectlags="AIC")
ERS <-lapply(AssetsMsub, ur.ers)

## Differences
DADF <- lapply(diff(AssetsMsub), ur.df, selectlags="AIC")
DERS <- lapply(diff(AssetsMsub), ur.ers)

plot(dnorm, -10, 10, n=1001)
plot(dcauchy, -10, 10, n=1001, col='red', add=TRUE)
#
a <- rnorm(1000, 0, 1) 
b <- rnorm(1000, 5, 2) 
c <- rnorm(1000, 3, 2)
d <- rnorm(1000, -2, 1)
d <- c(a, b, c, d)
df <- data.frame(d, id = as.factor(rep(c(1, 2, 3, 4), each = 1000)))
dim(df)
head(df)
ggplot(df) +
  stat_density(aes(x = d, group = id), position = "stack", geom = "line", show.legend = F, color = "red") +
  stat_density(aes(x = d, linetype = id), position = "identity", geom = "line")
#
set.seed(1234)               # this makes the example reproducible
N     = 10000             # this is how many data you want
probs = c(.95)            # these are *cumulative* probabilities; since they 
                          # necessarily sum to 1, the last would be redundant
dists = runif(N)          # here I'm generating random variates from a uniform
# to select the relevant distribution

# this is where the actual data are generated, it's just some if->then
#   statements, followed by the normal distributions you were interested in
data = vector(length=N)
for(i in 1:N){
  if(dists[i]<probs){
    data[i] = rnorm(1, mean=0, sd=1)
    } else {
    data[i] = rnorm(1, mean=0, sd=4)
  }
}

data
summary(data)
plot(density(data))

# 
gaussmix <- function(nsim,mean_1,mean_2,std_1,std_2,alpha){
  U <- runif(nsim)
  I <- as.numeric(U<alpha)
  y <- I*rnorm(nsim,mean=mean_1,sd=std_1)+
    (1-I)*rnorm(nsim,mean=mean_2,sd=std_2)
  return(y)
}

z1 <- rnorm(10000, 0, 1)
z2 <- gaussmix(10000,0,0,1,4,0.95)
zz <- cbind(z1, z2)
#
ggplot(data.frame(z2), aes(z2)) + 
  geom_histogram(aes(y=..density..), binwidth = 0.1)+
  stat_density(aes(z1), position = "stack", geom = "line", show.legend = F, color = "red")+
  ggtitle("Plot of mixture normal distribution: \n
          (1-X)*N (0, 1)+X*N (0, 16) with X being Bernoulli such that P (X=1)=0.05")+
  xlab("x")+ylab("f(x)")
# overlay two plots 
data <- melt(zz)
ggplot(data, aes(x=value, fill=Var2)) + geom_density(alpha=0.25)
ggplot(data, aes(x=value, fill=Var2)) + geom_density(aes(group=Var2))
  #geom_histogram(aes(y=..density..), binwidth = 0.1)+
  stat_density(aes(zz), position = "stack", geom = "line", show.legend = F, color = "red")+
  ggtitle("Plot of mixture normal distribution: \n
          (1-X)*N (0, 1)+X*N (0, 16) with X being Bernoulli such that P (X=1)=0.05")+
  xlab("x")+ylab("f(x)")





z1_standardized <- (z1-mean(z1))/sqrt(var(z1))
hist(z1_standardized,xlim=c(-10,10),ylim=c(0,500),
     main="Histogram of 95% of N(0,1) and 5% of N(0,36)",
     col="blue",xlab=" ")

ggplot(data.frame(z1_standardized), aes(z1_standardized)) + 
  geom_histogram(aes(y=..density..), binwidth = 0.1)+
  stat_density(aes(z1_standardized), position = "stack", geom = "line", show.legend = F, color = "red")
  

