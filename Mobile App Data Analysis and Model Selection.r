# Project
 


# loading data 
f = load("E:/STA4003 Time Series/Project/AppData1.RData")
data_raw = data.project
data_raw$date = as.Date(data_raw$date, "%Y/%m/%d")

# check the plots of 9 Apps  
plot.ts(data_raw) 
test_start = as.Date("2020-03-01")
data_test = data_raw[which(data_raw$date >= test_start),]
data = data_raw[which(data_raw$date < test_start),]

# draw ACF plot for 9 Apps (train data) -----------------------------------------------
plot.ts(data)
par(mfrow=c(3,3))
acf(data$A, 150, xlab='A') 
acf(data$B, 150, xlab='B') 
acf(data$C, 150, xlab='C') 
acf(data$D, 150, xlab='D') 
acf(data$E, 150, xlab='E') 
acf(data$F, 150, xlab='F') 
acf(data$G, 150, xlab='G') 
acf(data$H, 150, xlab='H')  
acf(data$O, 150, xlab='O') 


# Handle Non-stationary: logarithm 
data_log = data
for (i in 2:10) {
  data_log[i] = log(data[i])
}
par(mfrow=c(3,3))
acf(data_log$A, 150, xlab='A') 
acf(data_log$B, 150, xlab='B') 
acf(data_log$C, 150, xlab='C') 
acf(data_log$D, 150, xlab='D') 
acf(data_log$E, 150, xlab='E') 
acf(data_log$F, 150, xlab='F') 
acf(data_log$G, 150, xlab='G') 
acf(data_log$H, 150, xlab='H')  
acf(data_log$O, 150, xlab='O') 

plot.ts(data_log[2:10])

# Moving average on train data -------------------------------------------------
par(mfrow=c(3, 3))
k = 30
A1 = filter(data$A, sides = 2, rep(1,k)/k) 
plot(data$A, type="p", xlab='A')
lines(A1)

A1 = filter(data$B, sides = 2, rep(1,k)/k) 
plot(data$B, type="p", xlab='B')
lines(A1)

A1 = filter(data$C, sides = 2, rep(1,k)/k) 
plot(data$C, type="p", xlab='C')
lines(A1)

A1 = filter(data$D, sides = 2, rep(1,k)/k) 
plot(data$D, type="p", xlab='D')
lines(A1)

A1 = filter(data$E, sides = 2, rep(1,k)/k) 
plot(data$E, type="p", xlab='E')
lines(A1)

A1 = filter(data$F, sides = 2, rep(1,k)/k) 
plot(data$F, type="p", xlab='F')
lines(A1)

A1 = filter(data$G, sides = 2, rep(1,k)/k) 
plot(data$G, type="p", xlab='G')
lines(A1)

A1 = filter(data$H, sides = 2, rep(1,k)/k) 
plot(data$H, type="p", xlab='H')
lines(A1)

A1 = filter(data$O, sides = 2, rep(1,k)/k) 
plot(data$O, type="p", xlab='O')
lines(A1)


# Differencing model -------------------------------------------
# step1. fit a model yt = beta0 + beta1*sqrt(t) against data2$A
data['t'] = 1:91 

summary(fit <- lm(data$A~data$t))
par(mfrow=c(1,1))
plot(data2$A, type="o")
abline(fit, col='red')

A1 = filter(data2$A, sides = 2, rep(1,k)/k)  
lines(A1, col="blue")

summary(fit2 <- lm(A1~sqrt(time(A1))))
par(mfrow=c(1,1)) 
abline(fit2, col='green')

# check the difference 
data2 = data
par(mfrow=c(2,1))
plot(resid(fit), type='o', main='detrended')
plot(diff(data2$A), type='o', main = 'first difference')

par(mfrow=c(3,1))
acf(data2$A, 91, main='App A')
acf(resid(fit), 91, main='detrended')
acf(diff(data2$A), 91, main="first difference")

# compute the difference for all non-sta. ts (except E)
data3 = data.frame('A'=diff(data2$A), 'B'=diff(data2$B), 'C'=diff(data2$C),
                   'D'=diff(data2$D), 'E'=diff(data2$E), 'F'=diff(data2$C),
                   'G'=diff(data2$G), 'H'=diff(data2$H), 'O'=diff(data2$O)) 

plot.ts(data3, main='Difference data')
colMeans(data3)

# for data3: draw ACF plot to check stationary 
par(mfrow=c(3,3))
acf(data3$A, 150, xlab="A") 
acf(data3$B, 150, xlab="B") 
acf(data3$C, 150, xlab="C") 
acf(data3$D, 150, xlab="D") 
acf(data3$E, 150, xlab="E") 
acf(data3$F, 150, xlab="F") 
acf(data3$G, 150, xlab="G") 
acf(data3$H, 150, xlab="H")  
acf(data3$O, 150, xlab="O") 

# fit AR model for 9 apps separately ------------------------------------------------------------------

# This function revovers estimated difference data to estimated x_t. 
recoverEstimation <- function(diff_estm, x_t, diff_t, n_estm=60) {
# x_t: original data from t=1:n
# diff_t: difference of x_t, from t=1:(n-1); diff[k]=x[k+1]-x[k]
# diff_estm: estimated diff_t, from t=n:(n+m-1), 
#   if there are m x_t head to be estimated.  
    diff_c = c(diff_t, diff_estm)
    n = length(x_t)
    x_c = x_t
    for (m in 1:n_estm) {
      # x_nPm = diff_c[n+m-1]+diff_c[n+m-2]+x_c[n+m-2]
      x_nPm = diff_c[n+m-1]+x_c[n+m-1]
      x_c = c(x_c, x_nPm)
    } 
    return (x_c) # include both train and estimated data 
}

# Create a df that saves all estimated data for 9 apps 
fore_9 = data.frame(matrix(ncol=10, nrow=60))
colnames(fore_9) = names(data_test)
fore_9$date = data_test$date

# Example on App A 
x_t = data$A; diff_t = data3$A; x_c = data_raw$A; app = 2
(regr = ar.ols(diff_t, order=10, demean=FALSE, intercept=TRUE))
# (regr = ar.yw(diff_t, order=10, demean=FALSE, intecept=TRUE))
regr$asy.se.coef # standard errors of estimate (??)
diff_fore = predict(regr, n.ahead=60) # this is only estimated diff
fore = recoverEstimation(diff_fore$pred, x_t, diff_t)
fore = ts(fore)
par(mfrow=c(1,1))
ts.plot(x_c, fore, col=1:2) 
lines(fore+diff_fore$se, lty='dashed',col=4)
lines(fore-diff_fore$se, lty='dashed',col=4)
fore_9[app] = fore[92:151]

# check the estimation 
ts.plot(diff(data_test$A), ts(diff_fore$pred[1:59]), col=1:2) # the difference fits well; 
# but the estimation is strange


# App B
x_t = data$B; diff_t = data3$B; x_c = data_raw$B; app = 3
(regr = ar.ols(diff_t, order=10, demean=FALSE, intercept=TRUE))
regr$asy.se.coef # standard errors of estimate (??)
diff_fore = predict(regr, n.ahead=60) # this is only estimated diff
fore = recoverEstimation(diff_fore$pred, x_t, diff_t)
fore = ts(fore)
fore_9[app] = fore[92:151]
par(mfrow=c(1,1))
ts.plot(x_c, fore, col=1:2) 
lines(fore+diff_fore$se, lty='dashed',col=4)
lines(fore-diff_fore$se, lty='dashed',col=4)

# App C
x_t = data$C; diff_t = data3$C; x_c = data_raw$C; app = 4
(regr = ar.ols(diff_t, order=10, demean=FALSE, intercept=TRUE))
regr$asy.se.coef # standard errors of estimate (??)
diff_fore = predict(regr, n.ahead=60) # this is only estimated diff
fore = recoverEstimation(diff_fore$pred, x_t, diff_t)
fore = ts(fore)
fore_9[app] = fore[92:151]

# App D
x_t = data$D; diff_t = data3$D; x_c = data_raw$D; app = 5
(regr = ar.ols(diff_t, order=10, demean=FALSE, intercept=TRUE))
regr$asy.se.coef # standard errors of estimate (??)
diff_fore = predict(regr, n.ahead=60) # this is only estimated diff
fore = recoverEstimation(diff_fore$pred, x_t, diff_t)
fore = ts(fore)
fore_9[app] = fore[92:151]

# App E
x_t = data$E; diff_t = data3$E; x_c = data_raw$E; app = 6
(regr = ar.ols(diff_t, order=10, demean=FALSE, intercept=TRUE))
regr$asy.se.coef # standard errors of estimate (??)
diff_fore = predict(regr, n.ahead=60) # this is only estimated diff
fore = recoverEstimation(diff_fore$pred, x_t, diff_t)
fore = ts(fore)
fore_9[app] = fore[92:151]

# App F
x_t = data$F; diff_t = data3$F; x_c = data_raw$F; app = 7
(regr = ar.ols(diff_t, order=10, demean=FALSE, intercept=TRUE))
regr$asy.se.coef # standard errors of estimate (??)
diff_fore = predict(regr, n.ahead=60) # this is only estimated diff
fore = recoverEstimation(diff_fore$pred, x_t, diff_t)
fore = ts(fore)
fore_9[app] = fore[92:151]

# App G
x_t = data$G; diff_t = data3$G; x_c = data_raw$G; app = 8
(regr = ar.ols(diff_t, order=10, demean=FALSE, intercept=TRUE))
regr$asy.se.coef # standard errors of estimate (??)
diff_fore = predict(regr, n.ahead=60) # this is only estimated diff
fore = recoverEstimation(diff_fore$pred, x_t, diff_t)
fore = ts(fore)
fore_9[app] = fore[92:151]

# App H
x_t = data$H; diff_t = data3$H; x_c = data_raw$H; app = 9
(regr = ar.ols(diff_t, order=10, demean=FALSE, intercept=TRUE))
regr$asy.se.coef # standard errors of estimate (??)
diff_fore = predict(regr, n.ahead=60) # this is only estimated diff
fore = recoverEstimation(diff_fore$pred, x_t, diff_t)
fore = ts(fore)
fore_9[app] = fore[92:151]

# App O
x_t = data$O; diff_t = data3$O; x_c = data_raw$O; app = 10
(regr = ar.ols(diff_t, order=10, demean=FALSE, intercept=TRUE))
regr$asy.se.coef # standard errors of estimate (??)
diff_fore = predict(regr, n.ahead=60) # this is only estimated diff
fore = recoverEstimation(diff_fore$pred, x_t, diff_t)
fore = ts(fore)
fore_9[app] = fore[92:151]

RMSE_total <- function(fore, true) {
# fore and true are dataframe of the same dim. 
# each col of the df express the data for one app;
# first col is date. 
  RMSE = 0 
  for (i in 2:10) {
    RMSE = RMSE + (fore[i]-true[i])^2
  }
  return (sum(RMSE)/(60*9))
}

(RMSE_total(fore_9, data_test)) # 1356.461

# Repeat the AR fitting by YW estimator ------------------------------------------------

# Create a df that saves all estimated data for 9 apps 
fore_9 = data.frame(matrix(ncol=10, nrow=60))
colnames(fore_9) = names(data_test)
fore_9$date = data_test$date

# Example on App A 
x_t = data$A; diff_t = data3$A; x_c = data_raw$A; app = 2
# (regr = ar.ols(diff_t, order=10, demean=FALSE, intercept=TRUE))
(regr = ar.yw(diff_t, order=10, demean=FALSE, intecept=TRUE))
regr$asy.se.coef # standard errors of estimate (??)
diff_fore = predict(regr, n.ahead=60) # this is only estimated diff
fore = recoverEstimation(diff_fore$pred, x_t, diff_t)
fore = ts(fore)
par(mfrow=c(1,1))
ts.plot(x_c, fore, col=1:2) 
lines(fore+diff_fore$se, lty='dashed',col=4)
lines(fore-diff_fore$se, lty='dashed',col=4)
fore_9[app] = fore[92:151]

# check the estimation 
ts.plot(diff(data_test$A), ts(diff_fore$pred[1:59]), col=1:2) # the difference fits well; 
# but the estimation is strange


# App B
x_t = data$B; diff_t = data3$B; x_c = data_raw$B; app = 3
(regr = ar.yw(diff_t, order=10, demean=FALSE, intecept=TRUE))
regr$asy.se.coef # standard errors of estimate (??)
diff_fore = predict(regr, n.ahead=60) # this is only estimated diff
fore = recoverEstimation(diff_fore$pred, x_t, diff_t)
fore = ts(fore)
fore_9[app] = fore[92:151]
par(mfrow=c(1,1))
ts.plot(x_c, fore, col=1:2) 
lines(fore+diff_fore$se, lty='dashed',col=4)
lines(fore-diff_fore$se, lty='dashed',col=4)

# App C
x_t = data$C; diff_t = data3$C; x_c = data_raw$C; app = 4
(regr = ar.yw(diff_t, order=10, demean=FALSE, intecept=TRUE))
regr$asy.se.coef # standard errors of estimate (??)
diff_fore = predict(regr, n.ahead=60) # this is only estimated diff
fore = recoverEstimation(diff_fore$pred, x_t, diff_t)
fore = ts(fore)
fore_9[app] = fore[92:151]

# App D
x_t = data$D; diff_t = data3$D; x_c = data_raw$D; app = 5
(regr = ar.yw(diff_t, order=10, demean=FALSE, intecept=TRUE))
regr$asy.se.coef # standard errors of estimate (??)
diff_fore = predict(regr, n.ahead=60) # this is only estimated diff
fore = recoverEstimation(diff_fore$pred, x_t, diff_t)
fore = ts(fore)
fore_9[app] = fore[92:151]

# App E
x_t = data$E; diff_t = data3$E; x_c = data_raw$E; app = 6
(regr = ar.yw(diff_t, order=10, demean=FALSE, intecept=TRUE))
regr$asy.se.coef # standard errors of estimate (??)
diff_fore = predict(regr, n.ahead=60) # this is only estimated diff
fore = recoverEstimation(diff_fore$pred, x_t, diff_t)
fore = ts(fore)
fore_9[app] = fore[92:151]

# App F
x_t = data$F; diff_t = data3$F; x_c = data_raw$F; app = 7
(regr = ar.yw(diff_t, order=10, demean=FALSE, intecept=TRUE))
regr$asy.se.coef # standard errors of estimate (??)
diff_fore = predict(regr, n.ahead=60) # this is only estimated diff
fore = recoverEstimation(diff_fore$pred, x_t, diff_t)
fore = ts(fore)
fore_9[app] = fore[92:151]

# App G
x_t = data$G; diff_t = data3$G; x_c = data_raw$G; app = 8
(regr = ar.yw(diff_t, order=10, demean=FALSE, intecept=TRUE))
regr$asy.se.coef # standard errors of estimate (??)
diff_fore = predict(regr, n.ahead=60) # this is only estimated diff
fore = recoverEstimation(diff_fore$pred, x_t, diff_t)
fore = ts(fore)
fore_9[app] = fore[92:151]

# App H
x_t = data$H; diff_t = data3$H; x_c = data_raw$H; app = 9
(regr = ar.yw(diff_t, order=10, demean=FALSE, intecept=TRUE))
regr$asy.se.coef # standard errors of estimate (??)
diff_fore = predict(regr, n.ahead=60) # this is only estimated diff
fore = recoverEstimation(diff_fore$pred, x_t, diff_t)
fore = ts(fore)
fore_9[app] = fore[92:151]

# App O
x_t = data$O; diff_t = data3$O; x_c = data_raw$O; app = 10
(regr = ar.yw(diff_t, order=10, demean=FALSE, intecept=TRUE))
regr$asy.se.coef # standard errors of estimate (??)
diff_fore = predict(regr, n.ahead=60) # this is only estimated diff
fore = recoverEstimation(diff_fore$pred, x_t, diff_t)
fore = ts(fore)
fore_9[app] = fore[92:151]

(RMSE_total(fore_9, data_test)) # 1005.691

# Diagnostics: 
k = 2
res = data_test[k] - fore_9[k]
acf(res)
res = data.matrix(res)
hist(res, frequency=FALSE) 
qqnorm(res)
qqline(res) 

# Fit ARMA model -----------------------------------------------------
# for data3: draw PACF plot to check stationary 
par(mfrow=c(3,3))
pacf(data3$A, 150, xlab="A") 
pacf(data3$B, 150, xlab="B") 
pacf(data3$C, 150, xlab="C") 
pacf(data3$D, 150, xlab="D") 
pacf(data3$E, 150, xlab="E") 
pacf(data3$F, 150, xlab="F") 
pacf(data3$G, 150, xlab="G") 
pacf(data3$H, 150, xlab="H")  
pacf(data3$O, 150, xlab="O") 

# Create a df that saves all estimated data for 9 apps 
fore_9 = data.frame(matrix(ncol=10, nrow=60))
colnames(fore_9) = names(data_test)
fore_9$date = data_test$date
p=5; q=2

# App A 
x_t = data$A; diff_t = data3$A; x_c = data_raw$A; app = 2
par(mfrow=c(2,1))
acf(x_t, 48)
pacf(x_t, 48) # Seems an MA model would also explain the model somehow 
(x.fit = arima(diff_t, order=c(p, 0, q)))
diff_fore = predict(x.fit, n.ahead=60) # this is only estimated diff
fore = recoverEstimation(diff_fore$pred, x_t, diff_t)
fore = ts(fore)
# par(mfrow=c(1,1))#
# ts.plot(x_c, fore, col=1:2) 
# lines(fore+diff_fore$se, lty='dashed',col=4)
# lines(fore-diff_fore$se, lty='dashed',col=4)
fore_9['A'] = fore[92:151] 
# check the estimation 
# ts.plot(diff(data_test$A), ts(diff_fore$pred[1:59]), col=1:2) # the difference fits well; 
  
# App B
x_t = data$B; diff_t = data3$B; x_c = data_raw$B; app = 3
(regr = arima(diff_t, order=c(p, 0, q)))
regr$asy.se.coef # standard errors of estimate (??)
diff_fore = predict(regr, n.ahead=60) # this is only estimated diff
fore = recoverEstimation(diff_fore$pred, x_t, diff_t)
fore = ts(fore)
fore_9[app] = fore[91:150] 
par(mfrow=c(1,1))
# ts.plot(x_c, fore, col=1:2) 
# lines(fore+diff_fore$se, lty='dashed',col=4)
# lines(fore-diff_fore$se, lty='dashed',col=4) 

# App C
x_t = data$C; diff_t = data3$C; x_c = data_raw$C; app = 4
(regr = arima(diff_t, order=c(p, 0, q)))
regr$asy.se.coef # standard errors of estimate (??)
diff_fore = predict(regr, n.ahead=60) # this is only estimated diff
fore = recoverEstimation(diff_fore$pred, x_t, diff_t)
fore = ts(fore)
fore_9[app] = fore[92:151] 

# App D
x_t = data$D; diff_t = data3$D; x_c = data_raw$D; app = 5
(regr = arima(diff_t, order=c(p, 0, q)))
regr$asy.se.coef # standard errors of estimate (??)
diff_fore = predict(regr, n.ahead=60) # this is only estimated diff
fore = recoverEstimation(diff_fore$pred, x_t, diff_t)
fore = ts(fore)
fore_9[app] = fore[92:151] 

# App E
x_t = data$E; diff_t = data3$E; x_c = data_raw$E; app = 6
(regr = arima(diff_t, order=c(p, 0, q)))
regr$asy.se.coef # standard errors of estimate (??)
diff_fore = predict(regr, n.ahead=60) # this is only estimated diff
fore = recoverEstimation(diff_fore$pred, x_t, diff_t)
fore = ts(fore)
fore_9[app] = fore[92:151]
# check the estimation  

# App F
x_t = data$F; diff_t = data3$F; x_c = data_raw$F; app = 7
(regr = arima(diff_t, order=c(p, 0, q)))
regr$asy.se.coef # standard errors of estimate (??)
diff_fore = predict(regr, n.ahead=60) # this is only estimated diff
fore = recoverEstimation(diff_fore$pred, x_t, diff_t)
fore = ts(fore)
fore_9[app] = fore[92:151]
# check the estimation 
ts.plot(diff(data_test$B), diff_fore$pred, col=1:2) # the difference fits well; 

# App G
x_t = data$G; diff_t = data3$G; x_c = data_raw$G; app = 8
(regr = arima(diff_t, order=c(p, 0, q)))
regr$asy.se.coef # standard errors of estimate (??)
diff_fore = predict(regr, n.ahead=60) # this is only estimated diff
fore = recoverEstimation(diff_fore$pred, x_t, diff_t)
fore = ts(fore)
fore_9[app] = fore[92:151]

# App H
x_t = data$H; diff_t = data3$H; x_c = data_raw$H; app = 9
(regr = arima(diff_t, order=c(p, 0, q)))
regr$asy.se.coef # standard errors of estimate (??)
diff_fore = predict(regr, n.ahead=60) # this is only estimated diff
fore = recoverEstimation(diff_fore$pred, x_t, diff_t)
fore = ts(fore)
fore_9[app] = fore[92:151]

# App O
x_t = data$O; diff_t = data3$O; x_c = data_raw$O; app = 10
(regr = arima(diff_t, order=c(p, 0, q)))
regr$asy.se.coef # standard errors of estimate (??)
diff_fore = predict(regr, n.ahead=60) # this is only estimated diff
fore = recoverEstimation(diff_fore$pred, x_t, diff_t)
fore = ts(fore)
fore_9[app] = fore[92:151]

# evaluate the model
(RMSE_total(fore_9, data_test)) # 1265.854

 










