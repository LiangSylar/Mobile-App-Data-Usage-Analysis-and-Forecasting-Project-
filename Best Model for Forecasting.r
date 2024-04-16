
# loading data 
f = load("E:/1大四下学期课程/STA4003 Time Series/Project/AppData1.RData")
data_raw = data.project
data_raw$date = as.Date(data_raw$date, "%Y/%m/%d")

data2 = data
# compute the difference for all non-sta. ts (except E)
data3 = data.frame('A'=diff(data2$A), 'B'=diff(data2$B), 'C'=diff(data2$C),
                   'D'=diff(data2$D), 'E'=diff(data2$E), 'F'=diff(data2$C),
                   'G'=diff(data2$G), 'H'=diff(data2$H), 'O'=diff(data2$O)) 

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
  # ( diff_c[n+59])
  # print( diff_c[n+58])
  # print( x_c[n+58])
  return (x_c) # include both train and estimated data 
}

# This function computes the MSE.
MSE_total <- function(fore, true) {
  # fore and true are dataframe of the same dim. 
  # each col of the df express the data for one app;
  # first col is date. 
  RMSE = 0 
  for (i in 2:10) {
    RMSE = RMSE + (fore[i]-true[i])^2
  }
  return (sum(RMSE)/(60*9))
}


# Create a df that saves all estimated data for 9 apps 
fore_9 = data.frame(matrix(ncol=10, nrow=60))
colnames(fore_9) = names(data_test)
fore_9$date = data_test$date

# App A 
x_t = data$A; diff_t = data3$A; x_c = data_raw$A; app = 2
# (regr = ar.ols(diff_t, order=10, demean=FALSE, intercept=TRUE))
(regr = ar.yw(diff_t, order=10, demean=FALSE, intecept=TRUE))
regr$asy.se.coef # standard errors of estimate (??)
diff_fore = predict(regr, n.ahead=60) # this is only estimated diff
fore = recoverEstimation(diff_fore$pred, x_t, diff_t)
fore = ts(fore) 
fore_9[app] = fore[92:151]

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

mse=(MSE_total(fore_9, data_test)) # 1005.691
cat("mse is", mse)
