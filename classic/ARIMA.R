
############################################### 
# CLASSIC TIME SERIES IMPLEMENTTATION

# simulate Autoregressive model, AP(p)
# ARIMA data simulation
# ARIMA by package

############################################### 


# simulate Autoregressive model, AP(p) ------------------

# AP(p=1) with y_t = 20 +1.2*y_{t-1} + error_t  ------------------

p<-1

c0 <- 18

#  -1< phi< 1 for  stationary
phi<- numeric()
phi[1] <- -0.7

error_t <-  rnorm(mean =0, sd = 1, n = 100)

y<- numeric()

# 
y[1] <- c0 + error_t[1]

# length
len_y= 100


for (ii in 2:len_y) {
  
  y[ii] <- c0 + phi*y[ii-1] + error_t[ii]
  
}


#
plot(y,type = "l", xlab="time", main=TeX("AP(1): $y_{t} = c +\\phi_{1}*y_{t-1} + $ error_t.") ) 





# AP(p=2) with y_t = 8 +1.2*y_{t-1} - 0.7*y_{t-2} + error_t ------------------

p<-2

c0 <- 8

# -1 < phi[2]< 1,   phi[2]+  phi[1] < 1,  phi[2]- phi[1] < 1 
phi<- numeric()
phi[1:p] <- c(1.3, -0.7)

error_t <-  rnorm(mean =0, sd = 1, n = 100)

y<- numeric()

# 
y[1] <- c0 + error_t[1]
y[2] <- c0 + error_t[2]

# length
len_y= 100


for (ii in 3:len_y) {
  
  y[ii] <- c0 + phi[1]*y[ii-1] + phi[2]*y[ii-2] + error_t[ii]
  
}


#
plot(y,type = "l", xlab="time", main=TeX("AP(2): $y_{t} = c +\\phi_{1}*y_{t-1} + \\phi_{2}*y_{t-2} + $ error_t.") ) 



# simulate moving average models  ------------------
# MA(q) with y_t = 20 + error_t + 0.8 *error_{t-1} ------------------

q <- 1

c0 <- 20

error_t <-  rnorm(mean =0, sd = 1, n = 100)

# range (-1,1)
phi=numeric()
phi[1:q] <- 0.8


y<- numeric()
# 
y[1] <- c0 + error_t[1]

# length
len_y= 100


for (ii in 2:len_y) {
  
  y[ii] <- c0 + error_t[ii] + phi[1]* error_t[ii-1] 
  
}


#
plot(y,type = "l", xlab="time", main=TeX("MAP(1): $y_{t} = c +$ error_t $ + \\phi_{1} * error_{t-1}$ .") ) 


# simulate non-seasonal arima models  ------------------
# ARIMA(p,d,q),  p = order of autoregessive, d = degree of first differencing, q= order of the moving average
# ARIMA(p=1,d=0,q=3)
# y_{differenced-series} = c + phi_1 * y_{d,t-1} + error_{t} + theta_{1}*error_{t-1} +theta_{2}*error_{t-2} + theta_{3}*error_{t-3}

y <- arima.sim(n = 100, list(ar = c(0.6), ma = c(-0.3, 0.1, 0.17)), sd = sqrt(0.5))

#
plot(y,type = "l", xlab="time", main=TeX("ARIMA") ) 


# fit the model using forecast package
fit <- auto.arima(y,seasonal=FALSE)
plot(forecast(fit,h=20))



# AR(1) maximum likelihood estimation by hand ---------------------
# AP(p=1) with y_t = phi *y_{t-1} + error_t, where error_t~N(0,sigma^2)
















# 0. ARIMA data simulation ---------------------
#https://stat.ethz.ch/R-manual/R-devel/library/stats/html/arima.sim.html

library(forecast)



# 1. ARIMA ------------------------------------
#https://otexts.com/fpp2/arima-r.html
#https://stats.stackexchange.com/questions/77663/arima-estimation-by-hand

