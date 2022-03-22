library(MSGARCH)
library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library(xts)
library(dplyr)

getSymbols('IWV', src = 'yahoo', from = '2008-01-01', to = ' 2021-12-31')

Close = newIWV$Close
predvol = newIWV$predvolsavgML
predprob = newIWV$probML

IWV$IWV.predvol = predvol[1:3525]
IWV$IWV.predprob = predprob[1:3525]


#09-21
IWV1 = IWV[254:3525]
#18-21
IWV_test = IWV[2519:3525]
#09-18
IWV_train = IWV[254:2518]
#12-17
IWV_train2 = IWV[1010:2518]

data = IWV_train2


n = length(data$Close)




#Indicator:
sma5 <- SMA(data$IWV.Close, n = 5)
sma20 <- SMA(data$IWV.Close,n = 20)

##Prob quantile
prob =data$IWV.predprob
quantile = quantile(prob, c(0.48, 0.9))
quantile = as.numeric(quantile)

##vol quantile
voli = data$IWV.predvol
quantile_v = quantile(voli, c(0.7, 0.95))
quantile_v = as.numeric(quantile_v)

##Realized Vol
price = data$IWV.Close
logr = diff(log(Cl(data)))
rv = runSD(logr,n=30)
rv1 = diff(log(rv))
sum(is.na(rv1))

plot(rvn)
plot(rv1)
sma_rv = SMA(rv1, n=5)
plot(sma_rv)

#RVX as IV
getSymbols('RVX', src = 'yahoo', from = '2012-01-01', to = ' 2021-12-31')

iv_train = RVX$RVX.Close["2012-01-01/2017-12-31"]
iv_test = RVX$RVX.Close["2018-01-01/2021-12-31"]

#replace missing with previous
sum(is.na(iv_train))
sum(is.na(iv_test))
iv_test = na.locf(iv_test)
sum(is.na(iv_test))

#rate of change
iv1 = diff(log(Cl(iv_test)))
ivn = (iv1 - mean(iv1)) / sd(iv1)
iv2 = diff(iv1)
sma_iv = SMA(iv1, n=5)
plot(sma_iv)
plot(iv_train)
plot(iv1)
plot(iv2)

##iv quantile
qiv = quantile(iv_train, 0.9)

##Rolling quantile
n = length(voli)

low = 0.85
high = 0.95
period = 50

ql = c(0)
qh = c(0)
for (i in (period + 1):n){
  new_l = quantile(voli[i-period:i], low)
  new_h = quantile(voli[i-period:i], high)
  ql <- c(ql, as.numeric(new_l))
  qh <- c(qh, as.numeric(new_h))
}
zeros = replicate((period-1), NA)
ql = c(zeros, ql)
qh = c(zeros, qh)

##rolling qquantile for iv

low = 0.85
high = 0.95
periodv = 30

qlv = c(0)
qhv = c(0)
for (i in (period + 1):n){
  new_l = quantile(iv_test[i-periodv:i], low)
  new_h = quantile(iv_test[i-periodv:i], high)
  qlv <- c(ql, as.numeric(new_l))
  qhv <- c(qh, as.numeric(new_h))
}
zeros = replicate((period-1), NA)
qlv = c(zeros, qlv)
qhv = c(zeros, qhv)



#Strategy
voli_test = Lag(
  ifelse( data$IWV.predprob < 0.8 , 1, 
          ifelse(  data$IWV.predprob > 0.9 & sma5 < sma20, -1, 0)) 
)

#create strategy
voli_start = ifelse(voli_test > 1, 0, 1)
for (i in 1 : n) {
  voli_start[i] <- ifelse(voli_test[i] == 1,1,
                          ifelse(voli_test[i] == -1,-1, 
                                 ifelse(voli_test[i] == 0, 0 ,voli_start[i-1])))
}

voli_start[is.na(voli_start)] = 1
voli_stratcomp = cbind(data$IWV.Close,data$IWV.predvol,voli_test,voli_start)
colnames(voli_stratcomp) = c('close', 'PV', 'signal', 'position')

#back testing
ret_IWM = diff(log(Cl(data)))
benchmark_IWM = ret_IWM
cv_IWM_r = ret_IWM*voli_start

cv_IWM_comp <- cbind(cv_IWM_r, benchmark_IWM)
colnames(cv_IWM_comp) <- c('Strategy Return','IWM Benchmark')
charts.PerformanceSummary(cv_IWM_comp, main = 'IWV State Probability Performance')
sma_IWM_comp_table <- table.AnnualizedReturns(cv_IWM_comp)

sma_IWM_comp_table
