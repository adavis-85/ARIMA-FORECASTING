##install the packages first.

library('reader')
library('ggplot2')
library('forecast')
library('tseries')

attach(TSLA)
attach(TNX)

TSLA<-na.omit(TSLA)
TNX<-na.omit(TNX)

TSLA.INC=ts(TSLA[2])
TSLA=ts(TSLA[1])

GOVT=ts(TNX)

TSLA.INC<-na.omit(TSLA.INC)
#########################################################
##auto plot demo
autoplot(TSLA)+xlab("In Last Year")+ylab("Price Daily")

fit <- auto.arima(TSLA, seasonal=FALSE)

fit

predict(fit,5)

fit %>% forecast(h=10) %>% autoplot(include=95)

#######################################TSLA PERCENTAGE CHANGE#########
autoplot(TSLA.INC)+xlab("In Last Year")+ylab("% CHANGE")

fit <- auto.arima(TSLA.INC, seasonal=FALSE)

fit

predict(fit,5)

fit %>% forecast(h=10) %>% autoplot(include=95)
########################################################

TSLA.INC %>% diff() %>% ggtsdisplay(main="")

##(3,1,1)

(fit <- Arima(TSLA.INC, order=c(3,1,1)))

checkresiduals(fit)

autoplot(forecast(fit))
##pretty much the same as the autoplot.  
##########################################################

##########################################################
##10 YEAR TREASURY FORECAST
##AUTO ARIMA FIRST
##ADD IN THE ACTUAL GRAPH ALSO WITH THE FORECAST TO CHECK ACCURACY

autoplot(GOVT)+xlab("THIS YEAR")+ylab("DAILY RATES")

fit <- auto.arima(GOVT, seasonal=FALSE)

fit

predict(fit,5)

fit %>% forecast(h=10) %>% autoplot(include=95)

##LOOKING AT THE GRAPHS FIRST

GOVT %>% diff() %>% ggtsdisplay(main="")

##(3,1,3)

##arima accuracy check.

fit1<-tbats(GOVT)

seasonal<-!is.null(fit1$seasonal)

seasonal

##IF SO THEN fit<-Arima(GOVT,c(p,d,q),seasonal=c(p,d,q))
##MOST LIKELY THE SAME.  


opt_arima<-function(p,d,q,ar){
  
  O=Inf
  letters=rep(0,7)
  ##seasonality check
  fit1<-tbats(ar)
  seasonal<-!is.null(fit1$seasonal)
  seasonal
  
  for (x in 0:p){
    for (y in 0:d){
      for (z in 0:q){
        if (seasonal==FALSE){
          turn<-Arima(ar,order=c(x,y,z))
          turn_const<-Arima(ar,order=c(x,y,z),include.constant=TRUE)
         if (turn$aicc<O){
            O=turn$aicc
            letters[1]=x
            letters[2]=y
            letters[3]=z
            if (turn$aicc>turn_const$aicc){
              turn=turn_const
              letters[7]=turn$coef[5]
            }
            print(O)
        }else{
          ##make this part a seasonal brute force.
          for (a in 0:p){
            for (b in 0:d){
              for (c in 0:q){
          turn<-Arima(ar,order=c(x,y,z),seasonal=c(a,b,c))
          turn_const<-Arima(ar,order=c(x,y,z),include.constant=TRUE)
           if (turn$aicc<O){
            O=turn$aicc
            letters[1]=x
            letters[2]=y
            letters[3]=z
            letters[4]=a
            letters[5]=b
            letters[6]=c
            if (turn$aicc>turn_const$aicc){
              turn=turn_const
              letters[7]=turn$coef[5]
            }
            print(O)
          }
              }
            }
          }
        
      }
    }
  }
    }
  
  if (seasonal==FALSE){
  return(c(letters[1:3],letters[7],O))
  }else
    return(c(letters,c))
}
}

trying_this=opt_arima(5,2,5,TSLA)

trying_this

model<-Arima(TSLA,order=c(trying_this[1],trying_this[2],trying_this[3]))
autoplot(forecast(model))+xlab("OPT TSLA")+ylab("PRICE")

opt=opt_arima(5,2,5,GOVT)

opt

model<-Arima(GOVT,order=c(opt[1],opt[2],opt[3]),include.constant = TRUE)

autoplot(forecast(model))+xlab("OPT 10YR")+ylab("DAILY RATES")

trying_this=opt_arima(5,2,5,TSLA.INC)

trying_this

model<-Arima(TSLA.INC,order=c(trying_this[1],trying_this[2],trying_this[3]))
autoplot(forecast(model))+xlab("THIS YEAR")+ylab("INCREASE/DECREASE")


