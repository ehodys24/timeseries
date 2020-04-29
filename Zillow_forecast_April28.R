
# Taking only 3 random zipcodes -------------------------------------------
n<-dim(zillow.forecast)[2]
zillow.all<-zillow.forecast #can add all columns
dim(zillow.all)


l <- list(c(0,1,0))
z <- as.vector(l[1])
o <-  c(0,1,0)
xx <- unlist(l[1],  use.names=FALSE)


# Searches through p,q,d for normal and seasonal components ---------------


generateAllSarimaPlots <-  function(data, isSecondOrder, theTitle) {
  
  listOfSarimaModels <- vector(mode="list", length= 16  + 1)
  index = 1
  
  #firstOrder
  d = 1 

  if(isSecondOrder == 1)
  {
    d=2
  }

  for( p in 0:3 ){
    
    for( q in 0:3 ){
     
      for( P in 0:3 ){
        
        for( Q in 0:3 ){
     
      
      
      listOfSarimaModels[index] <- try(sarima(data,p=p, d = d, q = q, P=P,Q=0,D=0,S=12,Model = TRUE))
      #listOfSarimaModels[index]<-try(Arima(data,order=c(p,d,q),seasonal=list(order=c(P, D=0, Q=0), period=12,method="CSS")))
      title(main=theTitle)
      index = index + 1
        
        } 
      }
    }
    
  }

  return(listOfSarimaModels)
  
}

theZipCodes <- colnames(zillow.forecast)
cols <- c(1:24)
theZipCodes <- theZipCodes[cols]


auto.arima(zillow.all[,18])

#modelsZip1 <- generateAllSarimaPlots(zillow.all[,1], isSecondOrder = 0, theTitle = '1storder - 10003')
#modelsZip1 <- generateAllSarimaPlots(zillow.all[,2], isSecondOrder = 0, theTitle = '1storder - 10011')
#modelsZip1 <- generateAllSarimaPlots(zillow.all[,3], isSecondOrder = 0, theTitle = '1storder - 10013')
#modelsZip1 <- generateAllSarimaPlots(zillow.all[,4], isSecondOrder = 0, theTitle = '1storder - 10014')
#modelsZip1 <- generateAllSarimaPlots(zillow.all[,5], isSecondOrder = 0, theTitle = '1storder - 10021')
#modelsZip1 <- generateAllSarimaPlots(zillow.all[,6], isSecondOrder = 0, theTitle = '1storder - 10022')
#modelsZip1 <- generateAllSarimaPlots(zillow.all[,7], isSecondOrder = 0, theTitle = '1storder - 10023')
#modelsZip1 <- generateAllSarimaPlots(zillow.all[,8], isSecondOrder = 0, theTitle = '1storder - 10025')
#modelsZip1 <- generateAllSarimaPlots(zillow.all[,9], isSecondOrder = 0, theTitle = '1storder - 10028')
#modelsZip1 <- generateAllSarimaPlots(zillow.all[,10], isSecondOrder = 0, theTitle = '1storder - 10036')
#modelsZip1 <- generateAllSarimaPlots(zillow.all[,11], isSecondOrder = 0, theTitle = '1storder - 10128')
#modelsZip1 <- generateAllSarimaPlots(zillow.all[,12], isSecondOrder = 0, theTitle = '1storder - 10303')
#modelsZip1 <- generateAllSarimaPlots(zillow.all[,13], isSecondOrder = 0, theTitle = '1storder - 10304')
#modelsZip1 <- generateAllSarimaPlots(zillow.all[,14], isSecondOrder = 0, theTitle = '1storder - 10305')
#modelsZip1 <- generateAllSarimaPlots(zillow.all[,15], isSecondOrder = 0, theTitle = '1storder - 10306')
#modelsZip1 <- generateAllSarimaPlots(zillow.all[,16], isSecondOrder = 0, theTitle = '1storder - 10308')

#modelsZip1 <- generateAllSarimaPlots(zillow.all[,17], isSecondOrder = 0, theTitle = '1storder - 10309')
#modelsZip1 <- generateAllSarimaPlots(zillow.all[,18], isSecondOrder = 0, theTitle = '1storder - 10312')

#modelsZip1 <- generateAllSarimaPlots(zillow.all[,19], isSecondOrder = 0, theTitle = '1storder - 10314')
#modelsZip1 <- generateAllSarimaPlots(zillow.all[,20], isSecondOrder = 0, theTitle = '1storder - 11215')
#modelsZip1 <- generateAllSarimaPlots(zillow.all[,21], isSecondOrder = 0, theTitle = '1storder - 11217')
#modelsZip1 <- generateAllSarimaPlots(zillow.all[,22], isSecondOrder = 0, theTitle = '1storder - 11231')
#modelsZip1 <- generateAllSarimaPlots(zillow.all[,23], isSecondOrder = 0, theTitle = '1storder - 11234')
modelsZip1 <- generateAllSarimaPlots(zillow.all[,24], isSecondOrder = 0, theTitle = '1storder - 11434')






l<-length(modelsZip1)

getindex<-seq(1,l,1)
aic_list<-rep(0,l)

for (i in 1:length(aic_list))
  {
    
    aic_list[i]<-if(class(modelsZip1[[i]])=="character")
                  {0} 
                  else {getElement(modelsZip1[[i]], "aic")}
    
}


min_aic<-vector()
(sorted<-sort(aic_list))
for(i in 1:length(sorted) ){
  if(sorted[i]!=0){
    min_aic<-(sorted[i])
    break
  }
    
  
}
min_aic

iter.no<-getindex[which(aic_list==min_aic)]
modelsZip1[[iter.no[1]]]

fit.auto<-auto.arima(zillow.all[,3])
summary(fit.auto)



# With 2nd order differencing ---------------------------------------------

modelsZip2 <- generateAllSarimaPlots(zillow.all[,18], isSecondOrder = 1, theTitle = '1storder - 10312')

l<-length(modelsZip2)

getindex<-seq(1,l,1)
aic_list<-rep(0,l)

for (i in 1:length(aic_list))
{
  
  aic_list[i]<-if(class(modelsZip2[[i]])=="character")
  {0} 
  else {getElement(modelsZip2[[i]], "aic")}
  
}

min_aic<-vector()
sorted<-sort(aic_list)
for(i in 1:length(sorted) ){
  if(sorted[i]!=0){
    min_aic<-(sorted[i])
    break
  }
  
  
}
min_aic

iter.no<-getindex[which(aic_list==min_aic)]
modelsZip2[[iter.no[1]]]

auto.arima(zillow.all[,1])



# Best models  -------------------------------------------------------------
ts<-zillow.all[,9]
(fit1 <- Arima(ts, order=c(0,2,3), seasonal=c(0,0,0),include.constant = FALSE,lambda=0))
summary(fit1)
checkresiduals(fit1)  #diagnostics

fit1 %>% forecast(h=36) %>% autoplot()



(fit2 <- Arima(ts, order=c(2,2,3), seasonal=c(1,0,0),include.constant = FALSE,lambda=0))
summary(fit2)
checkresiduals(fit2)  # diagnostics
acf(resid(fit2))

fit2 %>% forecast(h=36) %>% autoplot()






# Multiple zipcodes -------------------------------------------------------


#generateAllSarimaPlots(zillow.forecast[,'10025'], isSecondOrder = 0, theTitle = '5')
theZipCodes <- colnames(zillow.forecast)
cols <- c(1:15,17,19:24)
theZipCodes <- theZipCodes[cols]

#16,18

for (curZip in theZipCodes) {
  generateAllSarimaPlots(zillow.forecast[,curZip], isSecondOrder = 0, theTitle = curZip)
  
}


# Code to run forecasts for all columns in one go -------------------------

batch <- function(data, n_train=120){
  
  lst.names <- c(colnames(data))
  lst <- vector("list", length(lst.names))
  names(lst) <- lst.names    
  
  for( i in 1:ncol(data) ){  
    
    lst[[1]][["train_dates"]] <- data[1:(n_train),1]
    lst[[1]][["test_dates"]] <- data[(n_train+1):nrow(data),1]
    
    est <- auto.arima(data[1:n_train,i])
    fcas <- forecast(est, h=18)$mean
    acc <- accuracy(fcas, data[(n_train+1):nrow(data),i])
    fcas_upd <- data.frame(date=data[(n_train+1):nrow(data),1], forecast=fcas,actual=data[(n_train+1):nrow(data),i])
    
    lst[[i]][["estimates"]] <- est
    lst[[i]][["forecast"]] <- fcas
    lst[[i]][["forecast_f"]] <- fcas_upd
    lst[[i]][["accuracy"]] <- acc
    
    cond1 = diff(range(fcas[1], fcas[length(fcas)])) == 0
    cond2 = acc[,3] >= 0.025
    
    if(cond1|cond2){
      
      mfcas = forecast(ma(data[,i], order=3), h=5)        
      lst[[i]][["moving_average"]] <- mfcas
      
    } else {
      
      est2 <- auto.arima(data[,i])
      fcas2 <- forecast(est, h=5)$mean
      
      lst[[i]][["estimates_full"]] <- est2
      lst[[i]][["forecast_full"]] <- fcas2
      
    }  
  }  
  return(lst)
}

batch(zillow.3)


zillow.3
         
ts<-zillow.3[,1]
plot(ts)         
adf.test(ts)         


acf(ts)
pacf(ts)

adf.test(ts,k=1)
adf.test(ts,k=2)
adf.test(ts,k=3)
adf.test(ts,k=4)
adf.test(ts,k=5)
adf.test(ts,k=6)

library('urca')
test_kpss<-ur.kpss(ts)
summary(test_kpss)

ts %>% stl(s.window='periodic') %>% seasadj() -> ts_nonseasonal
autoplot(ts_nonseasonal)
adf.test(ts_nonseasonal)

ts_nonseasonal %>% diff() %>% ggtsdisplay(main="")

ts_nonseasonal_1<-diff(ts_nonseasonal,differencing=1,lag=1)
adf.test(ts_nonseasonal_1,k=3)

ts_nonseasonal_2<-diff(ts_nonseasonal_1,differencing=1,lag=1)
adf.test(ts_nonseasonal_2)  #diff should be 2

auto.arima(ts)
ts_1<-diff(ts,differencing=1,lag=2)
adf.test(ts_1)

acf(ts_1,lag.max = 15)
pacf(ts_1)  

ts %>% ggtsdisplay(lag.max=40)

ts %>% diff() %>% ggtsdisplay(lag.max=40)
ts %>%  diff(lag=2) %>% adf.test() #d=1,ma=1 sma=1 due to spikes at lag=1 and lag=12 in acf


ts%>% arima(order=c(3,1,3),seasonal = c(0,0,1))%>%residuals()%>%ggtsdisplay()
(fit <- Arima(ts, order=c(0,1,1), seasonal=c(0,0,1),include.constant = FALSE,lambda=0))
summary(fit)
checkresiduals(fit)


ts%>% arima(order=c(2,1,2),seasonal = c(1,0,0))%>%residuals()%>%ggtsdisplay()




(fit <- Arima(ts, order=c(2,1,2), seasonal=c(0,0,1),include.constant = FALSE,lambda=0))
summary(fit)
checkresiduals(fit)

fit %>% forecast(h=36) %>% autoplot()
