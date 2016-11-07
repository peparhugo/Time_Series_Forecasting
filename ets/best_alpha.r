require(forecast)

best_alpha = 0
minRSME = 1/0
fcn_best_alpha<-function(ts, start=1,stop=100,by=1,forecast_n=6){
    for (i in seq(start,stop, by=by)){
        fit13 <- HoltWinters(ts[,2], beta=FALSE, alpha=i/100,seasonal="additive")
        if(accuracy(forecast(fit13,forecast_n),data_test$Champagne_Sales)[2,2]<minRSME){
            best_alpha = i
        }

    }
    best_alpha
}    