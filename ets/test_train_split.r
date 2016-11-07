test_train_slice<- function(data,n=10,prct=0){
    require(dplyr)
    data = tbl_df(data)

    if (prct==0) {
        data_test=data %>% slice((n()-n+1):n())
        data_train=data %>% slice(1:(n()-n))
    } else {
        data_test=data %>% slice(round(n()*prct+1,0):n())
        data_train=data %>% slice(1:round(n()*prct,0))
    }

    list(data_train=data_train,data_test=data_test)
}    