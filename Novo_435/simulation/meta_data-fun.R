
meta.data<-function(sample){

        treated<-sum(sample$GROUP==1)
        control<-sum(sample$GROUP==0)
        ratio<-control/treated
        n<-treated+control
        
        return<-round(c(n,treated, control, ratio),2)
        
        
        
}