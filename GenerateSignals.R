 CreateData<-function(L,n,output)
 {
     DF<-data.frame()
     for( i in 1:L)
     {
         data<-rnorm(n,0,1)
         params<-FitJohnsonDistribution(0, 2, 0, 20)
         ITYPE<-params["ITYPE"]
         GAMMA<-params["GAMMA"]
         DELTA<-params["DELTA"]
         XLAM<-params["XLAM"]
         XI<-params["XI"]
         dataKurt<-yJohnsonDistribution(data, ITYPE, GAMMA, DELTA, XLAM, XI)
         DF <- rbind(DF, t(dataKurt))
     }
     write.table(DF,output,col.names=FALSE,row.names=FALSE,sep=",")
 }
 
 K=2
 for (i in 1:K)
     CreateData(10,5,paste("Data",i,".txt",sep=""))