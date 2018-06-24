options(scipen = 999)

"%ni%" = Negate( "%in%" )
y_predicted<-ifelse(y_predicted<0,0,y_predicted)
predict.dt<-as.vector(append(y_predicted[-which(y_predicted<60)]-60,y_predicted[which(y_predicted<60)]))
pool.dt<-append(predict.dt,c(47.38,23.42,35,32.23,32.55))

pool<-length(pool.dt)
container40<-length(which(y_predicted>60))
container20<-0

stop<-0
while(pool>1){
  ##consolidate 40ft container
  local.max<-0
  temp.pool<-c()
  pool<-length(pool.dt)
  for(j in 1:pool){
    pool<-length(pool.dt)
    for(i in 1:pool){
      pool<-length(pool.dt)
       if(((pool.dt[i]+pool.dt[j])<=60)&((pool.dt[i]+pool.dt[j])>28) & (pool.dt[i]+pool.dt[j])>local.max & pool.dt[i]!=pool.dt[j]){
      temp.pool<-c(pool.dt[i],pool.dt[j])
        message(paste(temp.pool,collapse=","))
       
         local.max<-pool.dt[i]+pool.dt[j]
         message(local.max)
       }
       
    }
    
  }
  if(local.max>0){container40<-container40+1
  pool<-pool-2
  pool.dt<-pool.dt[ pool.dt %ni% temp.pool ]
  }
  else{
    stop<-stop+1
  }
  message("pool:")
  #pool.dt<-pool.dttemp.dt
  message(pool.dt)
  
  
  
  
  if(stop>pool){pool<-0}
}
container40




pool<-length(pool.dt)
container20<-length(which(pool.dt>28))
pool.dt<-append(pool.dt[-which(pool.dt<28)]-28,pool.dt[which(pool.dt<28)])
stop<-0
pool<-length(pool.dt)
pool
stop<-0
while(pool>1){
  ##consolidate 40ft container
  local.max<-0
  temp.pool<-c()
  pool<-length(pool.dt)
  for(j in 1:pool){
    pool<-length(pool.dt)
    for(i in 1:pool){
      pool<-length(pool.dt)
      if(((pool.dt[i]+pool.dt[j])<=28)&((pool.dt[i]+pool.dt[j])>0) & (pool.dt[i]+pool.dt[j])>local.max & pool.dt[i]!=pool.dt[j]){
        temp.pool<-c(pool.dt[i],pool.dt[j])
        message(paste(temp.pool,collapse=","))
        
        local.max<-pool.dt[i]+pool.dt[j]
        message(local.max)
      }
      
    }
    
  }
  if(local.max>0){container20<-container20+1
  pool<-pool-2
  pool.dt<-pool.dt[ pool.dt %ni% temp.pool ]
  }
  else{
    stop<-stop+1
  }
  message("pool:")
  #pool.dt<-pool.dttemp.dt
  message(pool.dt)
  
  
  
  
  if(stop>pool){pool<-0}
}
container20




combn(pool.dt,2,function(x){stop<-1
while(stop<0){
  if(sum(x)<28&combn(pool.dt,2,sum)){}
  stop<-stop+1
if(stop>length(x)){stop<-0}
  
  
  }})
  
pool.dt

  
})