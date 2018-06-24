 library(data.table)
# inbound.dt<-fread("~/Inbound_Orders_for_Hackathon.csv")
# live.dt<-fread("~/Live_Orders.csv")
# dt<-merge(inbound.dt,live.dt)
colnames(inbound.dt)<-c('Load_Port','Eqp_Type','ETD','Dsch_Port','ETA_Dsch_Port','Final_Dest','ETA_Final_Dest','Msr','MSR_Unit','Wgt','Wgt_Unit','PO_No','Item_Class','Shipped_Qty','PkgCode','Shipped_Dtl_Qty','PkgCode','Item_Shipped_Wgt','Item_Wgt_Unit','Item_Shipped_Msr','Item_Msr_Unit')
dt<-fread("~/HTR_Group1/dataset170623v3.csv")
colnames(dt)
temp.dt<-dt[,c(1:5,8:10,13,19:20,23:24,27:28,31:32,39:41)][!is.na(prev_ETD_Dte_diff)]
summary(temp.dt)
library(stats)
library(RSQLite)
library(odbc)
library(glmnet)
library(glmnetUtils)
library(opencpu)
library(quantreg)
library(pls)
unique(live.dt[,c('ETD','Secondary Descriptions','Supplier Location')])
final.temp.dt<-temp.dt


set.seed(124)



## 75% of the sample size
smp_size <- floor(0.75 * nrow(final.temp.dt))
train_ind <- sample(seq_len(nrow(final.temp.dt)), size = smp_size)

train <- final.temp.dt[train_ind, ]
test <- final.temp.dt[-train_ind, ]
# 
# smp_size <- floor(0.5 * nrow(temp.test))
# train_ind <- sample(seq_len(nrow(temp.test)), size = smp_size)
# 
# validate <- temp.test[train_ind, ]
# test <- temp.test[-train_ind, ]


f =as.formula(paste0("Sum_Shipped_Msr~factor(Load_Port) + factor(Item_Class)+factor(Load_Port)*ETD_Yr+ factor(Load_Port)*ETD_Day+ factor(Load_Port)*ETD_Mth+ factor(Load_Port)*ETA_Yr+ factor(Load_Port)*ETD_Day_1+ factor(Load_Port)*ETD_Mth_1+ factor(Load_Port)*HK_Prev_diff_ETD+ factor(Load_Port)*HK_Prev_diff_ETA+ factor(Load_Port)*HK_Next_diff_ETD+ factor(Load_Port)*HK_Next_diff_ETA+ factor(Load_Port)*UK_Prev_diff_ETD+ factor(Load_Port)*UK_Prev_diff_ETA+ factor(Load_Port)*UK_Next_diff_ETD+ factor(Load_Port)*UK_Next_diff_ETA+ factor(Load_Port)*prev_Shipped_Msr+ factor(Load_Port)*prev_ETD_Dte_diff+ factor(Load_Port)*Transport_Days+factor(Item_Class)*ETD_Yr+ factor(Item_Class)*ETD_Day+ factor(Item_Class)*ETD_Mth+ factor(Item_Class)*ETA_Yr+ factor(Item_Class)*ETD_Day_1+ factor(Item_Class)*ETD_Mth_1+ factor(Item_Class)*HK_Prev_diff_ETD+ factor(Item_Class)*HK_Prev_diff_ETA+ factor(Item_Class)*HK_Next_diff_ETD+ factor(Item_Class)*HK_Next_diff_ETA+ factor(Item_Class)*UK_Prev_diff_ETD+ factor(Item_Class)*UK_Prev_diff_ETA+ factor(Item_Class)*UK_Next_diff_ETD+ factor(Item_Class)*UK_Next_diff_ETA+ factor(Item_Class)*prev_Shipped_Msr+ factor(Item_Class)*prev_ETD_Dte_diff+ factor(Item_Class)*Transport_Days+",paste(colnames(final.temp.dt)[-c(1,2,9)],collapse = "+")))
#terms = attr(terms.formula(f), "term.labels")

#f = as.formula(sprintf("y ~ %s", paste(terms, collapse="+")))




fit<-cv.glmnet(f,train,use.model.frame = TRUE,alpha=0)
opt_lambda <- fit$lambda
opt_lambda
fit$lambda.min
# {
# y_predicted<-predict(fit,validate,s=fit$lambda.min)
# # step.model <-MASS::stepAIC( lm(Sum_Shipped_Msr~., data = train), direction = "backward", 
# #                       trace = FALSE)
# # summary(step.model)
# # y_predicted <- predict(step.model,test)
# result<-cbind(test,ifelse(y_predicted<0,0,y_predicted))
# # Sum of Squares Total and Error
# y<-test[,Sum_Shipped_Msr]
# sst <- sum((y-mean(y))^2)
# sse <- sum((ifelse(y_predicted<0,0,y_predicted) - mean(y))^2)
# 
# # R squared
# rsq <- 1 - sse / sst
# rsq
# }
# 




y_predicted<-ifelse(predict(fit,test,s=fit$lambda.1se)<0,0,predict(fit,test,s=fit$lambda.1se))
y<-test[,Sum_Shipped_Msr]
sst <- sum((y-mean(y))^2)
sse <- sum((ifelse(y_predicted<0,0,y_predicted) - mean(y))^2)

# R squared
rsq <- 1 - sse / sst
rsq
train<-train[!is.na(Item_Class)]
qreg.model<-hqreg::hqreg(train[,Sum_Shipped_Msr], train[,-c("Sum_Shipped_Msr")], tau = 0.9,method="huber")
pcr_model <- pcr(Sum_Shipped_Msr~., data = train, scale = TRUE, validation = "CV")

step.model <-MASS::stepAIC( lm(Sum_Shipped_Msr~., data = train), direction = "backward",
                      trace = FALSE)
summary(step.model)
y_predicted <- predict(step.model,test)
result<-cbind(test,ifelse(y_predicted<0,0,y_predicted))
# Sum of Squares Total and Error
y<-test[,Sum_Shipped_Msr]
sst <- sum((y-mean(y))^2)
sse <- sum((ifelse(y_predicted<0,0,y_predicted) - mean(y))^2)

# R squared
rsq <- 1 - sse / sst
rsq





live.dt<-fread("~/HTR_Group1/dataset170623v2_live.csv")
colnames(live.dt)<-colnames(dt)
final.live.dt<-unique(live.dt[,c(1:5,8:10,13,19:20,23:24,27:28,31:32,39:41)][!is.na(prev_ETD_Dte_diff)])
colnames(final.live.dt)[1]<"factor(Load_Port)"
result<-cbind(final.live.dt,ifelse(predict(step.model,data=final.live.dt)<0,0,predict(step.model,data=final.live.dt))
              ,ifelse(predict(fit,final.live.dt,s=fit$lambda.1se)<0,0,predict(fit,final.live.dt,s=fit$lambda.1se)))

# result<-cbind(final.live.dt,predict(fit,final.live.dt,s=fit$lambda.1se))
colnames(result)[length(result)-1]<-"predict_sum_mrs_1"
colnames(result)[length(result)]<-"predict_sum_mrs_2"
coef(fit
     )
result[,predict_sum_mrs:=predict_sum_mrs_1*0.8+predict_sum_mrs_2*0.2]
result[,predict_sum_mrs]
# class_map<-fread("~/class_map.csv")[,1:2]
# colnames(class_map)
# temp.live.dt<-merge(live.dt,class_map)[,c(3,5,11)]
# setnames(temp.live.dt,"Supplier Location","Load_Port")
# transport.dt<-final.temp.dt[,mean(Transport_Days),by="Load_Port"]
# colnames(transport.dt)[2]<-"Transport_Days"
# live.dt.make<-merge(temp.live.dt,transport.dt)
# fwrite(final.temp.dt,"~/live_make.csv")
