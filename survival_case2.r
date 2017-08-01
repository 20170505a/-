library(survival)
library(coin)
data1<- read.csv("https://raw.githubusercontent.com/20170505a/raw_data/master/data_szq_survival_case1.csv",sep=",")
data1$group <-as.factor(data1$group) #将分组转化为因子类型的数据
Surv(data1$time,data1$status==1)
data.fit <- survfit(Surv(data1$t,data1$status==1)~data1$group) 
summary(data.fit)
survdiff(Surv(time,status==1)~group,data=data1) 
plot(data.fit,col=c("red","blue"),lwd=1) #绘图
legend("topright",legend=c("BCG治疗组","药物和BCG治疗组"),col=c("red","blue"),lty=1,cex=0.8,x.intersp=1,bty="o") 
