library(survival)
library(coin)
# 导入例题19-02的数据文件
data1902 <- read.csv("https://raw.githubusercontent.com/20170505a/raw_data/master/data_szq_1902.csv",sep=",")
str(data1902) # 查看数据结构

data1902$group <-as.factor(data1902$group) #将分组转化为因子类型的数据
str(data1902)

Surv(data1902$t,data1902$status==1) 
#此语句的意思是创建一个生存分析对象；
#status==1表示完整数据，0为删失数据；
# 其中"+"是删失的数据，表示病人仍生存或失访

data.fit <- survfit(Surv(t,status==1)~group,data=data1902) 
summary(data.fit)
# 查看分析结果，group=1表示甲组，2表示乙组

survdiff(Surv(t,status==1)~group,data=data1902)
# 比较两条生存曲线的差异，用的是Log-rank检验

plot(data.fit,col=c("red","blue"),lwd=1) #绘图
legend("topright",legend=c("甲组","乙组"),col=c("red","blue"),lty=1,cex=0.8,adj=-0.7,bty="o") 
#加图例

plot(data.fit,col=c("red","blue"),lwd=1) #绘图
legend("topright",legend=c("甲组","乙组"),col=c("red","blue"),lty=1,cex=0.8,adj=-0.7,bty="o") 
#加图例 
abline(h=0.5)

a <- which(data1902$group==1)
b <- which(data1902$group==2)
data1902[a]
data1902[b]
median_a <- Surv(data1902[a,]$t,data1902[a,]$status==1)
median_b <- Surv(data1902[b,]$t,data1902[b,]$status==1)
summary(median_a)
summary(median_b)
