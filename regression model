#6
library(foreign)
library(MASS)

require(foreign)
require(MASS)

url<-"http://stats.idre.ucla.edu/stat/data/binary.csv"
mydata<-read.csv(url)
mydata
head(mydata)
str(mydata)
mydata$rank<-factor(mydata$rank)
mydata$rank
model<-glm(admit~gre+gpa+rank,data=mydata,family=binomial())
summary(model)
null_model<-glm(admit~1,data=mydata,family=binomial())
anova(null_model,model,test="Chisq")
pred_prob<-predict(model,type="response")
pred_class<-ifelse(pred_prob>0.5,1,0)
table(Predicted=pred_class,Actual=mydata$admit)
accuracy<-mean(pred_class==mydata$admit)
accuracy
