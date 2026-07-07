#experiment 9 : classification model
#(a) install and load relevant packages

install.packages("class")
install.packages("caret")
install.packages("ggplot2")
install.packages("pheatmap")

library(class)
library(caret)
library(ggplot2)
library(pheatmap)

#(b) create and preprocess dataset

df<-data.frame(
  id=c(1,2,3,4,5,6,7,8,9,10),
  age=c(19,29,15,21,23,17,16,18,15,20),
  sleep_hrs=c(6,7,4,5,6,7,8,9,6,7),
  study_hrs=c(5,4,4,6,3,2,1,2,4,3),
  play_hrs=c(2,3,8,4,2,5,6,3,4,1),
  gender=c('F','M','M','M','F','F','F','M','M','F'),
  result=c('P','P','P','P','F','F','F','F','P','P')
)
print(df)
df<-subset(df,select=-c(id))
df
df$gender<-as.numeric(factor(df$gender))
df

x_train<-df[1:7,-6]
y_train<-df[1:7,6]
x_test<-df[8:10,-6]
y_test<-df[8:10,6]

predict<-knn(train=x_train,test=x_test,cl=y_train,k=3)
print("predicted results:")
print(predict)

print("actual results:")
print(y_test)


#c
predict<-factor(predict)
y_test<-factor(y_test)
cm<-confusionMatrix(data=predict,reference=y_test)
print(cm)

cm_table<-as.table(cm$table)
pheatmap(as.matrix(cm_table),
         color=colorRampPalette(c("lightblue","darkblue"))(50),
         cluster_rows=FALSE,
         cluster_cols=FALSE,
         main="confusion matrix heatmap",
         display_numbers=TRUE)

accuracy <- as.numeric(cm$overall["Accuracy"])
acc_df <- data.frame(Metric = "Accuracy", Value = accuracy)

ggplot(acc_df, aes(x = Metric, y = Value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("%.2f", Value)), vjust = -0.5, size = 5) +
  ylim(0, 1) +
  ggtitle("Model Accuracy") +
  theme_minimal()
