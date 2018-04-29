# install.packages("forecast")
# install.packages("glmx")
# install.packages("readxl")
library(readxl)
library(glmx)
library(forecast)
attrib<-read_excel("E:\\R\\simplilearn datascience course\\APR16-MAY3\\Project\\Projects for Submission\\Retail\\Retail\\Attribute DataSet.xlsx",sheet="Sheet1")
date = seq(as.Date("2013/8/29"), as.Date("2013/10/12"), "2 days")
dress<-read_excel("E:\\R\\simplilearn datascience course\\APR16-MAY3\\Project\\Projects for Submission\\Retail\\Retail\\Extra files\\Dress Sales _DR.xlsx",sheet="Sheet2")
dress_df<-as.data.frame(dress)
data<-dress_df[,-501]
data["new.col"]<-date
data[is.na(data)] <- 0
data_mani <- ts(data$`1006032852`)
fit <- HoltWinters(data_mani, beta=FALSE, gamma=FALSE)
Model1 <- glm(Recommendation ~ Style + Price + Rating + Size + Season + NeckLine + SleeveLength + waiseline + Material + FabricType + Decoration + PatternType,data = attrib,family = binomial())
x<-ts(fit)
accuracy(x)
forecast(fit, 3)
plot(forecast(fit, 3))

install.packages("xlsx")
library(readxl)
library(foreign)

library(readxl)
library(glmx)
library(forecast)

dress<-read_excel("E:\\R\\simplilearn datascience course\\APR16-MAY3\\Project\\Projects for Submission\\Retail\\Retail\\Extra files\\Dress Sales _DR.xlsx",sheet="Sheet2")
data<-as.data.frame(dress)
dim(data)

dress_code_list<-colnames( data )


result<-data.frame(dress_code=numeric(0),firstdata=numeric(0),seconddata=numeric(0),thirddata=numeric(0))

data[is.na(data)] <- 0
data[is.character(data)] <- 0

for(dress_code1 in dress_code_list ){
  data_mani <- ts(data[[dress_code1]])
  fit <- HoltWinters(data_mani, beta=FALSE, gamma=FALSE)
  x<-ts(fit)
  accuracy(x)
  output<-forecast(fit, 3)
  output_df<-as.data.frame(output)
  de<-data.frame(dress_code1,output_df$'Point Forecast'[1],output_df$'Point Forecast'[2],output_df$'Point Forecast'[3])
  result<- data.frame(rbind(as.matrix(result), as.matrix(de)))
}

result


write.csv(result, "E:/MyResults2.csv")

