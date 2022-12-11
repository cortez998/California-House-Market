library(moments)
#import data
y = read.csv("C:/Users/mediaw/Desktop/BI/Data science for finance/Project/y.csv", header = TRUE, sep = ",")
x = read.csv("C:/Users/mediaw/Desktop/BI/Data science for finance/Project/x.csv", header = TRUE, sep = ",")
#define column names
col_names = c("MedianIncome", "HouseAge", "AvgRoom", "AvgBdrm", "Population", "AvgOccup", "Latitude", "Longitude")

summary(x)

attach(x)

par(mfrow=c(2,4))

#create histograms
hist(x$MedianIncome, 
     main="Histogram for MedianIncome in tens of thousands of US Dollar", 
     xlab="MedianIncome", 
     border="black", 
     col="blue",
     las=1,
     sub = paste("Skew:",round(skewness(x$MedianIncome), 3)))

hist(x$HouseAge, 
     main="Histogram for HouseAge", 
     xlab="HouseAge", 
     border="black", 
     col="blue",
     las=1,
     sub = paste("Skew:",round(skewness(x$HouseAge), 3)))

hist(x$AvgRoom, 
     main="Histogram for AvgRoom", 
     xlab="AvgRoom", 
     border="black", 
     col="blue",
     las=1,
     sub = paste("Skew:",round(skewness(x$AvgRoom), 3)))

hist(x$AvgBdrm, 
     main="Histogram for AvgBdrm", 
     xlab="AvgBdrm", 
     border="black", 
     col="blue",
     las=1,
     sub = paste("Skew:",round(skewness(x$AvgBdrm), 3)))

hist(x$Population , 
     main="Histogram for Population", 
     xlab="Population", 
     border="black", 
     col="blue",
     las=1,
     sub = paste("Skew:",round(skewness(x$Population), 3)))

hist(x$AvgOccup, 
     main="Histogram for AvgOccup", 
     xlab="AvgOccup", 
     border="black", 
     col="blue",
     las=1,
     sub = paste("Skew:",round(skewness(x$AvgOccup), 3)))

hist(x$Latitude, 
     main="Histogram for Latitude", 
     xlab="Latitude", 
     border="black", 
     col="blue",
     las=1,
     xlim = c(32,42),
     sub = paste("Skew:",round(skewness(x$Latitude), 3)))

hist(x$Longitude, 
     main="Histogram for Longitude", 
     xlab="Longitude", 
     border="black", 
     col="blue",
     las=1,
     sub = paste("Skew:",round(skewness(x$Longitude), 3)))

detach(x)


#removing outliers (leverage points) in x and y
df = cbind(y, x)
#Q1 = quantile(df[, "MedianIncome"])[1]

df2 = df
col_names = c("AvgRoom", "AvgBdrm", "AvgOccup", "Population")
for (i in col_names){
  Q1 = quantile(df[, i])[2]
  Q3 = quantile(df[, i])[4]
  IQR = Q3 - Q1
  lower_bound = Q1 - 1.5 * IQR
  upper_bound = Q3 + 1.5 * IQR
  df2 = df2[df2[i] <= upper_bound & df2[i] >= lower_bound,]
}

df2

attach(df2)

hist(df2$MedianIncome, 
     main="Histogram for MedianIncome in tens of thousands of US Dollar", 
     xlab="MedianIncome", 
     border="black", 
     col="blue",
     las=1,
     sub = paste("Skew:",round(skewness(df2$MedianIncome), 3)))

hist(df2$HouseAge, 
     main="Histogram for HouseAge", 
     xlab="HouseAge", 
     border="black", 
     col="blue",
     las=1,
     sub = paste("Skew:",round(skewness(df2$HouseAge), 3)))

hist(df2$AvgRoom, 
     main="Histogram for AvgRoom", 
     xlab="AvgRoom", 
     border="black", 
     col="blue",
     las=1,
     sub = paste("Skew:",round(skewness(df2$AvgRoom), 3)))

hist(df2$AvgBdrm, 
     main="Histogram for AvgBdrm", 
     xlab="AvgBdrm", 
     border="black", 
     col="blue",
     las=1,
     sub = paste("Skew:",round(skewness(df2$AvgBdrm), 3)))

hist(df2$Population , 
     main="Histogram for Population", 
     xlab="Population", 
     border="black", 
     col="blue",
     las=1,
     sub = paste("Skew:",round(skewness(df2$Population), 3)))

hist(df2$AvgOccup, 
     main="Histogram for AvgOccup", 
     xlab="AvgOccup", 
     border="black", 
     col="blue",
     las=1,
     sub = paste("Skew:",round(skewness(df2$AvgOccup), 3)))

hist(df2$Latitude, 
     main="Histogram for Latitude", 
     xlab="Latitude", 
     border="black", 
     col="blue",
     las=1,
     xlim = c(32,42),
     sub = paste("Skew:",round(skewness(df2$Latitude), 3)))

hist(df2$Longitude, 
     main="Histogram for Longitude", 
     xlab="Longitude", 
     border="black", 
     col="blue",
     las=1,
     sub = paste("Skew:",round(skewness(df2$Longitude), 3)))

rownames(df2) <- seq(length=nrow(df2))
df2

#write.csv(df2, "/Users/dominic/Dropbox/Data Science For Finance/data.csv")




