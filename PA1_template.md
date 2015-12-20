---
title: "Reproducible Research - Peer Assessment 1"
author: "thodk"
date: "12/20/2015"
output: html_document
---


### **Loading and preprocessing the data**  
```{r echo=TRUE}
df <- read.csv("./activity.csv", header=TRUE, stringsAsFactors=TRUE)
```
  
  
### **What is mean total number of steps taken per day?** 
- #### Calculate the total number of steps taken per day.
```{r echo=TRUE}
stepsPerDay <- aggregate(x=list("daily_steps"=df$steps), by=list("date" = df$date), 
                         FUN=sum, na.rm=TRUE)
```  
- #### Make a histogram of the total number of steps taken each day.  
```{r echo=TRUE}
hist(stepsPerDay$daily_steps, breaks=nrow(stepsPerDay)/2, col="blue", 
     main="Daily steps", xlab="Number of steps")
```
```{r results='hide'}
dev.copy(png, file="./figure/steps_hist1.png", width=480, height=480, units = "px")
dev.off()
```  
  
- #### Calculate and report the mean and median of the total number of steps taken per day.
```{r echo=TRUE}
mean(stepsPerDay$daily_steps)
median(stepsPerDay$daily_steps)
```  
  
### **What is the average daily activity pattern?**  

- #### Make a time series plot.
```{r echo=TRUE}
stepsPerInt <- aggregate(x=list("int_steps"=df$steps), by=list("interval" = df$interval), FUN=mean, na.rm=TRUE)
plot(stepsPerInt$interval, stepsPerInt$int_steps, type="l", 
     xlab="Daily interval", ylab="Mean of steps", main="Average steps for each daily interval")
```
```{r results='hide'}
dev.copy(png, file="./figure/daily_pattern.png", width=480, height=480, units = "px")
dev.off()
```  

- #### Which 5-minute interval, contains the maximum number of steps?
```{r echo=TRUE}
stepsPerInt$interval[which(stepsPerInt$int_steps==max(stepsPerInt$int_steps))]
```  
  
  
### **Imputing missing values**  
- #### Calculate and report the total number of missing values in the dataset.

```{r echo=TRUE}
nas = which(is.na(df$steps))
length(nas)
``` 

- ####  Fill in all of the missing values in the dataset.
```{r echo=TRUE}
for(i in 1:nrow(df)) {
  if (is.na(df[i,1])){
    df[i,1] <- round(stepsPerInt[which(stepsPerInt$interval==df[i,3]),2], 0)
  }
}
```  
  
- #### Calculate the total number of steps taken per day / histogram/ mean and median.
```{r echo=TRUE}
stepsPerDay <- aggregate(x=list("daily_steps"=df$steps), by=list("date" = df$date), 
                         FUN=sum, na.rm=TRUE)

hist(stepsPerDay$daily_steps, breaks=nrow(stepsPerDay)/2, col="blue", 
     main="Daily steps", xlab="Number of steps")
mean(stepsPerDay$daily_steps)
median(stepsPerDay$daily_steps)
``` 
```{r results='hide'}
dev.copy(png, file="./figure/steps_hist2.png", width=480, height=480, units = "px")
dev.off()
```
  
### **Are there differences in activity patterns between weekdays and weekends?**  
```{r}
df$date <- weekdays(as.Date(df$date))
c <- sapply(1:nrow(df), function(i){
    if (df[i,2] == "Saturday" ||  df[i,2] == "Sunday") {
      "weekend"
    } else {"weekday"}
  })
df$w <- c
df <- transform(df, w=factor(w))
stepsPerInt <- aggregate(x=list("int_steps"=df$steps), 
                         by=list("interval"=df$interval, "w"=df$w), FUN=mean, na.rm=TRUE)
library(lattice)
xyplot(int_steps~interval | w, data=stepsPerInt, layout=c(1,2), type="l",
       xlab="5-min daily interval", ylab="numer of steps")
```
```{r results='hide'}
dev.copy(png, file="./figure/pattern2.png", width=480, height=480, units = "px")
dev.off()
```