library(data.table)
df<-fread("activity.csv")

df2<-aggregate(df$steps,by=list(df$date),FUN=sum,na.rm=TRUE)
names(df2)<-c("day","Number_Steps")
# Now df2 is an databframe that contains dates and total steps per day
png("Historgam.png", width = 480, height = 480, units = "px")
hist(df2$Number_Steps,main="Total step taken per day",xlab="Number of steps")
dev.off()
summary(df2)



##################################################
interval<- aggregate(df$steps,by=list(df$interval),FUN=mean,na.rm=TRUE)
names(interval)<-c("interval","steps")

png("daily_activity_pattern.png", width = 480, height = 480, units = "px")
plot(interval$interval,interval$steps,type="l",main="Average daily activity pattern",xlab="interval",ylab="steps",lwd=2)
dev.off()
maximo_intervalo<-interval[which.max(interval$steps),]
maximo_intervalo
####################################################
# Calculate total rows with NA
mal<-sum(is.na(df$steps))
sum(is.na(df$date))

# We have the mean of each interval, so we'ññ put the mean in the interval with NA
filled<-df

for(i in 1:length(filled$steps)){
      if(is.na(filled$steps[i])){
        filled$steps[i]<-interval[interval$interval == filled$interval[i],"steps"]

      }
}

df2<-aggregate(filled$steps,by=list(filled$date),FUN=sum)
names(df2)<-c("day","Number_Steps")
# Now df2 is an databframe that contains dates and total steps per day
png("Histogram_with_dataframe_filled.png", width = 480, height = 480, units = "px")
hist(df2$Number_Steps,main="Total step taken per day",xlab="Number of steps")
dev.off()
summary(df2)

#####################################################################

filled$day_type<-weekdays(filled$date)
filled$day_type[filled$day_type %in% c("sábado","domigo")]<- "weekend"
filled$day_type[filled$day_type != "weekend"]<- "weekday"

filled<-aggregate(filled$steps,by=list(filled$interval,filled$day_type),FUN=mean)


names(filled)<-c("interval","day_type","steps")
str(filled)
png("Comparison.png", width = 480, height = 480, units = "px")
par(mfrow=c(2,1))
# with(filled,{
            plot(filled$interval[filled$day_type=="weekend"],filled$steps[filled$day_type=="weekend"],type="l",lwd=2,main="Average weekend activity pattern",xlab="interval",ylab="steps")

            plot(filled$interval[filled$day_type=="weekday"],filled$steps[filled$day_type=="weekday"],type="l",lwd=2,main="Average weekday activity pattern",xlab="interval",ylab="steps")

# })
dev.off()
