getwd()
setwd("/Users/linmemeda/Desktop/Assignment_BADS_WS1718")
oridata<-read.csv("BADS_WS1718_known.csv", sep = ",", header = TRUE)

#deal with delivery_date error and replace the error terms with median of delivery interval
oridata$delivery_date[oridata$delivery_date == "1990-12-31"] <- NA
#All error delivery_time data have delivery_date at 1990-12-31.
oridata$order_date<-as.Date(oridata$order_date)
oridata$delivery_date<-as.Date(oridata$delivery_date)
interval.median<-oridata$delivery_date-oridata$order_date
deli.median<-median(interval.median,na.rm = T)
oridata$delivery_date[is.na(oridata$delivery_date)]<-oridata$order_date[is.na(oridata$delivery_date)]+deli.median

#deal with users' age error
age.data<-as.Date(oridata$user_dob)
user.age<-median(age.data, na.rm = T)
dob.iqr <- IQR(age.data, na.rm = T)
first.qu <- quantile(as.vector(age.data), probs = 0.25, type = 1, na.rm = T)
third.qu <- quantile(as.vector(age.data), probs = 0.75, type = 1, na.rm = T)
age.data[age.data <= as.Date(first.qu, origin = "1970-01-01") - 3 * round(dob.iqr) |age.data >= as.Date(third.qu, origin = "1970-01-01") + 3 * round(dob.iqr)] <- NA
age.data[is.na(age.data)] <- user.age
summary(age.data)
oridata$user_dob <- age.data

#deal with item size

#deal with item color
colormiss<-sum(is.na(oridata$item_color))
colormiss
oridata$item_color[is.na(oridata$item_color)==T]<-names(table(oridata$item_color))[table(oridata$item_color)==max(table(oridata$item_color))]
summary(oridata$item_color)
sort(table(oridata$item_color))
#deal with user date
summary(oridata$user_state)
table(oridata$user_state)
summary(known$user_state)
table(known$user_state)
table(known$user_state, known$return)

table(oridata$item_price)
