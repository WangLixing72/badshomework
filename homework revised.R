getwd()

setwd("/Users/linmemeda/Desktop/Assignment_BADS_WS1718")


oridata<-read.csv("BADS_WS1718_known.csv", sep = ",", header = TRUE)
classdata<-read.csv("BADS_WS1718_class.csv", sep = ",", header = TRUE)

clean_date <- function(data){
  #deal with delivery_date error and replace the error terms with median of delivery interval
  
  data$delivery_date[data$delivery_date == "1990-12-31"] <- NA
  
  #All error delivery_time data have delivery_date at 1990-12-31.
  
  data$order_date<-as.Date(data$order_date)
  
  data$delivery_date<-as.Date(data$delivery_date)
  
  interval.median<-data$delivery_date-data$order_date
  
  deli.median<-median(interval.median,na.rm = T)
  
  data$delivery_date[is.na(data$delivery_date)]<-data$order_date[is.na(data$delivery_date)]+deli.median
  
  data$delivery_range <- data$delivery_date - data$order_date
  
  return(data)
}

clean_age <- function(data){
  #deal with users' age error
  
  age.data<-as.Date(data$user_dob)
  
  user.age<-median(age.data, na.rm = T)
  
  dob.iqr <- IQR(age.data, na.rm = T)
  
  first.qu <- quantile(as.vector(age.data), probs = 0.25, type = 1, na.rm = T)
  
  third.qu <- quantile(as.vector(age.data), probs = 0.75, type = 1, na.rm = T)
  
  age.data[age.data <= as.Date(first.qu, origin = "1970-01-01") - 3 * round(dob.iqr) |age.data >= as.Date(third.qu, origin = "1970-01-01") + 3 * round(dob.iqr)] <- NA
  
  age.data[is.na(age.data)] <- user.age
  
  summary(age.data)
  
  data$user_dob <- age.data
  
  #if(! "lubridate" %in% rownames(installed.packages())){install.packages("lubridate")}
  
  library(lubridate,warn.conflicts = F)
  
  data$age <- year(Sys.Date()) - year(data$user_dob)
  
  return(data)
}



cln.data <- clean_date(oridata)

cln.data <- clean_age(cln.data)
cln.data$return <- as.factor(cln.data$return)


#Prediction
library(caret)
library(rpart.plot)
set.seed(3333)
intrain <- createDataPartition(y = cln.data$return, p= 0.7, list = FALSE)
training <- cln.data[intrain,]
testing <- cln.data[-intrain,]

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
dt <- train(return ~item_price + age + user_title  + delivery_range + item_color + item_size, data = training, method = "rpart",
            parms = list(split = "information"),
            trControl=trctrl,
            tuneLength = 10)


dt.fit <- predict(dt,testing)
confusionMatrix(dt.fit,testing$return)


### prediction

cln.class <- clean_date(classdata)
cln.class <- clean_age(cln.class)
return.class <- predict(dt, newdata = cln.class, type = "class")
cln.class <- cbind(cln.class, return.class)
newdata <- cln.class[ , c("order_item_id", "return.class")]
colnames(newdata) <- c("order_item_id", "return")
write.csv(newdata, file = "527054_Qin.csv", row.names = FALSE)



