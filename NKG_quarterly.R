title: "Time series quarterly analysis"
output: pdf_document
date: "2023-03-23"
---
  # import library

# truoc khi thay data + run all can sua data dung format, sua start_q la thoi gian bat dau, sua model 3, model 8 neu data khong am
knitr::opts_chunk$set(echo = TRUE)
library(Hmisc)
library(tidyverse)
library(Metrics)
install.packages("astsa")
library(astsa)
library(tseries)
library(readxl)
install.packages("tsutils")
library(tsutils)
library(forecast)


# import data + plot series:

clean_hpg <- read_excel("C:/Users/LeHuyPhong/Downloads/Book2.xlsx")
start_q <- 2010 #2011Q1
ts_netp <- ts(clean_hpg$`Data`, start = c(start_q, 1), freq = 4)
t <- seq_along(ts_netp)
# prediction data for 2023
t_pred <- seq(length(t) + 2, length(t) * 2 + 1)
seas <- seasdummy(51, 4) # 63 = length(ts)
predict_data <- data.frame(t = t_pred, seas1 = seas[, 1], seas2 = seas[, 2], seas3 = seas[, 3])
head(clean_hpg)



plot.ts(ts_netp)


# Model 1: Linear time trend (series ~ time)


model1 <- lm(ts_netp ~ t)
summary(model1)


plot(ts_netp, xlim = c(start_q - 1, 2024))
fitted1 <- ts(fitted(model1), start = c(2007, 1), freq = 4)
prediction1 <- ts(predict(model1, newdata = predict_data)[1:4], start = c(2023, 1), freq = 4)
lines(fitted1, col = "red")
lines(prediction1, col = "blue")


# RMSE, MAPE all observation:
rmse(ts_netp, fitted(model1))
mape(ts_netp, fitted(model1))


AIC(model1)



# RMSE, MAPE last 4 observations:
rmse(ts_netp[(length(ts_netp) - 3): length(ts_netp)], fitted(model1)[(length(ts_netp) - 3): length(ts_netp)])
mape(ts_netp[(length(ts_netp) - 3): length(ts_netp)], fitted(model1)[(length(ts_netp) - 3): length(ts_netp)])



# Model 2: Log time trend (series ~ log(time))

model2 <- lm(ts_netp ~ log(t))
summary(model2)


plot(ts_netp, xlim = c(start_q - 1, 2024))
fitted2 <- ts(fitted(model2), start = c(2007, 1), freq = 4)
prediction2 <- ts(predict(model2, newdata = predict_data)[1:4], start = c(2023, 1), freq = 4)
lines(fitted2, col = "red")
lines(prediction2, col = "blue")

# RMSE, MAPE all observation:
rmse(ts_netp, fitted(model2))
mape(ts_netp, fitted(model2))

AIC(model2)

# RMSE, MAPE last 4 observations:
rmse(ts_netp[(length(ts_netp) - 3): length(ts_netp)], fitted(model2)[(length(ts_netp) - 3): length(ts_netp)])
mape(ts_netp[(length(ts_netp) - 3): length(ts_netp)], fitted(model2)[(length(ts_netp) - 3): length(ts_netp)])


# Model 3: log-linear time trend (log(series) ~ time)

# because net profit series have negative values (indicating loss), cannot take log -> rescale data
net_p_positive <- ts_netp + abs(min(ts_netp)) + 1
model3 <- lm(log(net_p_positive) ~ t)
summary(model3)


plot(ts_netp, xlim = c(start_q - 1, 2024))
fitted3 <- ts(exp(fitted(model3)) - abs(min(ts_netp)) - 1, start = c(2007, 1), freq = 4)
prediction3 <- ts(exp(predict(model3, newdata = predict_data)[1:4]) - abs(min(ts_netp)) - 1, start = c(2023, 1), freq = 4)
lines(fitted3, col = "red")
lines(prediction3, col = "blue")



# RMSE, MAPE all observation:
rmse(ts_netp, fitted(model3))
mape(ts_netp, fitted(model3))


AIC(model3)




# RMSE, MAPE last 4 observations:
rmse(ts_netp[(length(ts_netp) - 3): length(ts_netp)], fitted(model3)[(length(ts_netp) - 3): length(ts_netp)])
mape(ts_netp[(length(ts_netp) - 3): length(ts_netp)], fitted(model3)[(length(ts_netp) - 3): length(ts_netp)])


# Model 4: seasonal (series ~ s2 + s3 +s4)


model4 <- lm(ts_netp ~ seas)
summary(model4)



plot(ts_netp, xlim = c(start_q - 1, 2024))
fitted4 <- ts(fitted(model4), start = c(2007, 1), freq = 4)
prediction4 <- ts(predict(model4, newdata = predict_data)[1:4], start = c(2023, 1), freq = 4)
lines(fitted4, col = "red")
lines(prediction4, col = "blue")



# RMSE, MAPE all observation:
rmse(ts_netp, fitted(model4))
mape(ts_netp, fitted(model4))



AIC(model4)




# RMSE, MAPE last 4 observations:
rmse(ts_netp[(length(ts_netp) - 3): length(ts_netp)], fitted(model4)[(length(ts_netp) - 3): length(ts_netp)])
mape(ts_netp[(length(ts_netp) - 3): length(ts_netp)], fitted(model4)[(length(ts_netp) - 3): length(ts_netp)])


# Model 5: linear trend + seasonal (series ~ time + seas)

model5 <- lm(ts_netp ~ t + seas)
summary(model5)


plot(ts_netp, xlim = c(start_q - 1, 2024))
fitted5 <- ts(fitted(model5), start = c(2007, 1), freq = 4)
prediction5 <- ts(predict(model5, newdata = predict_data)[1:4], start = c(2023, 1), freq = 4)
lines(fitted5, col = "red")
lines(prediction5, col = "blue")



# RMSE, MAPE all observation:
rmse(ts_netp, fitted(model5))
mape(ts_netp, fitted(model5))



AIC(model5)



# RMSE, MAPE last 4 observations:
rmse(ts_netp[(length(ts_netp) - 3): length(ts_netp)], fitted(model5)[(length(ts_netp) - 3): length(ts_netp)])
mape(ts_netp[(length(ts_netp) - 3): length(ts_netp)], fitted(model5)[(length(ts_netp) - 3): length(ts_netp)])


# Model 6: linear time trend * seasonal (series ~ time * seas)

model6 <- lm(ts_netp ~ t * seas)
summary(model6)



plot(ts_netp, xlim = c(start_q - 1, 2024))
fitted6 <- ts(fitted(model6), start = c(2007, 1), freq = 4)
prediction6 <- ts(predict(model6, newdata = predict_data)[1:4], start = c(2023, 1), freq = 4)
lines(fitted6, col = "red")
lines(prediction6, col = "blue")



# RMSE, MAPE all observation:
rmse(ts_netp, fitted(model6))
mape(ts_netp, fitted(model6))


AIC(model6)



# RMSE, MAPE last 4 observations:
rmse(ts_netp[(length(ts_netp) - 3): length(ts_netp)], fitted(model6)[(length(ts_netp) - 3): length(ts_netp)])
mape(ts_netp[(length(ts_netp) - 3): length(ts_netp)], fitted(model6)[(length(ts_netp) - 3): length(ts_netp)])


# Model 7: Holt-winters additive

model7 <- hw(ts_netp, seasonal = "a") # package forecast
summary(model7)


prediction7 <- predict(model7)$mean
plot(ts_netp, xlim = c(start_q - 1, 2024))
lines(fitted(model7), col = "red")
lines(ts(prediction7[2:5], start = c(2023, 1), freq = 4), col = "blue")



# RMSE, MAPE all observation:
rmse(ts_netp, fitted(model7))
mape(ts_netp, fitted(model7))




# RMSE, MAPE last 4 observations:
rmse(ts_netp[(length(ts_netp) - 3): length(ts_netp)], fitted(model7)[(length(ts_netp) - 3): length(ts_netp)])
mape(ts_netp[(length(ts_netp) - 3): length(ts_netp)], fitted(model7)[(length(ts_netp) - 3): length(ts_netp)])

# Model 8: Holt-winters multiplicative

model8 <- HoltWinters(ts_netp, seasonal = "m")
model8


prediction8 <- predict(model8, 10)
plot(ts_netp, xlim = c(start_q - 1, 2024))
lines(fitted(model8)[, 1], col = "red")
lines(ts(prediction8[2:10], start = c(2023, 1), freq = 4), col = "blue")



# RMSE, MAPE all observation:
rmse(ts_netp, fitted(model8))
mape(ts_netp, fitted(model8))




# RMSE, MAPE last 4 observations:
rmse(ts_netp[(length(ts_netp) - 3): length(ts_netp)], fitted(model8)[(length(ts_netp) - 3): length(ts_netp)])
mape(ts_netp[(length(ts_netp) - 3): length(ts_netp)], fitted(model8)[(length(ts_netp) - 3): length(ts_netp)])

# Model 9: decomposition (hard):
# Model selection 
#-> Minimize AIC, SIC, HQ 
#-> maximize log-likelihood 
#-> compare forecast value: 
#-> minimize forecast error: 