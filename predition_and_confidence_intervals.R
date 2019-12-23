# Prediction and Confidence Intervals
# for lm() just for example
# plotting included (of course) 

df <- data.frame(x = c(0.5, 1, 1.5, 1, 2, 4, 5, 7, 9, 9.5, 10, 11, 11.1, 6, 5.5, 7, 8),
                 y = c(1, 0.75, 2, 0.8, 1.1, 4.2, 4.7, 8, 9, 9.5, 7, 7.6, 9, 6, 4.5, 6, 4))

lm1 <- lm(y ~ x, data=df)

p_conf1 <- predict(lm1,interval="confidence")
p_pred1 <- predict(lm1,interval="prediction")

new_data <- data.frame(x = seq(0,max(df$x), length = 40))

p_conf2 <- predict(lm1, interval="confidence", newdata = new_data)
p_pred2 <- predict(lm1, interval="prediction", newdata = new_data)

plot(y ~ x, data = df) # plot data
abline(lm1) # plot fit

matlines(df$x, p_conf1[,c("lwr","upr")], col=2, lty=1, type="b", pch="+")
matlines(df$x, p_pred1[,c("lwr","upr")], col=2, lty=2, type="b", pch=1)
matlines(new_data$x, p_conf2[,c("lwr","upr")], col=4, lty=1, type="b", pch="+")
matlines(new_data$x, p_pred2[,c("lwr","upr")], col=4, lty=2, type="b", pch=1)
