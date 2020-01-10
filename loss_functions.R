######################
### Loss Functions ###
# 1/9/2020 ###########
######################

# The idea is that x_pred is a vector of predicted values and we want to see how the loss changes
# compared to how "far" the actual is from the predicted - this is not meant to determine the "entire error" like MSE or SSE
# Actual value
x = 0
# Predicted values
x_pred = seq(-10,10,0.1)

x - x_pred
is.numeric(x)
is.numeric(x_pred)
str(x_pred)

abs_loss_function = function(actual, predicted){
  stopifnot(is.numeric(actual))
  stopifnot(is.numeric(predicted))
  return(abs(actual - predicted))
}

plot(x_pred, abs_loss_function(actual = x, predicted = x_pred), type = 'l')

squared_loss_function = function(actual, predicted){
  stopifnot(is.numeric(actual))
  stopifnot(is.numeric(predicted))
  return((actual - predicted)^2)
}

plot(x_pred, squared_loss_function(actual = x, predicted = x_pred), type = 'l')

zero_one_loss_function = function(actual, predicted){
  stopifnot(is.numeric(actual))
  stopifnot(is.numeric(predicted))
  diff = actual - predicted
  diff[diff != 0] = 1
  
  return(diff)
}

plot(x_pred, zero_one_loss_function(actual = x, predicted = x_pred), type = 'l')


plot(x_pred, squared_loss_function(actual = x, predicted = x_pred), type = 'l', col = 'green')
lines(x_pred, abs_loss_function(actual = x, predicted = x_pred), col = 'red')
lines(x_pred, zero_one_loss_function(actual = x, predicted = x_pred), col = 'blue')

