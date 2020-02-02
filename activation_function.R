############################
### Activation Functions ###
# useful in many (most?) statistical models in some way or another
# useful in neural networks, etc... 
# Wikipedia entry: https://en.wikipedia.org/wiki/Activation_function

# Identity Function
identity_function = function(x){stopifnot(is.numeric(x));return(x)}

# Binary Step Function
step_function = function(x){
  stopifnot(is.numeric(x))
  if(x < 0){return(0)}else{
    return(1) #x >= 0
  }
}

# Standard Logistic Function
logistic_function = function(x){
  stopifnot(is.numeric(x))
  return(1 / (1 + exp(-x)))
}

# Hyperbolic Tangent Function
hyperbolic_tangent_function = function(x){ # rewriting instead of using tanh() canned function for personal edification
  stopifnot(is.numeric(x))
  return((exp(x) - exp(-x))/(exp(x) + exp(-x)))
}

# Softsign Activation Function
softsign_function = function(x){ 
  stopifnot(is.numeric(x))
  return(x/(1 + abs(x)))
}

# Inverse Square Root Unit Activation Function
# https://arxiv.org/abs/1710.09967
isru_function = function(x, alpha){
  stopifnot(is.numeric(x))
  stopifnot(alpha >= 0)
  return(x/sqrt(1 + alpha*x^2))
}

# Inverse Square Root Linear Unit Activation Function
# https://arxiv.org/abs/1710.09967
isrlu_function = Vectorize(function(x, alpha){
  stopifnot(is.numeric(x))
  stopifnot(alpha >= 0)
  if(x >= 0){return(x)}else{return(x/sqrt(1 + alpha*x^2))}
}, vectorize.args = "x")

# Rectified Linear Unit Activation Function
relu_function = Vectorize(function(x){
  stopifnot(is.numeric(x))
  if(x > 0){return(x)}else{return(0)}
}, vectorize.args = "x")

# Leaky Rectified Linear Unit Activation Function
leaky_relu_function = Vectorize(function(x){
  stopifnot(is.numeric(x))
  if(x >= 0){return(x)}else{return(0.01*x)}
}, vectorize.args = "x")

# Exponential Linear Unit Activation Function
elu_function = Vectorize(function(x, alpha){
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(alpha))
  if(x > 0){return(x)}else{return(alpha*(exp(x) - 1))}
}, vectorize.args = "x")

# Softplus Activation Function
softplus_function = function(x){
  stopifnot(is.numeric(x))
  return(log(1 + exp(x)))
}

# Bent Identity Activation Function
bent_identity_function = function(x){
  stopifnot(is.numeric(x))
  return((sqrt(x^2 + 1) - 1)/2 + x)
}
