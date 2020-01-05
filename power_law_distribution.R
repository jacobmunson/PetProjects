######################################
### Power Law Distribution Example ###
######################################

power_law_dist = function(a, k, x){return(a*x^k)}
x = seq(1:10)
plot(x, power_law_dist(a = 0.61, -2, x), type = 'l')
x = seq(10,100,10)
plot(x, power_law_dist(a = 0.61, k = -2, x = x), type = 'l')
x = seq(100,1000,100)
plot(x, power_law_dist(a = 0.61, k = -2, x = x), type = 'l')

# They basically all look the same
# "Scale free property"

# Let's look at a few points
p2 = power_law_dist(a = 0.61, k = -2, x = 2)
p4 = power_law_dist(a = 0.61, k = -2, x = 4)
p2/p4 # ratio = 4
p8 = power_law_dist(a = 0.61, k = -2, x = 8)
p4/p8 # ratio = 4

# things don't normally work this way

exp_function = function(a, c, x){return(a*c^(x-1))}

p2e = exp_function(a = 1/3, c = 2/3, x = 2)
p4e = exp_function(a = 1/3, c = 2/3, x = 4)
p2e/p4e # ratio = 2.25
p8e = exp_function(a = 1/3, c = 2/3, x = 8)
p4e/p8e # ratio = 5.0625



