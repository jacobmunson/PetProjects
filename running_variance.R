# Running Variance Estimate
# credit to: https://www.johndcook.com/blog/standard_deviation/

# Sequence of data: x1, x2, x3, ..., xN
# Initialize: M1 = x1; s1 = 0
## for k in {2:N}
## Mk = M_{k-1} + (xk - M_{k-1})/k
## sk = s_{k-1} + (xk - M_{k-1})*(xk - M_{k})
# For k^th estimate of variance: s^2 = sk/(k-1)

x = c(-2,3,1,-5,-1,4,1)

m = c(); # empty vector
m[1] = x[1] # initialized
s = c(); # empty vector 
s[1] = 0; # initialized
s2 = c() # empty vector

for(k in 2:length(x)){
  m[k] = m[k-1] + (x[k] - m[k-1])/k
  s[k] = s[k-1] + (x[k] - m[k-1])*(x[k] - m[k])
  s2[k] = s[k]/(k-1)  
}

m; s; s2
var(x) == s2[length(s2)] # nice
