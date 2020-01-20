# Running Variance Estimate
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
