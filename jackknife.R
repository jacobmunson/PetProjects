r = c(4,3,3,2,5) 
sim = c(1,0.75,0.5,0.32,0.06)

df_x = data.frame(r = r, sim = sim)

library(bootstrap) # library version
theta = function(x){mean(x)}
jk = jackknife(r, theta = theta)



theta = function(df_x){sum(df_x$r * df_x$sim)/sum(abs(df_x$sim))}
theta_dot = theta(df_x)

mean(r)
theta_dot

x = r
df_loc = t(combn(1:length(x), length(x)-1))


theta_i = rep(NA,nrow(df_loc))
for(i in 1:nrow(df_loc)){
  print(df_x[df_loc[i,],])
  print(theta(df_x[df_loc[i,],]))
  theta_i[i] = theta(df_x[df_loc[i,],])
}

theta_dot # weighted mean estimate
theta_i # jk estimates of weighted mean
mean(theta_i)
n = nrow(df_x)

se_jack_weighted = sqrt(((n-1)/n)*(sum((theta_i - theta_dot     )^2)))
se_jack_unweighted = sqrt(((n-1)/n)*(sum((mean(r) - jk$jack.values)^2)))


# Confidence intervals 
multi = qt(p = 0.975, df = n-1)

c(mean(r) - multi*se_jack_unweighted, mean(r), mean(r) + multi*se_jack_unweighted)
c(theta_dot - multi*se_jack_weighted, theta_dot, theta_dot + multi*se_jack_weighted)
# what does this actually mean? CI for prediction? For... user? For what? 

