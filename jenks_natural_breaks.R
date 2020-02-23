#list = [4, 5, 9, 10]
d = c(14, 11, 10, 8, 7, 6.2, 5.5, 4.3, 4.1, 3.9, 3, 2.5, 1.1, 0.5, 0.45, 0.03, -0.12, -1)
#mean = 7  #(4 + 5 + 9 + 10) / 4
mu_d = mean(d)

sdam = sum((d - mu_d)^2) # 26
#SDAM = (4-7)^2 + (5-7)^2 + (9-7)^2 + (10-7)^2 = 9 + 4 + 4 + 9 = 26

k = 2


sdcm = rep(0,length(d))
for(i in 1:(length(d)-1)){
  #i = 1
  
  #i = 3
  # 1  2,3,4
  #i = 2
  # 1,2  3,4
  sdcm[i] = sum((d[1:i] - mean(d[1:i]))^2) + sum((d[-c(1:i)] - mean(d[-c(1:i)]))^2)

}
sdcm


gvf = rep(0,length(d))
for(i in 1:(length(d)-1)){
  #i = 1
  
  #i = 3
  # 1  2,3,4
  #i = 2
  # 1,2  3,4
  #sdcm[i] = sum((d[1:i] - mean(d[1:i]))^2) + sum((d[-c(1:i)] - mean(d[-c(1:i)]))^2)
  gvf[i] = (sdam - sdcm[i])/sdam 

}
gvf

d[1:which.max(gvf)]


# (sdam - sdcm[1])/sdam 
# (sdam - sdcm[2])/sdam 
# (sdam - sdcm[3])/sdam 
# (sdam - sdcm[4])/sdam 

x = c(1,2,3,4,5)
y = c(3,4,5,1,2)

lira(x_u = x, 
     x_v = y, 
     lira_pure_chance_pdf = lira_pure_chance_distribution(V = c(1,2,3,4,5)), 
     lira_same_cluster_pdf = lira_same_cluster_distribution(V = c(1,2,3,4,5)))

#(0.125^3 * 0.0625^2)/(0.24^3 * 0.16^2)

log10((0.125^3 * 0.0625^2)/(0.24^3 * 0.16^2))

x_new = c(x,3)
y_new = c(y,1)
log10(10^l * (0.125/0.24))

#abs(x_new - y_new)

lira(x_u = x_new, 
     x_v = y_new, 
     lira_pure_chance_pdf = lira_pure_chance_distribution(V = c(1,2,3,4,5)), 
     lira_same_cluster_pdf = lira_same_cluster_distribution(V = c(1,2,3,4,5)))


