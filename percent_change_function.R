###############################
### Percent Change Function ###
# 10/24/2019 ##################
# I find myself wanting to calculate percent change a lot, so I figured I'd throw a function together for it real quick like. 

percent_change = function(first_value, second_value){
  pct_change = ((second_value - first_value)/first_value) * 100
  print(paste0(round(pct_change,2), "% change."))
  return(pct_change)
}


percent_change(first_value = 144, second_value = 1295) # +799.3% change
percent_change(first_value = 1000, second_value = 42) # -95.8% change
percent_change(first_value = 100, second_value = 110) # +10% change
