
set.seed(1)
# Synthetic data generation

## data for 1 year
days = 360

## Events encoded as 0,1
event_indicator = c(0,1)

## Breaking the year into quarters 
## arbitrary, could be anything
q1_prob = c(0.5,0.5)
q2_prob = c(0.7,0.3)
q3_prob = c(0.4,0.6)
q4_prob = c(0.1,0.9)

## Generating Events based on quarterly probabilities 
q1_events = sample(x = c(0,1), size = days/4, replace = T, prob = q1_prob)
q2_events = sample(x = c(0,1), size = days/4, replace = T, prob = q2_prob)
q3_events = sample(x = c(0,1), size = days/4, replace = T, prob = q3_prob)
q4_events = sample(x = c(0,1), size = days/4, replace = T, prob = q4_prob)

events = c(q1_events, q2_events, q3_events, q4_events, 
           q1_events, q2_events, q3_events, q4_events,
           q1_events, q2_events, q3_events, q4_events,
           q1_events, q2_events, q3_events, q4_events)

## Plotting
# the sample() bit is just to change magnitude to reflect what an actual data source would probably look like
# clear seasonality in Q4
plot(events * sample(x = seq(1:10), size = length(events), replace = T), type = 'h', ylab = "") # Inflated

# Indicator
plot(events, type = 'h', main = 'Event Indicator') # Indicator of event

# Empty matrices for transition probabilities (as a transition matrix and tracking the transition probabilities through time)
event_transitions = matrix(data = 0, nrow = 2, ncol = 2, dimnames = list(c("No Event","Event"),c("No Event","Event")))
transition_probabilities = matrix(data = 0, nrow = length(events)-1, ncol = 4); colnames(transition_probabilities) = c("E_E","E_NE","NE_NE","NE_E")

for(i in 1:(length(events)-1)){

  #events[i:(i+1)]
  if(events[i] == 1 & events[i+1] == 1){event_transitions["Event","Event"] = event_transitions["Event","Event"] + 1}
  if(events[i] == 1 & events[i+1] == 0){event_transitions["Event","No Event"] = event_transitions["Event","No Event"] + 1}
  if(events[i] == 0 & events[i+1] == 0){event_transitions["No Event","No Event"] = event_transitions["No Event","No Event"] + 1}
  if(events[i] == 0 & events[i+1] == 1){event_transitions["No Event","Event"] = event_transitions["No Event","Event"] + 1}

  transition_probabilities[i,"E_E"] = (event_transitions/rowSums(event_transitions))["Event","Event"]
  transition_probabilities[i,"E_NE"] = (event_transitions/rowSums(event_transitions))["Event","No Event"]
  transition_probabilities[i,"NE_NE"] = (event_transitions/rowSums(event_transitions))["No Event","No Event"]
  transition_probabilities[i,"NE_E"] = (event_transitions/rowSums(event_transitions))["No Event","Event"]
  
}
transition_probabilities[!is.finite(transition_probabilities)] = 0 # there are a few NaNs in the first few lines from bad 0/0 averages

## Transition Probablities 
event_transitions/rowSums(event_transitions)

## Running Transition Probabilities
transition_probabilities

## Plot of above 
plot(transition_probabilities[,"E_E"], type = 'l', col = "darkgreen", ylim = c(0,1)); #abline(v = seq(0,days, by = days/4), col = 'red')
lines(transition_probabilities[,"E_NE"], col = 'orange'); #abline(v = seq(0,days, by = days/4), col = 'red')
lines(transition_probabilities[,"NE_NE"], col = 'grey'); #abline(v = seq(0,days, by = days/4), col = 'red')
lines(transition_probabilities[,"NE_E"], col = 'purple'); #abline(v = seq(0,days, by = days/4), col = 'red')
#abline(v = seq(0,days*4,days/4), col = 'red')


### ignore below - make for comparing a "convergence" of timeframes 

#A = transition_probabilities - 1440
lines(A[,"E_E"], type = 'l', col = "darkgreen"); #abline(v = seq(0,days, by = days/4), col = 'red')
lines(A[,"E_NE"], col = 'orange'); #abline(v = seq(0,days, by = days/4), col = 'red')
lines(A[,"NE_NE"], col = 'grey'); #abline(v = seq(0,days, by = days/4), col = 'red')
lines(A[,"NE_E"], col = 'purple'); #abline(v = seq(0,days, by = days/4), col = 'red')

#B = transition_probabilities - 720
lines(B[,"E_E"], type = 'l', col = "darkgreen"); #abline(v = seq(0,days, by = days/4), col = 'red')
lines(B[,"E_NE"], col = 'orange'); #abline(v = seq(0,days, by = days/4), col = 'red')
lines(B[,"NE_NE"], col = 'grey'); #abline(v = seq(0,days, by = days/4), col = 'red')
lines(B[,"NE_E"], col = 'purple'); #abline(v = seq(0,days, by = days/4), col = 'red')

#C = transition_probabilities - 1080
lines(C[,"E_E"], type = 'l', col = "darkgreen"); #abline(v = seq(0,days, by = days/4), col = 'red')
lines(C[,"E_NE"], col = 'orange'); #abline(v = seq(0,days, by = days/4), col = 'red')
lines(C[,"NE_NE"], col = 'grey'); #abline(v = seq(0,days, by = days/4), col = 'red')
lines(C[,"NE_E"], col = 'purple'); #abline(v = seq(0,days, by = days/4), col = 'red')

#D = transition_probabilities - 360
lines(D[,"E_E"], type = 'l', col = "darkgreen"); #abline(v = seq(0,days, by = days/4), col = 'red')
lines(D[,"E_NE"], col = 'orange'); #abline(v = seq(0,days, by = days/4), col = 'red')
lines(D[,"NE_NE"], col = 'grey'); #abline(v = seq(0,days, by = days/4), col = 'red')
lines(D[,"NE_E"], col = 'purple'); #abline(v = seq(0,days, by = days/4), col = 'red')

#E = transition_probabilities - 1200
lines(E[,"E_E"], type = 'l', col = "darkgreen"); #abline(v = seq(0,days, by = days/4), col = 'red')
lines(E[,"E_NE"], col = 'orange'); #abline(v = seq(0,days, by = days/4), col = 'red')
lines(E[,"NE_NE"], col = 'grey'); #abline(v = seq(0,days, by = days/4), col = 'red')
lines(E[,"NE_E"], col = 'purple'); #abline(v = seq(0,days, by = days/4), col = 'red')

#G = transition_probabilities# - 1300
lines(G[,"E_E"], type = 'l', col = "darkgreen"); #abline(v = seq(0,days, by = days/4), col = 'red')
lines(G[,"E_NE"], col = 'orange'); #abline(v = seq(0,days, by = days/4), col = 'red')
lines(G[,"NE_NE"], col = 'grey'); #abline(v = seq(0,days, by = days/4), col = 'red')
lines(G[,"NE_E"], col = 'purple'); #abline(v = seq(0,days, by = days/4), col = 'red')



(A[,"E_E"] - B[,"E_E"])[length(A[,"E_E"] - B[,"E_E"])]
(A[,"E_NE"] - B[,"E_NE"])[length(A[,"E_NE"] - B[,"E_NE"])]
(A[,"NE_NE"] - B[,"NE_NE"])[length(A[,"NE_NE"] - B[,"NE_NE"])]
(A[,"NE_E"] - B[,"NE_E"])[length(A[,"NE_E"] - B[,"NE_E"])]

(A[,"E_E"] - C[,"E_E"])[length(A[,"E_E"] - C[,"E_E"])]
(A[,"E_NE"] - C[,"E_NE"])[length(A[,"E_NE"] - C[,"E_NE"])]
(A[,"NE_NE"] - C[,"NE_NE"])[length(A[,"NE_NE"] - C[,"NE_NE"])]
(A[,"NE_E"] - C[,"NE_E"])[length(A[,"NE_E"] - C[,"NE_E"])]

(A[,"E_E"] - D[,"E_E"])[length(A[,"E_E"] - D[,"E_E"])]
(A[,"E_NE"] - D[,"E_NE"])[length(A[,"E_NE"] - D[,"E_NE"])]
(A[,"NE_NE"] - D[,"NE_NE"])[length(A[,"NE_NE"] - D[,"NE_NE"])]
(A[,"NE_E"] - D[,"NE_E"])[length(A[,"NE_E"] - D[,"NE_E"])]

(A[,"E_E"] - E[,"E_E"])[length(A[,"E_E"] - E[,"E_E"])]
(A[,"E_NE"] - E[,"E_NE"])[length(A[,"E_NE"] - E[,"E_NE"])]
(A[,"NE_NE"] - E[,"NE_NE"])[length(A[,"NE_NE"] - E[,"NE_NE"])]
(A[,"NE_E"] - E[,"NE_E"])[length(A[,"NE_E"] - E[,"NE_E"])]

(A[,"E_E"] - G[,"E_E"])[length(A[,"E_E"] - G[,"E_E"])]
(A[,"E_NE"] - G[,"E_NE"])[length(A[,"E_NE"] - G[,"E_NE"])]
(A[,"NE_NE"] - G[,"NE_NE"])[length(A[,"NE_NE"] - G[,"NE_NE"])]
(A[,"NE_E"] - G[,"NE_E"])[length(A[,"NE_E"] - G[,"NE_E"])]


