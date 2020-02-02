##############################################################################
### Building First Order Transition Matrices for Markov Chains using dplyr ###
##############################################################################

## Build a dataset
num_samples = 1000000
num_users = 25000
users = seq(1:num_users)
states = c("A","B","C","D","E")

## States
da = sample(states, size = num_samples/4, replace = T, prob = c(0.8,0.05,0.05,0.05,0.05))
db = sample(states, size = num_samples/4, replace = T, prob = c(0.15,0.4,0.15,0.1,0.2))
dc = sample(states, size = num_samples/4, replace = T, prob = c(0.25,0.15,0.25,0.3,0.25))
dd = sample(states, size = num_samples/4, replace = T, prob = c(0.35,0.15,0.1,0.3,0.1))
d1 = c(da,db,dc,dd)

## Users
d2 = sample(users, size = num_samples, replace = T)

## Dataset
D = data.frame(user = d2, state = d1, stringsAsFactors = F)
D = as_tibble(D)

## Looking at it
head(D)

## Framing up a transition matrix
# possible transitions
states_transitions = expand.grid(states,states,stringsAsFactors = F)
colnames(states_transitions) = c("state","next_state")
# give each unique transition an identifier
states_transitions$identifier = seq(1:nrow(states_transitions))

## Join on state and next_state to give each transition an identifer
state_movements = D %>% arrange(user) %>% 
  group_by(user) %>% 
  mutate(next_state = lead(state)) %>% 
  arrange(user) %>% left_join(y = states_transitions) %>% .$identifier

## Join frequency of previous step
actual_movements = merge(data.frame(table(state_movements)), 
                         states_transitions, 
                         by.x = "state_movements", by.y = "identifier")
actual_movements

## Transition Matrix
transition_matrix = matrix(data = 0, 
                           nrow = length(states), 
                           ncol = length(states), 
                           dimnames = list(states, states))

## Fill in Transition Matrix
# much cheaper to do than iteration through original matrix
# only 25 lines here instead of a million
for(i in 1:nrow(actual_movements)){
  transition_matrix[actual_movements[i,"state"], actual_movements[i,"next_state"]] = actual_movements[i,"Freq"]
}

transition_matrix

## Normalize
transition_matrix = transition_matrix/rowSums(transition_matrix)
round(transition_matrix, 3)

## Stationary Distribution
transition_matrix = transition_matrix %*% transition_matrix %*% 
                    transition_matrix %*% transition_matrix %*% 
                    transition_matrix %*% transition_matrix %*% 
                    transition_matrix %*% transition_matrix %*% 
                    transition_matrix %*% transition_matrix %*% 
                    transition_matrix %*% transition_matrix %*% 
                    transition_matrix %*% transition_matrix %*% 
                    transition_matrix %*% transition_matrix %*% 
                    transition_matrix %*% transition_matrix %*% 
                    transition_matrix %*% transition_matrix %*% 
                    transition_matrix %*% transition_matrix %*% 
                    transition_matrix %*% transition_matrix %*% 
                    transition_matrix %*% transition_matrix %*% 
                    transition_matrix %*% transition_matrix %*% 
                    transition_matrix %*% transition_matrix

## Visual Examination
round(transition_matrix, 3)

## Probability of Next Step
initial_vector = c(0,0,0,1,0)
transition_matrix * initial_vector


