D %>% group_by(user) %>% #filter(user == 36955) %>% 
  arrange(timestamp) %>% 
  select(item) %>% 
  group_by(user) %>% 
  mutate(next_item = lead(item)) %>% 
  arrange(user) #%>% filter(user == 36955)
  


item_names = D %>% select(item) %>% .$item

item_transitions 



num_samples = 1000000
num_users = 25000
users = seq(1:num_users)
states = c("A","B","C","D","E")


da = sample(states, size = num_samples/4, replace = T, prob = c(0.8,0.05,0.05,0.05,0.05))
db = sample(states, size = num_samples/4, replace = T, prob = c(0.15,0.4,0.15,0.1,0.2))
dc = sample(states, size = num_samples/4, replace = T, prob = c(0.25,0.15,0.25,0.3,0.25))
dd = sample(states, size = num_samples/4, replace = T, prob = c(0.35,0.15,0.1,0.3,0.1))

d1 = c(da,db,dc,dd)

d2 = sample(users, size = num_samples, replace = T)

Q = data.frame(user = d2, state = d1, stringsAsFactors = F)
Q = as_tibble(Q)

head(Q)
Q %>% arrange(user) %>% 
  group_by(user) %>% 
  mutate(next_state = lead(state)) %>% 
  arrange(user)

states_transitions = expand.grid(states,states,stringsAsFactors = F)
colnames(states_transitions) = c("state","next_state")
states_transitions$identifier = seq(1:nrow(states_transitions))
str(states_transitions)

state_movements = Q %>% arrange(user) %>% 
  group_by(user) %>% 
  mutate(next_state = lead(state)) %>% 
  arrange(user) %>% left_join(y = states_transitions) %>% .$identifier


actual_movements = merge(data.frame(table(state_movements)), states_transitions, by.x = "state_movements", by.y = "identifier")

actual_movements


movement_matrix = matrix(data = 0, 
                         nrow = length(states), 
                         ncol = length(states), 
                         dimnames = list(states, states))

for(i in 1:nrow(actual_movements)){
  
  movement_matrix[actual_movements[i,"state"], actual_movements[i,"next_state"]] = actual_movements[i,"Freq"]
    
}

movement_matrix
movement_matrix = movement_matrix/rowSums(movement_matrix)
round(movement_matrix, 3)

movement_matrix = movement_matrix %*% movement_matrix %*% 
                  movement_matrix %*% movement_matrix %*% 
                  movement_matrix %*% movement_matrix %*% 
                  movement_matrix %*% movement_matrix %*% 
                  movement_matrix %*% movement_matrix %*% 
                  movement_matrix %*% movement_matrix %*% 
                  movement_matrix %*% movement_matrix %*% 
                  movement_matrix %*% movement_matrix %*% 
                  movement_matrix %*% movement_matrix %*% 
                  movement_matrix %*% movement_matrix %*% 
                  movement_matrix %*% movement_matrix %*% 
                  movement_matrix %*% movement_matrix %*% 
                  movement_matrix %*% movement_matrix %*% 
                  movement_matrix %*% movement_matrix %*% 
                  movement_matrix %*% movement_matrix

round(movement_matrix, 3)

movement_matrix * c(0,0,0,1,0)


