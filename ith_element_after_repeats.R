# This came as a request from a classmate, so throwing it here. 
x = seq(-1,1.55,0.01) # generate some data
x = sample(x) # reorder it
which(x > 1.5) # elements that are larger than 1.5
which(x > 1.5)[2] # the second element that satisfies the condition - user specified
x[which(x > 1.5)] # what are the actual elements? (not requested)


position = c() # empty vector
for(i in 1:10){ # loop of size 10
  x = seq(-1,1.55,0.01) # make a new sequence with each run
  x = sample(x) # reorder as before
  position[i] = which(x > 1.5)[2] # put position of the second elements in the empty vector
}
position # review results
