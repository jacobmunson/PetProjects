#########################
### Cosine Similarity ###
# Cosine of two vectors in R
# cos() function takes cosine of a vector
# Examples to verify work:
# https://onlinemschool.com/math/library/vector/angl/

cosine_similarity = function(vec1, vec2){
  cos_sim <- (vec1 %*% vec2)/(sqrt(sum(vec1^2)) * sqrt(sum(vec2^2)))
  return(cos_sim)
}

a = c(3,4)
b = c(4,3)
cosine_similarity(a, b) # 0.96

a = c(7,1)
b = c(5,5)
cosine_similarity(a, b) # 0.8

a = c(3,4,0)
b = c(4,4,2)
cosine_similarity(a, b) # 0.933 = 14/15
