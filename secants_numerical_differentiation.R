# A little project I did on a flight from BZN to Houston
# Date? Early winter 2018?
# Basically, I wanted a visually representation of secant lines approaching the tangent line from either direction
# Difference Eqn: f(x) = f(x + h) - f(x)/(x + h)
# Derivative is lim h->0

x <- seq(-3,3,0.1)
y <- seq(1,10,1)
f <- function(x){
  y <- x^3
  return(y)
}
f. <- function(x,h){
  y. <- (f(x + h) - f(x) )/(x + h)
  return(y.)
}
point.x <- function(x1,x2){
  y1 <- f(x = x1)
  y2 <- f(x = x2)
  pt1 <- c(x1,y1)
  pt2 <- c(x2,y2)
  pts <- c(pt1,pt2)
  return(pts)
  #Get a secant going
  #Get a tangent going
  #plot both
  #get it so can inch h -> x
}
slope.x <- function(x1,y1,x2,y2){
  m <- (y2 - y1)/(x2 - x1)
  return(m)
}
intcpt.x <- function(x1,y1,m){
  # y = mx + b
  b = y1 - m*x1
  return(b)
}
secant <- function(x_0, xph, color){
  
  pts <- point.x(x1 = x_0, x2 = xph)
  #print(pts)
  #abline(v=xph)
  #abline(v = x_0)
  points(x = pts[1], y = pts[2])
  points(x = pts[3], y = pts[4])
  m <- slope.x(x1 = pts[1],y1 = pts[2],x2 = pts[3],y2 = pts[4])
  b <- intcpt.x(x1 = pts[1], y1 = pts[2], m = m)
  abline(a = b, b = m, col = color) #secant line
  return(m)
}
secants <- function(x_0, xph.range, color){
  sec <- NULL
  for(i in 1:length(xph.range)){
    sec[i] <- secant(x = x_0, xph = xph.range[i], color = color)
  }
  return(sec)
}
left_right_secants <- function(x_0, width, fineness, color_left, color_right){
  left_side <- seq(x_0 - width, x_0 - fineness, fineness)
  right_side <- seq(x_0 + fineness, x_0 + width, fineness)
  sec_left <- secants(x_0, xph.range = left_side, color = color_left)
  sec_right <- secants(x_0, xph.range = right_side, color = color_right)
  
  print(c("From left:",sort(sec_left, decreasing = TRUE)))
  print(c("From right:",sort(sec_right, decreasing = TRUE)))
  sec_right_sort <- sort(sec_right, decreasing = TRUE)
  sec_left_sort <- sort(sec_left, decreasing = FALSE)
  
  
  #more cases ? this isn't working quite right, probably more trickery to get it
  #in < case, it can pick both from right, need to fix that
  if(x_0 < 0){
    print(paste("Tangent slope is between", min(sec_right_sort[length(sec_right)], sec_left[length(sec_left)]), "and", max(sec_right_sort[length(sec_right)], sec_left[length(sec_left)]), "at x =", x_0))
  }else{
    print(paste("Tangent slope is between", min(sec_right_sort[length(sec_right)], sec_left_sort[length(sec_left_sort)]), "and", max(sec_right_sort[length(sec_right)], sec_left_sort[length(sec_left_sort)]), "at x =", x_0))
  }
    
  
  #print(paste("Tangent slope is between", min(sec_right_sort[length(sec_right)], sec_left[length(sec_left)]), "and", max(sec_right_sort[length(sec_right)], sec_left[length(sec_left)]), "at x =", x_0))
  print(paste("Secant line interations:", 2*length(sec_left)))
  
  #once the tangent slope range is correct, maybe average the two slopes from L/R
  #then use that slope in point slope form, use a point s.t. f' = m (I think)
  # or... use the two points that give those two slopes and point a line through those points   
  
  #plot(sec_left, ylim = c(min(sec_right,sec_left),max(sec_right,sec_left)))
  #points(sec_right_sort)
}

f_x <- f(x)
f_x
plot(x, f_x, type = "l")


xph <- 2
x_0 <- 7.5

xph.range <- seq(x_0 + 1, x_0 + 10, 1)

plot(x, f_x, type = "l")
left_right_secants(x_0 = -1, width = 2, fineness = 0.1, color_left = "orange", color_right = "green")


#BGD says "what about the quadratization of this bullshit?"
#Second degree (At most) approximation at any point.


#How many estimates? 
(2-1)/0.0001
(3-2)/0.0001
