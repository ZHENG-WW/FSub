################################
#data1: n>>p (n:1000, p:11)
################################
size = 1000 #n: number of instances
new_row <- function() {
    x1 = rnorm(1) #runif(n, min, max)
    x2 = rnorm(1)
    x3 = rnorm(1)
    x4 = rnorm(1)
    x5 = rnorm(1)
    
    #Target Variable 
    target = 3*x1 + 
      4*x2+ 
      8*x3+ 
      (-6)*x4 +
      (-12)*x5
    row = c(target,x1,x2,x3,x4,x5)
    return(row)
  }
  
  datalist = list()
  for (i in 1:size) {
    datalist[[i]] <- new_row()
  }
  data = do.call(rbind, datalist)
  colnames(data) =  c('target','x1', 'x2', 'x3', 'x4','x5') 
  data = as.data.frame(data)
  
  #noise
  rho = 0
  noise1 = rho * scale(data$target) + (1 - rho^2)^0.5* scale(rnorm(size))
  noise2 =rho * scale(data$target) + (1 - rho^2)^0.5* scale(rnorm(size))
  noise3 = rho * scale(data$target) + (1 - rho^2)^0.5* scale(rnorm(size))
  
  #redundant variables
  rho = 0.90
  redundance1 = rho * scale(data$x5) + (1 - rho^2)^0.5* scale(rnorm(size))
  cor(redundance1,data$x5, method = "spearman")
  
  rho = 0.95
  redundance2 = rho * scale(data$x5) + (1 - rho^2)^0.5* scale(rnorm(size))
  cor(redundance2,data$x5, method = "spearman")
  
  rho = 0.99
  redundance3 = rho * scale(data$x5) + (1 - rho^2)^0.5* scale(rnorm(size))
  cor(redundance3,data$x5, method = "spearman")
  
data = cbind.data.frame(data, noise1 = noise1, noise2 = noise2, noise3 = noise3,
                          redundance1 = redundance1, redundance2 = redundance2, redundance3 = redundance3)
  

################################
#data2, data3: n<<p (n:50, p:1000)
################################
size = 50  # number of instances
p=5      # number of valid features, data2:5, data3:300
w = runif(p,5,100)  # vector of beta
valid.data = matrix(0,size,p)
for (i in 1:p) {
    valid.data[,i]=rnorm(size)
}
target = valid.data %*% w
  
#noise
rho = 0.0000001
p=495
noise.data = matrix(0,size,p)
for (i in 1:p) {
    noise.data[,i]= round(rho * scale(target) + (1 - rho^2)^0.5* scale(rnorm(size)),2)
}
  
#redundant variables
rho = 0.99
p=500
redun.data = matrix(0,size,p)
for (i in 1:p) {
    redun.data[,i]=rho * scale(valid.data[,1]) + (1 - rho^2)^0.5* scale(rnorm(size))
}
data = cbind.data.frame(target, valid.data, noise.data, redun.data)
colnames(data) = c("target", paste0("x",1:5),paste0("noise",1:495), paste0("redun.",1:500))

  