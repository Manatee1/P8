library('tidyverse')


f = function(x){
  return((x>0)) 
}

phi_k = function(x){
  n_x = x^2
  return(exp(1/(n_x-1))*(n_x <= 1))
}

k = integrate(phi_k, lower = -5, upper = 5)$value 

phi = function(x,k){
  n_x = x^2 
  return((1/k)*exp(1/(n_x-1))*(n_x <= 1))
}

phi_eps = function(x,k,eps){
  return((1/eps)*phi(x/eps,k))
}


x = seq(from = -1.2, to = 1.2, length.out = 1000)
phi_1 = sapply(x,function(x){phi_eps(x,k=k, eps = 1)})
phi_2 = sapply(x,function(x){phi_eps(x,k=k, eps = 1/2)})
phi_3 = sapply(x,function(x){phi_eps(x,k=k, eps = 1/3)})
phi_4 = sapply(x,function(x){phi_eps(x,k=k, eps = 1/4)})

Phi = cbind(phi_1,phi_2,phi_3,phi_4,x) %>% data.frame()
names(Phi) = c("phi[1]","phi[1/2]","phi[1/3]","phi[1/4]","x")

Phi %>% gather(key = "Function",value = "Value",-x) %>% ggplot(aes(x = x, y = Value, color = Function)) + geom_line() +
  scale_color_discrete(labels = c(expression(varphi[1/2]), 
                                  expression(varphi[1/3]),
                                  expression(varphi[1/4]),
                                  expression(varphi[1]))) + 
  ggtitle(expression(paste("Plot of ",phi1[1/n]))) + 
  theme(legend.text = element_text(size = 12),
        legend.text.align = 0)


x_hat = seq(from = -5, to = 5, length.out = 1000)
y_hat = matrix(0,nrow = 1000, ncol = 3)
for(j in 1:3){
  for(i in 1:1000){
    y_hat[i,j] = integrate(function(x){f(x_hat[i]-x)*phi_eps(x,k=k,1/j)}, lower = -5, upper = 5)$value
  }
}


y = sapply(x_hat,f)
plot(x_hat,y, type = "p")
for(i in 1:4){
  lines(x_hat,y_hat[,i], type = "l", col = i+1)
}

Y = cbind(y,y_hat)
Y_df = data.frame(Y,x=x_hat)
names(Y_df) = c("y","y[1/2]","y[1/3]", "y[1/4]","x")


Y_df %>% gather(key = "Series", value = "Value",-x) %>% 
  ggplot(aes(x = x, y = Value, color = Series)) + 
  geom_line() + 
  scale_color_discrete(labels = c(expression(f), 
                                  expression(paste(f," * ",varphi[1])),
                                  expression(paste(f," * ",varphi[1/2])),
                                  expression(paste(f," * ",varphi[1/3])))) + 
  ggtitle(expression(paste("Plot of f and its convolution with ", varphi[1/n]))) +
  theme(legend.text = element_text(size = 12),
        legend.text.align = 0)
  
