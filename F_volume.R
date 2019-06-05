#Visualization of Halfopen Intervals
library(tidyverse)

x = c(1,1,2,2)
y = c(1,2,1,2)

X = c(0,4)
Y = c(0,4)

plane = data.frame(X,Y)

plot_gg = ggplot(aes(x = X, y = Y),data = plane)

plot_gg + theme_void() + 
  geom_rect(aes(xmin = 1, ymin = 1, xmax = 2, ymax =2), alpha = 0.2)+
  geom_segment(aes(x = 1, y = 1, xend = 1, yend =2),linetype = "dashed") + 
  geom_segment(aes(x = 1, y = 1, xend = 2, yend =1),linetype = "dashed") + 
  geom_segment(aes(x = 2, y = 1, xend = 2, yend =2)) + 
  geom_segment(aes(x = 1, y = 2, xend = 2, yend =2)) + xlim(c(-0.5,7.5)) + ylim(c(-0.5,2.5)) +
  geom_segment(aes(x = -0.5, y = 0, xend = 2.5, yend = 0)) + 
  geom_segment(aes(y = -0.5, x = 0, yend = 2.5, xend = 0)) + 
  geom_point(aes(x = 1,y = 0), shape = 1) + 
  geom_point(aes(x = 2,y = 0)) +
  geom_point(aes(x = 0,y = 1), shape = 1) + 
  geom_point(aes(x = 0,y = 2)) + 
  annotate("text", x = -0.1, y = 1, label = "a", parse = T,family = "serif") +
  annotate("text", x = -0.1, y = 2, label = "b", parse = T,family = "serif") +
  annotate("text", x = 1, y = -0.1, label = "a", parse = T,family = "serif") +
  annotate("text", x = 2, y = -0.1, label = "b", parse = T,family = "serif") + 
  geom_point(aes(x = 1,y = 1), shape = 1, size = 2) + 
  geom_point(aes(x = 2,y = 1), size = 2) +
  geom_point(aes(x = 1,y = 2), size = 2) + 
  geom_point(aes(x = 2,y = 2), size = 2) +
  annotate("text", x = 6, y = (1+1)/2.5, label = "F(a,a)", parse = T, family = "serif") +
  geom_point((aes(x = 7, y = (1+1)/2.5))) + 
  annotate("text", x = 6, y = (1+2)/2.5, label = "F(a,b) == C(b,a) ", parse = T,family = "serif") +
  geom_point((aes(x = 7, y = (1+2)/2.5))) + 
  annotate("text", x = 6, y = (2+2)/2.5, label = "F(b,b)", parse = T,family = "serif") +
  geom_point((aes(x = 7, y = (2+2)/2.5))) +
  annotate("text", x = 3.75, y = 1.75, label = "F", parse = T,family = "serif") + 
  geom_curve(aes(x = 4.5, y = 1.15, xend = 3, yend = 1.15), arrow = arrow(type = "closed",length = unit(5,"pt"),ends = "first")) + 
  geom_segment(aes(x = 7, y = -0.5, xend = 7, yend = 2.5)) + 
  geom_point(aes(x = 7, y = 0), shape  =1)

  
