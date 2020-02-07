library(fda)
library(devtools)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(reshape2)

handwritx = data.frame(time= handwritTime,  handwrit[,,1]) 
handwrity = data.frame(time= handwritTime,  handwrit[,,2]) 

handwritx.mlt = handwritx %>% melt(id.vars='time', value.name = 'x', variable.name ='replicate')
handwrity.mlt = handwrity %>% melt(id.vars='time', value.name = 'y', variable.name ='replicate')

handwrit.join = inner_join(handwritx.mlt, handwrity.mlt, by = c("time", "replicate"))


gif.h = handwrit.join %>% ggplot(aes(x=x,y=y, group=replicate, color = replicate))+
  geom_line()+
  geom_point()+
  transition_reveal(along=as.numeric(time))+
  theme_classic()+
  scale_color_manual(values = c25)+
  labs(title= "Functional Data Analysis (FDA)",
       subtitle = "20 cursive samples of 1401 (x, y) coordinates of handwriting \"fda\"",
       caption = "data: library(fda) | viz: gunning_edward"
       )+
  theme(legend.position = 'none',
        plot.title.position = 'plot',
        plot.title = element_text(size=20, face='bold', family="Comic Sans MS" ),
        plot.subtitle = element_text(size=14, family="Comic Sans MS" ),
        axis.title = element_text(size=16, face='bold', family="Comic Sans MS"),
        axis.text = element_text(size=14, family="Comic Sans MS" ),
        plot.caption = element_text(size=12, family= "Comic Sans MS"))

animate(gif.h, fps=7, renderer = gifski_renderer(file="fda.gif",loop='F'))


