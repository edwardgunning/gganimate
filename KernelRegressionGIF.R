####### Packages Used
library(tidyverse)
library(gganimate)
library(KernSmooth)
library(extrafont)
library(refund)


#### defining data 
DTI.argvals = c(1:93)/93
DTI.example = DTI$cca[1,] # I've just picked the first subject

# Gaussian Kernel - could use an alternative
Kernel <- function(x,b){
  K <- (1/((sqrt(2*pi))))*exp(-0.5 *(x/b)^2)
  return(K)
}

#### Bandwidths we'll use
bandgrid = seq(0.07,0.3, by=0.0025)
### starting points
band=0.06
kern.evalpoints = seq(0,1, length.out = 93)
kern.outpoints = Kernel(0.5-kern.evalpoints, b=band)
smooth = locpoly(x=DTI.argvals, y=DTI.example, bandwidth = band, degree = 3)
smooth.x = kern.evalpoints
smooth.y = approx(x=smooth$x,y=smooth$y, xout=smooth.x)$y
nbhd.evalpoints = seq(0,1, length.out = 93)
nbhd.weights = Kernel(DTI.argvals-0.5, b=band)
nbhd.model = lm(DTI.example~ DTI.argvals + I(DTI.argvals^2)+I(DTI.argvals^3), weights = nbhd.weights)
newdata=data.frame(DTI.argvals=nbhd.evalpoints)
nbhd.outpoints = predict(nbhd.model, newdata = newdata)
newdata2 = data.frame(DTI.argvals=0.5)
point.est = predict(nbhd.model, newdata = newdata2)
k.data = data.frame(x.data= DTI.argvals,
                         y.data = DTI.example,
                         x.kern=kern.evalpoints,
                         y.kern = kern.outpoints,
                         x.smooth = smooth.x,
                         y.smooth = smooth.y,
                         x.nbhd = nbhd.evalpoints,
                         y.nbhd = nbhd.outpoints,
                         x.pointfive = rep(0.5,93),
                         y.pointfive= rep(point.est,93),
                         h = rep(band,93)) 
##### evaluate for different bandwidths

for(i in 1:length(bandgrid)){
  band = bandgrid[i]
  kern.evalpoints = seq(0,1, length.out = 93)
  kern.outpoints = Kernel(0.5-kern.evalpoints, b=band)
  smooth = locpoly(x=DTI.argvals, y=DTI.example, bandwidth = band, degree = 3)
  smooth.x = kern.evalpoints
  smooth.y = approx(x=smooth$x,y=smooth$y, xout=smooth.x)$y
  nbhd.evalpoints = seq(0,1, length.out = 93)
  nbhd.weights = Kernel(DTI.argvals-0.5, b=band)
  nbhd.model = lm(DTI.example~ DTI.argvals + I(DTI.argvals^2)+I(DTI.argvals^3), weights = nbhd.weights)
  newdata=data.frame(DTI.argvals=nbhd.evalpoints)
  nbhd.outpoints = predict(nbhd.model, newdata = newdata)
  newdata2 = data.frame(DTI.argvals=0.5)
  point.est = predict(nbhd.model, newdata = newdata2)
  k.data = rbind(k.data,data.frame(x.data= DTI.argvals,
                      y.data = DTI.example,
                      x.kern=kern.evalpoints,
                      y.kern = kern.outpoints,
                      x.smooth = smooth.x,
                      y.smooth = smooth.y,
                      x.nbhd = nbhd.evalpoints,
                      y.nbhd = nbhd.outpoints,
                      x.pointfive = rep(0.5,93),
                      y.pointfive= rep(point.est,93),
                      h = rep(band,93)))
}


##### create gif
k.gif <- k.data %>%
  ggplot(mapping = aes(x=x.kern, y=y.kern))+
  ylim(c(0,max(k.data$y.data)))+
  geom_point(aes(x=x.data, y=y.data), alpha=0.2)+
  geom_ribbon(aes(x=x.kern,ymax=y.kern), ymin=0, alpha=0.5, fill='indianred', color='black')+
  geom_line(aes(x=x.smooth, y=y.smooth), lwd=1.5)+
  geom_line(aes(x=x.nbhd, y= y.nbhd), color='cornflowerblue', lwd=2)+
  geom_vline(xintercept = 0.5, lty=2)+
  geom_point(aes(x=x.pointfive, y=y.pointfive), pch=4, lwd=4)+
  theme_classic()+
  labs(title = "Locally Weighted Polynomial Regression",
       subtitle = "Bandwidth={round(frame_time,2)}",
       x= "x",
       y="f(x)",
       caption="Data: library(refund) | Figure: @gunning_edward"
  )+
  theme(plot.title = element_text(size=18, family = "Courier New"),
        plot.subtitle = element_text(size=14, family = "Courier New" ),
        axis.title = element_text(size=14, family = "Courier New" ),
        axis.text = element_text(size = 12, family = "Courier New" ),
        plot.caption = element_text(size = 10, family = "Courier New" ))+
  transition_time(h)
  
k.gif  
#### save gif
animate(k.gif, renderer = gifski_renderer(file="Kern.gif",loop='F'))

