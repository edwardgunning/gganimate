library(tidyverse)
library(fda)
library(fda.usc)
library(extrafont)
library(ggExtra)


##### I'm just getting some random data here and making the y values nicely in the range 0,1
data(aemet,package = "fda.usc")
x = aemet$temp$argvals
temp = as.data.frame(aemet$temp$data,row.names=F)
range.tt = aemet$temp$rangeval
inv.temp = data.frame(t(aemet$temp$data)) 
names(inv.temp) = aemet$df$name 
y = inv.temp[["ALICANTE"]]/ max(inv.temp[["ALICANTE"]]) - 0.2 


my.basis = create.fourier.basis(rangeval = range(x),nbasis = 5)
my.smooth = smooth.basis(argvals = x, fdParobj = my.basis, y=y)
coef = my.smooth$fd$coefs


my.basis.eval = eval.basis(evalarg = 0.5:364.5, basisobj = my.basis)
sum = rowSums(my.basis.eval)

weighted.eval = my.basis.eval %*% diag(as.vector(coef))
weighted.sum = rowSums(weighted.eval)


n =length(x)
colnames(weighted.eval) <- colnames(my.basis.eval)

d1 = rbind(data.frame(x=x, my.basis.eval, state=rep("Unweighted",n)),
      data.frame(x=x, weighted.eval, state=rep("Weighted",n))) %>%
  melt(id.vars=c('x', 'state'), variable.name='basis')

d2 = rbind(data.frame(x=x,s=sum, state=rep("Unweighted",n)),
           data.frame(x=x,s=weighted.sum, state=rep("Weighted",n)))



p = d1 %>% ggplot()+
  geom_line(aes(x=x,y=value, group=basis, color=basis))+
  geom_point(data=data.frame(x=x,y=y), aes(x=x,y=y), alpha=0.2)+
  geom_line(data=d2, aes(x=x,y=s), lwd=1.2)+
  scale_color_discrete(name="Term", labels=c("Constant", "sin1", "cos1", "sin2", "cos2"))+
  labs(y="f(x)",caption = "@gunning_edward", title = "Functional Data Analysis (FDA)",
       subtitle = "The Fourier Basis System")+
  transition_states(state)+
  theme_classic()+
  theme(plot.title = element_text(size=18, family = "Courier New"),
        plot.subtitle = element_text(size=14, family = "Courier New" ),
        axis.title = element_text(size=14, family = "Courier New" ),
        axis.text = element_text(size = 12, family = "Courier New" ),
        plot.caption = element_text(size = 10, family = "Courier New"),
        legend.text = element_text(size = 12, family = "Courier New"),
        legend.title = element_text(size = 14, family = "Courier New"))
  
p
animate(p, renderer = gifski_renderer(file="fourier.gif",loop='F'))

