
library(tidyverse)
library(fda)
library(ggtext)
(gaittime <- as.numeric(dimnames(gait)[[1]])*20)
gaitrange <- c(0,20)

apply(gait, 3, range)


harmaccelLfd <- vec2Lfd(c(0, (2*pi/20)^2, 0), rangeval=gaitrange)

gaitbasis <- create.fourier.basis(gaitrange, nbasis=21)

gaitLoglam <- seq(-4,0,0.25)
nglam   <- length(gaitLoglam)

gaitSmoothStats <- array(NA, dim=c(nglam, 3),
      dimnames=list(gaitLoglam, c("log10.lambda", "df", "gcv") ) )
gaitSmoothStats[, 1] <- gaitLoglam

#  loop through smoothing parameters

for (ilam in 1:nglam) {
  gaitSmooth <- smooth.basisPar(gaittime, gait, gaitbasis,
                   Lfdobj=harmaccelLfd, lambda=10^gaitLoglam[ilam])
  gaitSmoothStats[ilam, "df"]  <- gaitSmooth$df
  gaitSmoothStats[ilam, "gcv"] <- sum(gaitSmooth$gcv)
  # note: gcv is a matrix in this case
}

gaitfd <- smooth.basisPar(gaittime, gait,
       gaitbasis, Lfdobj=harmaccelLfd, lambda=1e-2)$fd
names(gaitfd$fdnames) <- c("Normalized time", "Child", "Angle")
gaitfd$fdnames[[3]] <- c("Hip", "Knee")



gaitfdPar  <- fdPar(gaitbasis, harmaccelLfd, lambda=1e-2)

gaitpca.fd <- pca.fd(gaitfd, nharm=4, gaitfdPar)

gaitpca.fd <- varmx.pca.fd(gaitpca.fd)

gaitmeanvec = gaitpca.fd$meanfd
gaitmeanvec = eval.fd(gaittime,gaitmeanvec)


gaitharmfd <- gaitpca.fd$harmonics
gaitharmmat <- eval.fd(gaittime, gaitharmfd)
eig = gaitpca.fd$values

mean= data.frame(hip  = gaitmeanvec[,,1], knee = gaitmeanvec[,,2])


iharm = 1
fac=sqrt(eig[iharm])
meanplus = mean + fac *data.frame(gaitharmmat[,iharm, 1], gaitharmmat[,iharm, 2])
mandmp = rbind(mean, meanplus)
time= gaittime
m = c(rep('mean',20),rep('meanplus',20))
fpcplot.df =  data.frame(time=time, mean=m, val = mandmp, FPC=rep(iharm,40))

for(i in 2:4){
  iharm=i
  meanplus = mean + fac *data.frame(gaitharmmat[,iharm, 1], gaitharmmat[,iharm, 2])
  mandmp = rbind(mean, meanplus)
  time= gaittime
  m = c(rep('mean',20),rep('meanplus',20))
  fpcplot.df.tmp =  data.frame(time=time, mean=m, val = mandmp, FPC=rep(iharm,40))
  fpcplot.df = rbind(fpcplot.df, fpcplot.df.tmp)
}



library(gganimate)
library(ggplot2)
gif.g <- fpcplot.df %>%
  ggplot(mapping=aes(x=val.hip, y=val.knee, label=time+0.5))+
  geom_path(data=. %>% filter(mean=='mean'), arrow=arrow(length=unit(0.5,"cm"),
                        type = "closed"), color='indianred', lwd=1.5)+
  geom_path(data=. %>% filter(mean=='meanplus'), color='black', linetype=3,lwd=1.3)+
  geom_point(data=. %>% filter(mean=='mean'), lwd=3)+
  geom_path(aes(group=time), arrow = arrow(length=unit(0.25,"cm"), type='closed'))+
  geom_label(data=. %>% filter(mean=='mean'))+
  labs(x="hip angle (deg)",
       y="knee angle (deg)",
       title = "Functional Principal Components Analysis (FPCA)",
       subtitle = "Adding a multiple of FPC{closest_state} to the mean gait cycle",
       caption = "data: library(fda) | viz: gunning_edward")+
  theme_bw()+
  theme(legend.position = 'none',
        plot.title.position = 'plot',
        plot.title = element_text(size=20, face='bold', family="Comic Sans MS" ),
        plot.subtitle = element_text(size=16, family="Comic Sans MS" ),
        axis.title = element_text(size=16, face='bold', family="Comic Sans MS"),
        axis.text = element_text(size=14, family="Comic Sans MS" ),
        plot.caption = element_text(size=12, family= "Comic Sans MS"))+
  transition_states(states=FPC)

animate(gif.g, fps=7, renderer = gifski_renderer(file="pca.gif",loop='F'))

