library(tidyverse) # CRAN v1.3.0
library(gganimate) # CRAN v1.0.5
library(reshape2) # CRAN v1.4.4
library(extrafont) # CRAN v0.17

samples <- data.frame(number=1:200, mean=numeric(200), lower=numeric(200), upper=numeric(200))
higher <- qt(p = 0.975, df = 9, lower.tail = T)
lower <- qt(p = 0.025, df = 9, lower.tail = T)
set.seed(2070)
for(i in 1:200){
  
  tmp_sample = rnorm(n = 10, mean = 10, sd = 5)
  
  samples$mean[i] = mean(tmp_sample)
  
  samples$lower[i] = mean(tmp_sample) +  lower * sd(tmp_sample)/sqrt(10)
  
  samples$upper[i] = mean(tmp_sample) +  higher * sd(tmp_sample)/sqrt(10)
}



samples <- samples %>% 
  mutate(result=ifelse(10>=lower & 10 <= upper, "inside", "outside")) %>%
  melt(measure.vars=c("mean", "upper", "lower"), variable.name="sample_stat")

samples %>% group_by(result) %>% summarise("%"=n()/600)


df <- data.frame(x1 = 11, x2 = 20, y1 = 0.04, y2 = 0.05)
df2 <- data.frame(x1 = 11, x2 = 18.5, y1 = 0.005, y2 = 0.0275)
p <- ggplot(data=samples)+
  stat_function(fun = dnorm, geom="area", xlim = c(-10,30), args=list(mean=10, sd=5), fill="lightgrey", alpha=0.4, color="black")+
  theme_minimal()+
  geom_vline(xintercept = 10, linetype="dashed")+
  geom_curve(aes(x=x2, xend=x1, y=y2, yend=y1), data=df, arrow = arrow(), curvature = -0.4)+
  annotate(geom="text", x=20.85, y=0.0575, label=" μ = 10 \n \"parameter of interest\" \n this is fixed", family="Comic Sans MS", size=5)+
  geom_curve(aes(x=x2, xend=x1, y=y2, yend=y1), data=df2, arrow = arrow(), curvature = 0.4)+
  annotate(geom="text", x=24.85, y=0.03, label="\"95% CI for μ\" \n different for every sample", family="Comic Sans MS", size=5)+
  geom_line(aes(x=value, y=0.002, color=result, group=factor(number)), lwd=1.5)+
  geom_point(aes(x=value, y=0.002, color=result), pch="|", size=5, data= . %>% filter(sample_stat=='lower'))+
  geom_point(aes(x=value, y=0.002, color=result), pch="|", size=5, data= . %>% filter(sample_stat=='upper'))+
  geom_point(aes(x=value, y=0.002, color=result), size=5, pch=18, data= . %>% filter(sample_stat=='mean'))+
  annotate(geom="text", x=24.85, y=0.02, label="Under Repeated Sampling:", family="Comic Sans MS", size=4.5)+
  annotate(geom="text", x=24.85, y=0.015, label="95% of CIs will contain μ", family="Comic Sans MS", size=4.5, color="navy")+
  annotate(geom="text", x=24.85, y=0.01, label="5% of CIs will not", family="Comic Sans MS", size=4.5, color="indianred")+
  transition_states(states = factor(number))+
  theme(legend.position = "none")+
  theme(axis.title = element_text(family = "Comic Sans MS", size=15),
        plot.title = element_text(family = "Comic Sans MS", size=18, hjust = 0.5),
        plot.subtitle = element_text(family = "Comic Sans MS", size=14, hjust = 0.5),
        axis.text = element_text(family = "Comic Sans MS", size=12))+
  scale_color_manual(values=c("navy", "indianred"))+
    labs(title = "\"A 95% Confidence Interval\"",
         subtitle = "Sample Number {previous_state} of size n=10 from N(10,5)",
         x="x",
         y="pdf(x)")+
  coord_cartesian(clip = "off")

animate(p, nframes = 400, width=700, height=600)
anim_save(filename = "confint.gif")  





