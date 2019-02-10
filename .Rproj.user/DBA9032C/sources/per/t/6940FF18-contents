# import file using readxl library
library(readxl)
data <- read_excel("Democracy Cross-National Data V4.1 09092015.xlsx")

#import ggplot2 and ggprepel(for labels)
library(ggplot2)
library(ggrepel)

# fit a linear regression on the data
fit1<- lm(FHstand2014~GDP2014,data=data)
# plot the correlation between GDP per capita and freedom house rating in 2014
# include country labels
ggplot(data, aes(x=GDP2014,y=FHstand2014))+
    geom_point()+
    scale_x_log10()+
    ggtitle("GDP per capita vs freedom house rating in 2014")+
    xlab("GDP per capita")+
    ylab("freedom house rating")+
    stat_smooth(method="lm",color="red")+
    geom_label_repel(aes(label=Nation),
                    box.padding = 0.1,
                    point.padding = 0.2,
                    segment.color = "grey50")+
    labs(title = paste("Adj R2 = ",signif(summary(fit1)$adj.r.squared, 5),
                     "Intercept =",signif(fit1$coef[[1]],5 ),
                     " Slope =",signif(fit1$coef[[2]], 5),
                     " P =",signif(summary(fit1)$coef[2,4], 5)))
# plot the correlation without labels
ggplot(data, aes(x=GDP2014,y=FHstand2014))+
  geom_point()+
  scale_x_log10()+
  ggtitle("GDP per capita vs freedom house rating in 2014")+
  xlab("GDP per capita")+
  ylab("freedom house rating")+
  stat_smooth(method="lm",color="red")+
  labs(title = paste("Adj R2 = ",signif(summary(fit1)$adj.r.squared, 5),
                     "Intercept =",signif(fit1$coef[[1]],5 ),
                     " Slope =",signif(fit1$coef[[2]], 5),
                     " P =",signif(summary(fit1)$coef[2,4], 5)))

# combining data from 1975, 1984, 2000, and 2014 in one plot
ggplot()+
  geom_point(data=data, 
             aes(x=GDP1975,y=FHstand1975,
                 color="1975"))+
  geom_point(data=data,
             aes(x=GDPPC1984,y=FHStand1984,
                 color="1984"))+
  geom_point(data=data, 
             aes(x=GDP2000,y=Fhstand2000,
                 color="2000"))+
  geom_point(data=data,
             aes(x=GDP2014,y=FHstand2014,
                 color="2014"))+
  scale_x_log10()+
  xlab("GDP per capita")+
  ylab("freedom house rating")+
  labs(color="year")+
  geom_smooth(method="lm")
