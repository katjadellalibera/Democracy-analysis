library(readxl)
data <- read_excel("Democracy Cross-National Data V4.1 09092015.xlsx")
for (i in 1:195) {data$Nation[i]<- data.frame(c(1971:2014))}
for (i in 1:195){data$Nation[i]<- c(data$Nation[i])}
gdp_fh_by_year<-data.frame(year=c(1971:2014),
                           )
data$Fhstand2000

library(ggplot2)
library(ggrepel)
# 2014
fit1<- lm(FHstand2014 ~ GDP2014,data=data)
ggplot(data, aes(x=GDP2014,y=FHstand2014))+
    geom_point()+
    scale_x_log10()+
    ggtitle("GDP per capita vs freedom house rating in 2014")+
    xlab("GDP per capita")+
    ylab("freedom house rating")+
    stat_smooth(method="lm",color="red")+
    labs(title = paste("2014:  Adj R2 = ",signif(summary(fit1)$adj.r.squared, 5),
                     "Intercept =",signif(fit1$coef[[1]],5 ),
                     " Slope =",signif(fit1$coef[[2]], 5),
                     " P =",signif(summary(fit1)$coef[2,4], 5)))
ggplot(data, aes(x=GDP2014,y=FHstand2014))+
  geom_point()+
  scale_x_log10()+
  ggtitle("GDP per capita vs freedom house rating in 2014")+
  xlab("GDP per capita")+
  ylab("freedom house rating")+
  stat_smooth(method="lm",color="red")+
  labs(title = paste("2014:  Adj R2 = ",signif(summary(fit1)$adj.r.squared, 5),
                     "Intercept =",signif(fit1$coef[[1]],5 ),
                     " Slope =",signif(fit1$coef[[2]], 5),
                     " P =",signif(summary(fit1)$coef[2,4], 5)))+
  geom_label_repel(aes(label = Natlabel),
                   box.padding   = 0.05, 
                   point.padding = 0.2,
                   segment.color = 'grey50')

#1975
ggplot(data, aes(x=GDP1975,y=FHstand1975))+
  geom_point()+
  scale_x_log10()+
  ggtitle("GDP per capita vs freedom house rating in 1975")+
  xlab("GDP per capita")+
  ylab("freedom house rating")+
  geom_smooth(method="lm")

#2000
ggplot(data, aes(x=GDP2000,y=Fhstand2000))+
  geom_point()+
  scale_x_log10()+
  ggtitle("GDP per capita vs freedom house rating in 2000")+
  xlab("GDP per capita")+
  ylab("freedom house rating")+
  geom_smooth(method="lm")


ggplot()+
  geom_point(data=data, 
             aes(x=GDP1975,y=FHstand1975,
                 color="1975"))+
  geom_point(data=data, 
             aes(x=GDP2000,y=Fhstand2000,
                 color="2000"))+
  geom_point(data=data,
             aes(x=GDP2014,y=FHstand2014,
                 color="2014"))+
  scale_x_log10()+
  xlab("GDP per capita")+
  ylab("freedom house rating")+
  labs(color="year")
