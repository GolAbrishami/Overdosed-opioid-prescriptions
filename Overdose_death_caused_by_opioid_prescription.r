
library("ggplot2")
library(dplyr)
library(psych)
library(quantmod)
library(MASS)

OD <- read.csv("OpiodDeath.csv", head=T)
OD <- data.frame(OD)
head(OD)

colnames(OD) <-c("s","year","death","pop","c","cl","cu","p")

ODC <- OD[complete.cases(OD), ]

ODC$nd <- (ODC$pop - ODC$death)

summary(ODC)

lbl_plot <- ggplot(ODC,aes(x=p))
lbl_plot + geom_histogram(fill="#de2d26", alpha=0.4) +
  stat_bin(geom="text", aes(label=..count.., angle=15),
           hjust=0.4, vjust=-0.5) + 
  labs(title= "Distribution of Opiod prescriptions\nFrom Years 1999 - 2014",
       x = "Number of Opiod Perception") +
  theme(plot.title= element_text(size=rel(1.8), color="brown"),
        axis.title.x= element_text(size=rel(1.2)),
        axis.title.y= element_text(size=rel(1.2)),
        axis.text.x= element_text(angle=30)) +
  ylim(0,140)


ind1= which (ODC$s=="Alabama")
ind2= which (ODC$s=="Arizona")
ind3= which (ODC$s=="California")
ind4= which (ODC$s=="Florida")
ind5= which (ODC$s=="Hawaii")
ind6= which (ODC$s=="Iowa")
plot(ODC$year[ind1],ODC$c[ind1],main="Crude rate each year in some states",
     ylab="Crude Rate",xlab="Year",type="l",cex.lab=1.25,cex.axis=0.8,xlim=c(1999,2015),
    ylim=c(0,15))
lines(ODC$year[ind2],ODC$c[ind2],col=2)
lines(ODC$year[ind3],ODC$c[ind3],col=3)
lines(ODC$year[ind4],ODC$c[ind4],col=4)
lines(ODC$year[ind5],ODC$c[ind5],col=5)
lines(ODC$year[ind6],ODC$c[ind6],col=6,lty=2)
legend(1999,15,legend=c("Alabama","Arizona","California","Florida","Hawaii","Iowa"),col=1:6,lty=c(rep(1,5),2))



hist(ODC$p, freq=FALSE, col="#feb24c", main="Histogram of opiod prescriptions",
     xlab="Perception Numbers")
curve(dnorm(x, mean=mean(ODC$p), 
            sd=sd(ODC$p)), add=TRUE, col="#f03b20") 


ODC_plt <- ggplot(ODC, aes(factor(p),c, fill=p))
ODC_plt +  geom_boxplot(width = 0.3, lwd = 1.3, outlier.size = 3) +
  geom_violin(alpha = 0.4) +
labs(title="Does Opioide Perceptions raise the Crude Rate",
       y="Crude Rate",
       x="Number of Opioide Perception") +
  theme(plot.title= element_text(size=rel(1.8), color="darkblue"),
        axis.title.x= element_text(size=rel(1.4)),
        axis.title.y= element_text(size=rel(1.6)),
        axis.text.y= element_text(angle=30, size = rel(2)))

boxplot(ODC[ODC$year=="1999",]$c,
        ODC[ODC$year=="2003",]$c,
        ODC[ODC$year=="2008",]$c,
        ODC[ODC$year=="2013",]$c,main="Average number of crude rate",
        names=c("1999","2003","2008","Mid 2013"),
        col=(c("gold","darkgreen","magenta","maroon")))

ODC_ch <- ODC %>% 
group_by(s) %>%
    summarize(nd=median(nd))

head(ODC)

reg <- lm(c ~.-s,data=ODC)
summary(reg)

plot(ODC$c ~ ODC$p,
     main="Correlation between Crude Rate and Opiod",
    xlab="Number of Opioide Perception",
    ylab="Crude Rate",pch=22,col="red")

mod1 = lm(formula = c ~ p, data = ODC)
abline(mod1, col = 'blue', lwd = 2, lty="dashed")


pairs.panels(ODC[-c(5,6,7,10)])

reg = lm(formula = c ~ p, data = ODC)
plot.new()
par(mfrow = c(2, 2))
plot(reg)
par(mfrow = c(1, 1))

trans = boxcox(reg)
trans_df = as.data.frame(trans)
optimal_lambda = trans_df[which.max(trans$y),1]
print(optimal_lambda)
reg = lm(formula = c^optimal_lambda ~ log(p), data = ODC)
plot.new()
par(mfrow = c(2, 2))
plot(reg)
par(mfrow = c(1, 1))
