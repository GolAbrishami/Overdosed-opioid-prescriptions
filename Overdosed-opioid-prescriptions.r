
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

ODC[is.na(ODC),]
sum(apply(ODC,2,is.nan))


ODC$nd <- (ODC$pop - ODC$death)

scaled.dat <- scale(ODC[,c(3,4,8,9)])

colMeans(scaled.dat)
apply(scaled.dat, 2, sd)
IDC_Norm<-cbind(ODC[,c(1,2,5,6,7)],scaled.dat)
head(IDC_Norm,3)

summary(ODC)

lbl_plot <- ggplot(ODC,aes(x=c))
lbl_plot + geom_histogram(fill="#de2d26", alpha=0.4) +
  stat_bin(geom="text", aes(label=..count.., angle=15),
           hjust=0.4, vjust=-0.5) + 
  labs(title= "Distribution of Crude Rate, From Years 1999 - 2014",
       x = "Crude Rate") +
  theme(plot.title= element_text(size=rel(1.8), color="brown"),
        axis.title.x= element_text(size=rel(1.2)),
        axis.title.y= element_text(size=rel(1.2)),
        axis.text.x= element_text(angle=30)) +
  ylim(0,140)


hist(ODC$p, freq=FALSE, col="#5ab4ac", main="Distribution of opioid prescription,1999 - 2014",
     xlab="Prescription Numbers")
curve(dnorm(x, mean=mean(ODC$p), 
            sd=sd(ODC$p)), add=TRUE, col="#f03b20") 


unique(ODC$s)

high_drug=ODC[ODC$s %in% c('Missouri','New Hampshire','New Mexico','Michigan','West Virginia'),] 
head(high_drug,3)

theme_improve <- function(my_plot, title_color= 'brown',
                           title_size= 2, axis_title_size=1.5,
                           axis_text_size=1.3,
                           x_axis_text_angle=30) {
  my_plot = my_plot +
  theme(plot.title = element_text(size = rel(title_size),
                                  color = title_color),
        axis.title.y = element_text(size = rel(axis_title_size)),
        axis.title.x = element_text(size = rel(axis_title_size)),
        axis.text.x = element_text(size = rel(axis_text_size),
                                   angle = x_axis_text_angle),
        axis.text.y = element_text(size = rel(axis_text_size))
        )
  return(my_plot)
}


my_plt <- ggplot(high_drug, aes(x=year, y=c))
(my_plt +
  geom_line(aes(color = factor(s))) +
  labs(y='Crude Rate',x="Year",
       fill='states',
       title="Crude rate's raise in the high_drug_use states,\n1999 - 2014") +
  theme(legend.position='bottom'))%>%
theme_improve

which (ODC$s=="West Virginia")

ind5
ODC$year[ind1]

ODC_plt <- ggplot(ODC, aes(factor(p),c, fill=p))
ODC_plt +  geom_boxplot(width = 0.3, lwd = 1.3, outlier.size = 3) +
  geom_violin(alpha = 0.4) +
labs(title="Do Opioid Prescriptions Raise the Crude Rate?",
       y="Crude Rate",
       x="Number of Opioid Prescriptions",
    fill="Prescriptions") +
  theme(plot.title= element_text(size=rel(1.8), color="darkblue"),
        axis.title.x= element_text(size=rel(1.4)),
        axis.title.y= element_text(size=rel(1.6)),
        axis.text.y= element_text(angle=30, size = rel(2))) 


ODC_ch <- ODC %>% 
group_by(s) %>%
    summarize(nd=median(nd))

head(ODC_ch)

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
