library(FSA)
library(car)
library(multcomp)
library(plotrix)
library(base)
library(plyr)


setwd("~/UVM/Mysis Metapopulation/Feeding Experiment")
dat <-read.csv("FE620.csv")
str(dat)

d2 <-Subset(dat,Comments=="") #remove comments
d3 <- Subset(d2, Date%in% c("1/20/2014","1/21/2014","1/22/2014")) # selects experimental orgs
d4 <- Subset(d2, Date%in% c("1/3/2014","1/20/2014"))
d5 <- Subset(d2, Date=="1/3/2014")
d1 <- Subset(d3,GC%in% c("BS", "Algae", "Empty")) #removes food source
str(d1)
d <- d1

###Summary data
####finds mean(mx) standard error (sx) and standard deviation(sdx) for each group.
summaryd <- ddply(d, 'GC', function(x) c(n=mean(x$N),c=mean(x$C),wtN=mean(x$WN),mwC=mean(x$WC),
              mCN=mean(x$C.N), sn=(sd(x$N)/sqrt(length(x$N))),sc=(sd(x$C)/sqrt(length(x$C))),
              swN=(sd(x$WN)/sqrt(length(x$WN))),swC=(sd(x$WC)/sqrt(length(x$WC))),
              sCN=(sd(x$C.N)/sqrt(length(x$C.N))), sdN=sd(x$N),sdC=sd(x$C),
              sdwN=sd(x$WN),sdwC=sd(x$WC),sdCN=sd(x$C.N), nrow=nrow(x)))

summaryd <- ddply(d5, 'GC', function(x) c(len=mean(x$length), sdlen=sd(x$length), fecund=sum(x$ff=="y"),nfecund=sum(x$ff=="n"),n=mean(x$N),c=mean(x$C),mCN=mean(x$C.N), sn=(sd(x$N)/sqrt(length(x$N))),sc=(sd(x$C)/sqrt(length(x$C))),
                                         sCN=(sd(x$C.N)/sqrt(length(x$C.N))), sdN=sd(x$N),sdC=sd(x$C),
                                         sdCN=sd(x$C.N), nrow=nrow(x)))
summaryd

a <- summaryd$fecund
b <- summaryd$nfecund
tbl <- cbind(a,b)
chisq.test(tbl)
## ANCOVA
results <- lm(data=d4, N~length+Date)
t <- (anova <- anova(results))
str(d4)
outlierTest(results)
leveneTest(results)
### box plots
par(mfrow=c(1,1)) 

plot(C~Date+Tank, data=d4, main="FE N" )


## Idicator variable (ANCOVA)

lm3 <- lm(C~length+GC, data=d1) ## testing for difference in slope, sig interaction means sig dif in slope
(results <- anova(lm3))

summary(lm3)
confint(lm3)
fitPlot(lm3)

## Tukeys test
GC <- glht(lm3,mcp(GC="Tukey"))
summary(GC)

cld(GC)

confint(GC)

(sum <- Summarize(C~GC,data=d1, digits=1)) #summarizes data

sum <- within(sum,{ 
  LCI <- mean-1.96*sd/sqrt(n)
  UCI <- mean+1.96*sd/sqrt(n)
})

sum


with(sum, plotCI(1:3, mean, ui=UCI, li=LCI, pch=16, xlim=c(.5,3.5), xaxt="n",
                 ylim=c(-28.5,-26.5), xlab="GC", ylab="C"))
axis(1,1:3,c("Algae","BS","Empty"))
text(1:3,sum$mean,c("a","a","a"), pos=c(4,4))
text(1:3,3.8,paste("n=",sum$n,sep=""))
#
lm3 <- lm(C.N~length+GC, data=d) ## testing for a difference in intercepts, looking at p value for main effects, ANCOVA
anova(lm3)
summary(lm3)
confint(lm3)
pl <- fitPlot(lm3, ylab="C:N", xlab="", pch=1, lwd=1.5,  col = c("gray","gray20","gray50"), legend="bottom")


plot(lm3)

fitPlot(lm3, plot.pts = TRUE, class=IVR,pch = c(18, 16, 15),  lty = c(1,4,5), lwd = 3,
        main = "", legend = "topright")
summary(lm3)
