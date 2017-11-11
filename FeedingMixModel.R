library(siar)
library(plotrix)
IsotopeR()

setwd("~/UVM/Mysis Metapopulation/Feeding Experiment")
consumer <-read.csv("consumer.csv")
sources <- read.csv("sourcesiar.csv")
tef <- read.csv("tef.csv")

model1 <- siarmcmcdirichletv4(consumer,sources,tef,concdep=0,500000,50000)

siarplotdata(model1, leg=1)
axis.break(-4,-3,style="slash")
par(mfrow=c(1,1))
siarproportionbygroupplot(model1,prn=TRUE,grp=2,probs=c(5,25,75,95))
axis.break(-4,-3,style="slash")
siarproportionbysourceplot(model1)

siarproportionbysourceplot(model1,prn=TRUE,grp=2,probs=c(5,25,75,95))


############## circ stats#########
library(FSA)
library(CircStats)
library(circular)


setwd("C:/Users/Peuclide/OneDrive/Documents/UVM/Mysis Metapopulation/Feeding Experiment")
d <- read.csv("circdataFE1-28b.csv",header=TRUE)
head(d)
GA <- d[d$mysis=="GA",c("a1d")]
BS <- d[d$mysis=="BS",c("a1d")]
GABS <- d[d$mysis=="ga-bs",c("a1d")]
sGA <- d[d$mysis=="sGA",c("a1d")]
sBS <- d[d$mysis=="sBS",c("a1d")]

r1 <- circular(GA,type="angles", units="degrees",template="geographics")
r2 <- circular(BS,type="angles", units="degrees",template="geographics")
r3 <- circular(GABS,type="angles", units="degrees",template="geographics")
r4 <- circular(sGA,type="angles", units="degrees",template="geographics")
r5 <- circular(sBS,type="angles", units="degrees",template="geographics")

plot(r4, cex=1.5, bin=720, stack=TRUE, sep=0.035, shrink=.8, axes=FALSE, xlim=c(-1.5,2.3), ylim=c(-1.5,1.5), pch=16)
points(r5,cex=1.5, bin=720, stack=TRUE, sep=0.035, col="black",pch=1)
# annotate north
text(0, 1.2, '+ ‰N', cex=1.0, font=1)
# annotate west
text(1.38,0, '+ ‰C', cex=1.0, font=1)

arrows.circular(r1, y = d[d$mysis=="GA","r1"], x0 = 0, y0 = 0, col="black",lty=1)
arrows.circular(r2, y = d[d$mysis=="BS","r1"], x0 = 0, y0 = 0, col="black",lty=5)

# food source arrows

arrows.circular(r4, y = d[d$mysis=="sGA","r1"], x0 =0, y0 = 0, col="black",lty=1)
arrows.circular(r5, y = d[d$mysis=="sBS","r1"], x0 = 0, y0 = 0, col="black",lty=5)


#arrows.circular(r3, y = d[d$mysis=="ga-bs","r1"], x0 = 0, y0 = 0, col="black",lty=5)

legend(1.0,1.2,c("Green Algae", "Artemia"), 
      pch = c(16,1),col = c("black","black","black"))

#################### Circplot in plotrix  #############################################################



#---------------------- round 3, plottrix--------------------

library(plotrix)
labs <- c("", "", "", "")

rads <- c( 0,-2.03285128,0,-1.45017266)
lens <- c(0,0.2906888,0,0.3324154)

radial.plot(lengths=lens, radial.pos= rads, show.grid.labels=T, start=0, radial.lim=c(0,.8), lwd=2, lty=c(1,1,2,2), add=T,labels=labs, grid.col="dimgrey")
radial.plot(lengths=lens2, radial.pos=rads2, rp.type="s", point.symbols= c(1,1,16,16),add=F, start=0)
legend(.15,.35,c("Green Algae", "Artemia"), 
       pch = c(16,1),col = c("black","black","black"))


rads2 <- c(0, -0.8518, 0, -0.78573)
lens2 <- c(0,.6, 0,.8)
lens2 <- c(0,5, 0,22)

deg <- c(0,-72.34988, 0, -39.559668)
polar.plot(lengths=lens, polar.pos=deg, show.grid.labels=T, start=1.57, radial.lim=c(0,1), lwd=2, lty=c(1,1,2,2), grid.col="dimgrey")
