rm(list=ls())  # clears everything out of work space
setwd("C:\\Users\\Ken\\Google Drive\\Courses\\eH705\\K705 Data\\Retinol") # sets working directory
Retinol<- read.csv("http://cybr.mcmaster.ca/eH705_W2019/Retinol_22jan16.csv", sep=",", header=TRUE) # reading in the data file
head(Retinol)
attach(Retinol)
str(Retinol)

# Homogeneity of Variance test and Plot
# install.packages("HH")
library(HH)
hov( Retplasma~  as.factor(Smokstat),  data=Retinol)
hovPlot(Retplasma~ as.factor(Smokstat), data=Retinol)

bartlett.test(Retplasma ~ as.factor(Smokstat), data=Retinol)

fligner.test(Retplasma ~ as.factor(Smokstat), data=Retinol)

#_________________________________
install.packages("lawstat")
# library(lawstat)
load("Retinol.rda")
attach(Retinol)
levene.test(Retplasma, Smokstat, location=c("median"),
            bootstrap=FALSE,  num.bootstrap=1000, kruskal.test=FALSE,
            correction.method=c("none") )

levene.test(Retplasma, Smokstat, location=c("median"),
            bootstrap=FALSE,  num.bootstrap=1000, kruskal.test=TRUE,
            correction.method=c("none") )

levene.test(Retplasma, Smokstat, location=c("median"),
            bootstrap=TRUE,  num.bootstrap=1000, kruskal.test=FALSE,
            correction.method=c("none") )

plot.design(Retplasma ~ Smokstat, data = Retinol)
#_____________________________________________________

## univariate normality
rr <- Retinol[ ,c(4, 14,15)] # moves columns 14 & 15 to r
head(rr)
# install.packages("MVN", dependencies=T)
library(MVN)
mvn(rr[-1], univariatePlot = "qqplot") # creates univariate Q-Q plots
mvn(rr[-1], univariatePlot = "histogram") # creates univariate histograms
mvn(rr[-1], univariateTest = "SW", desc = TRUE) # Shapiro-Wilk univariate, descriptives

# following are tests for multivariate normality
# i.e., Ho: the distributions are normal in concert (Betaplasma and Retplasma)

mvn(rr[-1], mvnTest="mardia", univariatePlot = "qqplot") # Mardia test for MVN
mvn(rr[-1], mvnTest="hz") # Henze-Zirkler's 
mvn(rr[-1], mvnTest="royston") # Royston's test for MVN
mvn(rr[-1], multivariatePlot = "persp") # perspective plot
mvn(rr[-1], multivariatePlot = "contour") # contour plot

mvn(data=rr, subset = "Smokstat", mvnTest="hz", univariateTest = "SW", univariatePlot = "qqplot") # Henze-Zirkler's 
mvn(data=rr, subset = "Smokstat", mvnTest = "hz", univariatePlot = "histogram"  )

# since normality is rejected based on the analysis above, conduct the Kruskal-Wallis test.
kruskal.test(Retplasma ~ Smokstat, data = Retinol)
