library(RCurl)
#When reading data from github, we should pass in the raw version of the data in read.csv(), 
#We should get the URL for the raw version by clicking on the Raw button displayed above the data.
x <- getURL("https://raw.githubusercontent.com/jazzsun000/how-to-build-statistical-distribution-analysis-on-wage-data/master/Wages.csv")
Wages <- read.csv(text = x)

summary(Wages)
#1.Highlight three descriptive facts from the data with supporting analysis and graphs.
##1.exp:years of full-time work experience.
h = hist(Wages$exp) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE,col="red")

##2.wks:weeks worked.
h1 = hist(Wages$wks) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h1$density = h1$counts/sum(h1$counts)*100
plot(h1,freq=FALSE,col="blue")

##3.ed:years of education.
h2 = hist(Wages$ed) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h2$density = h2$counts/sum(h2$counts)*100
plot(h2,freq=FALSE,col="green")

#2.Pick a continuous variable of interest, what is the distribution of this variable?

hist(Wages$lwage, breaks=12, col="gray")
d <- density(Wages$lwage) # returns the density data 
plot(d) # plots the results

#clean outlier
#For a given continuous variable, outliers are those observations that lie outside  1.5 * IQR, where IQR, the ???Inter Quartile Range??? is the difference between 75th and 25th quartiles. Look at the points outside the whiskers in below box plot
outlier_values <- boxplot.stats(Wages$lwage)$out  # outlier values.
boxplot(Wages$lwage, main="logarithm of wage.", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)
x<-Wages$lwage
result = x[!x %in% boxplot.stats(x)$out]
hist(x, breaks=12, col="red")
hist(result, breaks=12, col="blue")
library(fitdistrplus)
plot(result, pch=20)
plotdist(result, histo = TRUE, demp = TRUE)
descdist(result, discrete=FALSE, boot=500)

#Say, in the previous eg, we chose the weibull, gamma and log-normal to fit:
fitdistr(result, "gamma")
ks.test (result, "pgamma", shape=250.81658478, rate=37.5848940 )
fit_w  <- fitdist(result, "weibull")
fit_g  <- fitdist(result, "gamma")
fit_ln <- fitdist(result, "lnorm")
summary(fit_ln)

#we can plot the results:
par(mfrow=c(2,2))
plot.legend <- c("weibull", "lognormal", "gamma")
denscomp(list(fit_w, fit_g, fit_ln), legendtext = plot.legend)
cdfcomp (list(fit_w, fit_g, fit_ln), legendtext = plot.legend)
qqcomp  (list(fit_w, fit_g, fit_ln), legendtext = plot.legend)
ppcomp  (list(fit_w, fit_g, fit_ln), legendtext = plot.legend)
#install.packages(flexsurv)
library(flexsurv) # on CRAN

gengammafit  <-  fitdistrplus::fitdist(result, "gengamma",
                                       start=function(d) list(mu=mean(d),
                                                              sigma=sd(d),
                                                              Q=0))

qqcomp(list(fit_g, fit_ln, fit_w, gengammafit),legendtext=c("gamma", "lnorm", "weibull", "gengamma") )
#But if take a further more look on above plot.

#None of the distributions fit very well in the right (upper) tail, but the generalized gamma is best

#And if we take a look from AIC, generalized gamma distribution show better than the other model.

#AIC comparison
gengammafit$aic
fit_w$aic
fit_g$aic
fit_ln$aic

#A good model is the one that has minimum AIC among all the other models. 
#And if we take a look from AIC, generalized gamma distribution show better than the other model.

