library(ggplot2)
library(ggmanh)#Bioconductor qq-plot GWAS
#https://bioconductor.org/packages/release/bioc/vignettes/ggmanh/inst/doc/ggmanh.html
library(precrec)
#https://cran.r-project.org/web/packages/precrec/vignettes/introduction.html
n=1000
set.seed(123)
f=runif(n)

ppii=0.1
y=rep(0,n)
y[sample(1:n,n*ppii)]=1
y

df=data.frame(f=f, y=y)
ggplot(data=df,aes(x=f))+geom_density()+facet_wrap(~y)

sscurves <- evalmod(scores = df$f, labels = df$y)
autoplot(sscurves)
autoplot(sscurves, "PRC")
sscurves.df <- as.data.frame(sscurves)
knitr::kable(head(sscurves.df))
str(sscurves.df)
sscurves[sscurves$type=="PRC"]
aucs <- auc(sscurves)
aucs_prc <- subset(aucs, curvetypes == "PRC")
aucs_prc
mpoints <- evalmod(scores = df$f, labels = df$y, mode = "basic")
autoplot(mpoints, c("specificity", "sensitivity", "precision"))
mpoints
# ser ut som de bytter y=0 med y=-1
# hva er normalized rank?
# kan vi studere vår syk-andel i detalj og vise at det er viktig med PRC og ikke ROC?

# Lag en bedre klassifikator, ved å trekke 0/1 fra mindre p-verdier
ff=sort(f)
id=sort(c(sample(1:round(n*(1-ppii)),round(n*ppii/2)),
     sample(round(n*(1-ppii)):n,round(n*ppii/2))))
y1=rep(0,n)
y1[id]=1
y1
sscurves1 <- evalmod(scores = ff, labels = y1)
sscurves1[sscurves1$type=="PRC"]
autoplot(sscurves1)
aucs1 <- auc(sscurves1)
aucs_prc1 <- subset(aucs1, curvetypes == "PRC")
aucs_prc1

# alle de med høyest verdi er y=1
y1=rep(0,n)
y1[round(n*(1-ppii)):n]=1
y1
sscurves1 <- evalmod(scores = ff, labels = y1)
sscurves1[sscurves1$type=="PRC"]
autoplot(sscurves1)
aucs1 <- auc(sscurves1)
aucs_prc1 <- subset(aucs1, curvetypes == "PRC")
aucs_prc1

# er metoden bedre enn tilfeldig gjetting?
# kan teste om f(x) har lik fordeling for Y=0 og Y=1
# mann whitney independent samples test

wilcox.test(df$f[df$y==0],df$f[df$y==1],paired = FALSE)
# dette er ok test slik at man trenger ikke egen test for at PRC er som random guessing

# CI for PR-kurven - punktvis?


# ikke direkte relevant, men også interessant å se om noe kan brukes av simuleringene over i et annet prosjekt
qqunif(df$f,conf.int=0.95)
mu=0;sigma=1
n1=5000
n2=5000
ff=rnorm(n1,mu,sigma)

