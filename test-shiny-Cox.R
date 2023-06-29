#######################################
# Initial set-up

rm(list=ls()); options(stringsAsFactors=F)
library(survival)


##################################
# Generate an example data set
#
set.seed(2023)
n=500
x1=rnorm(n)              # predictor x
u1=runif(n)              # predictor u
v1=rexp(n)               # predictor v
tm1=rexp(n)                  # event time
ev1=rbinom(n,1,0.5)          # event indicator
srv1=Surv(tm1,ev1)
grp1=sample(LETTERS[1:3],n,T)
sex1=sample(c("male","female"),n,T)
strta=rep(1:2,n/2)
strt2a=rep(LETTERS[1:2],each=n/2)


dset1=cbind.data.frame(grp=grp1,x=x1,
                      u=u1,v=v1,
                      sex=sex1,
                      ev=ev1,tm=tm1,
                      strt=strta,
                      srv=srv1)


x2=rnorm(n)              # predictor x
u2=runif(n)              # predictor u
v2=rexp(n)               # predictor v
tm2=rexp(n)                  # event time
ev2=rbinom(n,1,0.5)          # event indicator
srv2=Surv(tm2,ev2)
grp2=sample(LETTERS[1:3],n,T)
sex2=sample(c("male","female"),n,T)
strtb=rep(1:2,n/2)
strt2b=rep(LETTERS[1:2],each=n/2)


dset2=cbind.data.frame(grp=grp2,x=x2,
                       u=u2,v=v2,
                       sex=sex2,
                       ev=ev2,tm=tm2,
                       strt=strtb,
                       srv=srv2)

x3=rnorm(n)              # predictor x
u3=runif(n)              # predictor u
v3=rexp(n)               # predictor v
tm3=rexp(n)                  # event time
ev3=rbinom(n,1,0.5)          # event indicator
srv3=Surv(tm3,ev3)
grp3=sample(LETTERS[1:3],n,T)
sex3=sample(c("male","female"),n,T)
strtc=rep(1:2,n/2)
strt2c=rep(LETTERS[1:2],each=n/2)


dset3=cbind.data.frame(grp=grp3,x=x3,
                       u=u3,v=v3,
                       sex=sex3,
                       ev=ev3,tm=tm3,
                       strt=strtc,
                       srv=srv3)

x4=rnorm(n)              # predictor x
u4=runif(n)              # predictor u
v4=rexp(n)               # predictor v
tm4=rexp(n)                  # event time
ev4=rbinom(n,1,0.5)          # event indicator
srv4=Surv(tm4,ev4)
grp4=sample(LETTERS[1:3],n,T)
sex4=sample(c("male","female"),n,T)
strtd=rep(1:2,n/2)
strt2d=rep(LETTERS[1:2],each=n/2)


dset4=cbind.data.frame(grp=grp4,x=x4,
                       u=u4,v=v4,
                       sex=sex4,
                       ev=ev4,tm=tm4,
                       strt=strtd,
                       srv=srv4)
##########################################
# Fit a cox model to the example data set

cox.fit1=coxph(srv~sex+u+v+grp+strata(strt),
               data=dset1,
               x=TRUE,model=TRUE)

cox.fit2=coxph(srv~sex+u+v+grp+strata(strt),
               data=dset2,
               x=TRUE,model=TRUE)

cox.fit3=coxph(srv~sex+u+v+grp+strata(strt),
               data=dset3,
               x=TRUE,model=TRUE)

cox.fit4=coxph(srv~sex+u+v+grp+strata(strt),
               data=dset4,
               x=TRUE,model=TRUE)
########################################
# Remove example data set to ensure no inadvertent access to it while building app

# rm(dset1)
# rm(dset2)
# rm(dset3)
# rm(dset4)

########################################
# Generate an app for the example data set model

source("~/Projects/shinyCox/cox-predictions.R")
source("~/Projects/shinyCox/shiny-blocks.R")
source("~/Projects/shinyCox/shiny-builder.R")
app.dir="~/Project/ShinyCox/TestApps/"

test.res=shine.coxph(fit1 = cox.fit1,fit2 = cox.fit2,
                     fit3 = cox.fit3, fit4 = cox.fit4,# pass the coxph fit result
                     app.dir=app.dir,  # application directory
                     launch.app=T)     # indicates whether to launch the app after it is created



# REAL DATA EXAMPLE
##################################################################################


######################################################

# srv <- Surv(bladder$stop, bladder$event)
# rx1 <- as.factor(bladder$rx)
# 
# bladder.ph <- coxph(srv ~ rx1 + number + size, bladder, x = TRUE, 
#                     model = TRUE)
# 
# bladde.res <- shine.coxph(bladder.ph,
#                           app.dir = app.dir)

# 
# amltable <- data.frame(time = aml$time, status = aml$status, ex = aml$x,
#                        srv = Surv(aml$time, aml$status))
# 
# fit <- coxph(srv ~ ex, amltable, x = TRUE, model = TRUE)
# 
# aml.test <- shine.coxph(fit, app.dir=app.dir)


# 
# srv <- Surv(cgd$tstart, cgd$tstop, cgd$status)
# 
# cgd.fit <- coxph(srv ~ treat + sex + age + inherit, cgd, x = TRUE, model = TRUE)
# 
# test.cgd <- shine.coxph(only = cgd.fit, app.dir = app.dir)

