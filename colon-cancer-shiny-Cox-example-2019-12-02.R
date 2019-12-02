
# Install and load the survival package
inst.pack=installed.packages()
if (!any(rownames(inst.pack)=="survival")) install.packages("survival")
library(survival)

# Load and preview the colon cancer data set and its documentation
data(colon)
help(colon)
head(colon)

# Separate RFS data from OS data
colon.RFS=colon[colon$etype==1,] # recurrence-free survival
colon.OS=colon[colon$etype==2,]  # overall survival

# Define RFS and OS in their respective data sets
colon.RFS$RFS=Surv(colon.RFS$time,colon.RFS$status)
colon.OS$OS=Surv(colon.OS$time,colon.OS$status)

# Fit the model for RFS, use x=T to keep the information required for Shiny app generation
RFS.fit=coxph(RFS~as.factor(rx)+as.factor(sex)+age+as.factor(obstruct)+nodes,
              data=colon.RFS,x=T)


# source the shine.coxph function defition
source('D:/SPounds/Teaching/shinyCoxApp/shiny-Cox-2019-12-02.R')


# Specify the location of the directory in which to create the app on your machine.
app.dir="D:/SPounds/Teaching/shinyCoxApp/"

# create and launch a shiny R app to visualize the coxph result in RFS.fit
test.res=shine.coxph(RFS.fit,app.dir,launch.app=T)

# 
OS.fit=coxph(OS~as.factor(rx)+as.factor(sex)+age+
                as.factor(obstruct)+as.factor(perfor)+as.factor(node4)+as.factor(surg),
             data=colon.OS,x=T)

test.res=shine.coxph(OS.fit,app.dir,launch.app=T)
