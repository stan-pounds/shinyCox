


pack.list=installed.packages()
if (!any(rownames(pack.list)=="shiny")) install.packages("shiny")
library(shiny)

################################
# shine.coxph
# Version date: 2019-12-02
# Purpose: quickly generate a shiny R app to visualize the predictions of a specific Cox model for an individual patient
# Features to add:
#      For coxph objects that include a strata, plot curve only for selected stratum
#      Add notations to explain colors
#      

shine.coxph=function(coxph.result,   # result of coxph with x = T
                     app.dir=NULL,   # name of the directory in which to create the app
                     launch.app=F,   # indicates whether to launch the app when done, default = F
                     xlab="Time",    # Options for Graph Labels
                     ylab="Prob",
                     main="",
                     cex.lab=1.5,
                     cex.axis=1.5,
                     las=1)
{
  ##############################
  # stop for invalid input
  if (!any(class(coxph.result)=="coxph"))
    stop("The argument coxph.object must be the result object of the coxph function.")
  
  if (!any(names(coxph.result)=="x"))
    stop("The coxph.object must include a subcomponent named x.   Re-run coxph with x=TRUE and try again.")
  
  ################################
  # initialize the code lines of the shiny app
  global.lines=c("library(shiny)","library(survival)")
  server.lines=NULL
  ui.lines=NULL
  newdata.lines=NULL
  
  ###################################################
  # get the terms of the Cox model and their data types
  cox.terms=attr(coxph.result$terms,
                 "dataClasses")
  cox.terms=cox.terms[-1]           # drop the survival response variable
  
  ########################################
  # Stop if there are no predictor terms
  if (length(cox.terms)==0)         # no predictor terms
  {
    stop("The coxph.result object has no predictor terms - no app generated.")
  }
  
  ##########################################
  # Create a pick list for each categorical predictor term
  if (!is.null(coxph.result$xlevels))     # only do this if necessary
  {
    nvar=length(coxph.result$xlevels)     # number of categorical predictor terms
    x.names=names(coxph.result$xlevels)   # names of categorical predictor terms
    for (i in 1:nvar)                     # begin loop over categorical predictor terms
    {
      x.name=get.term.name(x.names[i])                # extract the variable name
      temp=ez.pickone(x.name,x.name,                  # generate ui & server code for picklist
                      coxph.result$xlevels[[i]])
      server.lines=c(server.lines,                    # add new line to server code
                     temp$server.code)
      ui.lines=c(ui.lines,                            # add new line to ui code
                 temp$ui.code)
      term.index=which(names(cox.terms)==x.names[i])  # identify this term in the predictor term list
      newdata.lines=c(newdata.lines,                  # add a line to the new data frame
                      paste0(x.name," = ",
                             x.name))
      cox.terms=cox.terms[-term.index]                # remove this term from the set of terms to be added to the shiny app
    }                                    # end loop over categorical predictor terms                                
  } # end section to create pick list for each categorical predictor term
  
  ##############################################
  # Create a slider input for each numeric predictor term
  if (any(cox.terms=="numeric"))
  {
    num.terms=which(cox.terms=="numeric")    # identify the numeric predictor terms
    for (i in num.terms)                     # loop over numeric predictor terms
    {
      x.name=names(cox.terms)[i]                 # get the name of this numeric predictor term
      x=coxph.result$x[,x.name]                  # get the data for this numeric predictor term
      x.min=min(x,na.rm=T)                       # compute the minimum for this numeric predictor term     
      x.max=max(x,na.rm=T)                       # compute the maximum for this numeric predictor term
      x.rng=(x.max-x.min)                        # compute the range for this numeric predictor term
      x.val=median(x,na.rm=T)                    # compute the median to use as a default value in the app
      temp=ez.slider(names(cox.terms)[i],        # generate the server and ui code for this numeric predictor term
                     names(cox.terms)[i],
                     min=x.min-0.10*x.rng,
                     max=x.max+0.10*x.rng,
                     value=x.val)
      server.lines=c(server.lines,               # update the overall server code
                     temp$server.code) 
      ui.lines=c(ui.lines,                       # update the overall ui code
                 temp$ui.code)
      newdata.lines=c(newdata.lines,             # update the new data code
                      paste0(x.name," = ",
                             x.name))
    }                                         # end loop over numeric predictor variables
    cox.terms=cox.terms[-num.terms]           # remove the numeric predictors from the cox. terms
  } # end section for numeric predictor variables
  
  ################################################
  # Generate warnings for other types of predictor terms
  if (length(cox.terms)>0)
  {
    warning(paste0("Unable to automatically include predictor term ",
                   names(cox.terms)," in the app."))
  }
  
  ############################################
  # Create code to choose number of neighbors for goodness-of-fit visualization
  opts.n=(0:5)*10
  temp=ez.pickone("n.gof","Number of Prediction Neighbors",(0:5)*10)
  server.lines=c(server.lines,
                 temp$server.code)
  ui.lines=c(ui.lines,
             temp$ui.code)
  
  #############################################
  # create the server code lines to define the new.data data.frame
  newdata.lines=paste0(newdata.lines,collapse=", ")
  newdata.lines=paste0("new.data = cbind.data.frame(",
                       newdata.lines,")")
  
  ##########################################
  # Create the server code lines to compute the new KM fit
  server.lines=c(server.lines,newdata.lines)
  newfit.lines=c("new.fit = survfit(coxph.result,newdata=new.data,conf.type='log-log')",
                 "subj.pred=predict(coxph.result,newdata=new.data)",
                 "pred.edf=approx(sort.pred,1:n.sort/(1+n.sort),xout=subj.pred,rule=2)$y",
                 "pred.diff=cox.pred-subj.pred",
                 "pred.ord=order(abs(pred.diff))",
                 "n.gof=min(as.numeric(input$n.gof),length(pred.diff))",
                 "gof.vals=cox.pred[pred.ord[1:n.gof]]",
                 "gof.edf=approx(sort.pred,1:n.sort/(1+n.sort),xout=gof.vals,rule=2)$y",
                 "gof.surv=coxph.result$y[pred.ord[1:n.gof]]",
                 "gof.KM=survfit(gof.surv~1)")
  server.lines=c(server.lines,newfit.lines)
  
  ####################################
  # add code to show the predicted KM curve
  KM.lines=c(paste0("output$KM=renderPlot({plot(new.fit, conf.int=T,",
                                            "xlab='",xlab,
                                            "', ylab='",ylab,
                                            "', cex.lab=",cex.lab,
                                            ", main='",main, 
                                            "', las=",las,",col='red')"),
                     "if (n.gof>0) lines(gof.KM,col='blue',conf.int=F)})")
  server.lines=c(server.lines,KM.lines)
  
  ###################################
  # add server code to show the table of hazard ratio estimates
  tbl.line=paste0("output$cox.tbl=renderTable(cox.tbl)")
  server.lines=c(server.lines,tbl.line)
  
  tbl.line="output$zph.tbl=renderTable(zph.tbl)"
  server.lines=c(server.lines,tbl.line)
  
  ########################################
  # add server code to show the plot of predicted cox scores
  pred.lines=c("output$pred.plot=renderPlot({plot(range(coxph.result$y[,1],na.rm=T),exp(range(cox.pred)),",
               "                                  xlab='Time',ylab='Overall Hazard-Ratio',type='n',",
               "                                  cex.lab=1.5,cex.axis=1.5,las=1)",
               "                             segments(0,exp(subj.pred),max(coxph.result$y[,1],na.rm=T),col='red',lwd=2,lty=2)",
               "                             pt.clr=rep('black',length(cox.pred))",
               "                             pt.pch=c(3,19)[1+coxph.result$y[,2]]",
               "                             if (n.gof>0) pt.clr[pred.ord[1:n.gof]]='blue'",
               "                             points(coxph.result$y[,1],exp(cox.pred),pch=pt.pch,col=pt.clr)",
               "                             #if (n.gof>0) points(coxph.result$,gof.edf,col='blue',cex=1.5,pch=19)",
               "                             #points(exp(subj.pred),pred.edf,col='red',cex=1.5,pch=19)",

               "                            })")
  server.lines=c(server.lines,pred.lines)
  
  ##############################
  # add code for the go button
  go.button=ez.button('go',"Generate Plot",server.lines)
  server.lines=go.button$server.code
  ui.lines=c(ui.lines,go.button$ui.code)
  
  ##################################
  # add code for the exit button
  exit.button=ez.exit()
  server.lines=c(exit.button$server.code,
                 server.lines)
  
  ui.lines=c(ui.lines,exit.button$ui.code)
  ui.lines=c('h3("Enter Data Values for Subject")',
             ui.lines)
  
  ################################################
  # add code for plots and tables in main panel
  ui.main=c("h3('Predicted Survival Curve')",
            "plotOutput(outputId = 'KM')",
            "h3('Overall Hazard Ratio vs. Time')",
            "plotOutput(outputId = 'pred.plot')",
            "h3('Hazard Ratio Estimates')",
            "tableOutput(outputId = 'cox.tbl')")  
            
  sb.code=ez.sidebarLayout(ui.lines,ui.main)
  ui.lines=sb.code
  
  #########################################
  # add code for start and end of ui code
  ui.lines=c("ui=fluidPage(",
             "h1('Cox Model Survival Predictions'),",
             paste0("         ",ui.lines),
             "   )")
  
  ##########################################
  # add code for start and end of server code
  server.lines=c("server=function(input,output)",
                 "       {",
                 paste0("         ",server.lines),
                 "       }")
  
  #########################################
  # create the application directory
  if (is.null(app.dir))  # if not specified, use present working directory
    app.dir=getwd()
  
  # add a time stamp
  time.stamp=gsub(" ","-",Sys.time(),fixed=T)
  time.stamp=gsub(":","-",time.stamp,fixed=T)
  
  # create the directory path string
  out.dir=paste0(app.dir,"/shinyCoxApp-",time.stamp)
  dir.create(out.dir)
  message(paste0("The shiny app will be built in the directory ",
                 out.dir,"."))
  
  ############################################
  # Remove x (predictor) data to protect patient privacy
  # and save coxph.result
  x.index=which(names(coxph.result)=="x")
  coxph.result2=coxph.result[-x.index]
  class(coxph.result2)="coxph"
  coxph.result=coxph.result2
  coxph.resfile=paste0(out.dir,
                       "/coxph.result.Rdata")
  
  save(coxph.result,file=coxph.resfile)
  
  #######################################
  # Add the filepath of the coxph.result Rdata file to the app code
  global.lines=c(global.lines,
                 paste0("load('",coxph.resfile,"')"))
  global.lines=c(global.lines,
                 "cox.smry=summary(coxph.result)",
                 "cox.CIs=confint(coxph.result)",
                 paste0("cox.tbl=cbind.data.frame(Variable.Name=rownames(cox.CIs),",
                                                  "Hazard.Ratio=cox.smry$coefficients[,'exp(coef)'],",
                                                  "Lower.Bound=exp(cox.CIs[,1]),",
                                                  "Upper.Bound=exp(cox.CIs[,2]),",
                                                  "P.value=cox.smry$coefficients[,'Pr(>|z|)'])"),
                 "cox.pred=predict(coxph.result)",
                 "sort.pred=sort(cox.pred[!is.na(cox.pred)])",
                 "n.sort=length(sort.pred)",
                 "zph.res=cox.zph(coxph.result)",
                 "zph.tbl=zph.res$table",
                 "cox.tbl=cbind.data.frame(cox.tbl,p.PH.assumption=zph.tbl[cox.tbl$Variable.Name,3])",
                 "last.row.cox=c(Variable.Name='Overall Model',Hazard.Ratio=NA,Lower.Bound=NA,",
                 "               Upper.Bound=NA,P.value=cox.smry$sctest['pvalue'],",
                 "               p.PH.assumption=zph.tbl[nrow(zph.tbl),3])",
                 "cox.tbl=rbind.data.frame(cox.tbl,last.row.cox,stringsAsFactors=F)")
  
  ###############################
  # now write the shinyApp code.file
  app.file=paste0(out.dir,"/shinyCoxApp.R")
  write(global.lines,file=app.file)
  write("\n \n",file=app.file,append=T)
  write(ui.lines,file=app.file,append=T)
  write(server.lines,file=app.file,append=T)
  write("cox.app=shinyApp(ui,server)",file=app.file,append=T)
  write("runApp(cox.app)",file=app.file,append=T)
  
  #######################################
  # return everything as a list object
  res=list(global.lines=global.lines,
           ui.lines=ui.lines,
           server.lines=server.lines,
           coxph.result=coxph.result,
           app.dir=out.dir)
  
  ########################################
  # if requested, launch the app
  if (launch.app) source(app.file)
  
  return(res)
} # end shine.coxph

####################################
# Create a sidebar panel layout
ez.sidebarLayout=function(ui.sidebar,  # ui code for the sidebar
                          ui.main)     # ui code for the main panel
{
  # Add commas after all but the last line of ui.sidebar
  n.sidebar=length(ui.sidebar)
  ui.sidebar[-n.sidebar]=paste0(ui.sidebar[-n.sidebar],",")
  
  # Add some indention space to enhance readability
  ui.sidebar=paste0("    ",ui.sidebar)
  
  # similar operations for main panel ui code
  n.main=length(ui.main)
  ui.main[-n.main]=paste0(ui.main[-n.main],",")
  ui.main=paste0("    ",ui.main)
  
  # write the code before and after
  res=c("sidebarLayout(",
        "  sidebarPanel(",ui.sidebar,"    ), ",
        "  mainPanel(",ui.main,"    ))")
  return(res)
}


###################################
# Generate an exit app button
ez.exit=function(id="app.exit",
                 label="Exit App")
{
  exit.code="stopApp()"
  res=ez.button(id,label,exit.code)
  res$server.code=paste0(res$server.code,collapse="")
  res$server.code=paste0(res$server.code,"# Exit when exit button is pressed")
  return(res)
}

#############################################
# Generate ui and server code for an action button; embeds server.lines into the action of the button
ez.button=function(id,              # identifier of the button
                   label,           # label to show on the button
                   server.lines="") # lines to be executed in server when button is pushed 
  
{
  ui.code=paste0("actionButton(inputId = '",id,
                 "', label = '",label,"')")
  server.code=c(paste0('observeEvent(input$',id,', {'),
                server.lines,'})')
  server.code=gsub('{"','{',server.code,fixed=T)
  res=list(ui.code=ui.code,
           server.code=server.code)
  return(res)
}

################################################
# Generate ui and server code for a slider (numeric data input)
ez.slider=function(vname,  # variable name
                   label,  # variable label in the GUI
                   min,    # minimum possible value of the variable
                   max,    # maximum possible value of the variable
                   value)  # default value of the variable
  
{
  ui.code=paste0("sliderInput(inputId = '",vname,"'",
                 ", label = '",label,"'",
                 ", min = ",min,
                 ", max = ",max,
                 ", value = ",value,")")
  server.code=paste0(vname," = input$",vname)
  return(list(ui.code=ui.code,
              server.code=server.code))
}

#########################################
# Generate ui and server code for to pick one choice from a dropdown
ez.pickone=function(vname,    # Variable name
                    label,    # Variable label in the GUI
                    choices)  # possible values of the variable
  
{
  ui.code=paste0("selectInput(inputId = '",vname,
                 "', label = '",label,"'",
                 ", choices = c(", 
                 paste0(
                   paste0("'",choices,"'"),collapse=", "),"))")
  server.code=paste0(vname," = input$",vname)
  return(list(ui.code=ui.code,
              server.code=server.code))
}

#################################################
# Extract the name of a term from the model structure
get.term.name=function(name.string)
{
  name.split=unlist(strsplit(name.string,split="(",fixed=T))
  name.split=unlist(strsplit(name.split,split=")",fixed=T))
  return(name.split[length(name.split)])
}
