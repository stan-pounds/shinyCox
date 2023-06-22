##### SUBODH EDITED VERSION
### 2022-04-11: changed shine.coxph ui.code and server.code; added cox predicted times table


shine.coxph=function(...)
  
{
  ########################
  # determine the class of each input argument
  input.list=list(...)
  n.list=length(input.list)
  list.class=rep("",n.list)
  for (i in 1:n.list)
    list.class[i]=class(input.list[[i]])
  
  #######################
  # extract the coxph models from the input
  cox.list=which(list.class=="coxph")
  n.model=length(cox.list)
  model.list=vector("list",n.model)
  for (i in 1:n.model)
    model.list[[i]]=input.list[[cox.list[i]]]
  names(model.list)=names(input.list)[cox.list]
  
  #######################
  # get colors list
  # RColorBrewer brewer.pal
  clrs=input.list$clrs
  if (is.null(clrs))
    clrs=hcl.colors(n.model, "Dark 2", alpha = 1)
  
  ##########################
  # get app directory
  app.dir=input.list$app.dir
  if (is.null(app.dir))
  {
    app.dir=getwd()
  }
  date.time=as.character(Sys.time())
  date.time=gsub(":","-",date.time,fixed=T)
  date.time=gsub(" ","-",date.time,fixed=T)
  app.dir=paste0(app.dir,"/",date.time,"/")
  dir.create(app.dir)
  message(paste0("Shiny app will be written in directory ",app.dir,"."))
  
  #######################
  # convert the coxph models to coxfit objects
  coxfit.list=vector("list",n.model)
  for (i in 1:n.model)
    coxfit.list[[i]]=prep.coxfit(model.list[[i]])
  names(coxfit.list)=names(model.list)
  
  #######################
  # Save app data into app directory
  cox.fit.list=coxfit.list
  save(cox.fit.list,clrs,file=paste0(app.dir,"appData.Rdata"))
  
  #######################
  # get different code sections
  input.data.code=write.coxfit.input.data.code(coxfit.list)
  KM.plot.code=write.KM.plot.code(coxfit.list,clrs)
  table.code=prop.haz.tables(cox.fit.list)
  
  ########################
  # generate header code
  hdr.code=c("#rm(list=ls())",
             "options(stringsAsFactors=F)",
             paste0("load('",app.dir,"appData.Rdata')"))
  
  
  ########################
  # generate ui code
  ui.code=c("ui=navbarPage(",
            "             'Cox Model Survival Predictions',",
            "    tabPanel('Plot',",
            "             sidebarLayout(",
            "             sidebarPanel(",
            paste0("                          ", # SUBODH CHANGED FLOW
                   input.data.code$ui.code,
                   ","),
            "textInput('predProbTimes','Times for predicted probabilities',placeholder='Enter values separated by a comma'),", # SUBODH ADDITION
            "actionButton(inputId = 'go', label = 'Generate Plot'),",
            "actionButton(inputId = 'reset', label = 'Reset'),", # SUBODH ADDITION
            "actionButton(inputId = 'app.exit', label = 'Exit App'),",
            "selectInput('clrs', label = 'Choose Colors', choices = hcl.pals())", # color choice
            "),",
            "mainPanel(",
            "h3('Predicted Survival Curve'),",
            "plotOutput(outputId = 'KM'),",
            "h3('Predicted Probability at Fixed Times'),", # SUBODH ADDITION
            "textOutput(outputId='noPredTimes'),", # SUBODH ADDITION
            "tableOutput(outputId = 'cox.times')))),",# SUBODH ADDITION
            "tabPanel('Summary Tables',",
            "mainPanel(",
            # "h3('Hazard Ratio Summary Table'),", # SUBODH ADDITION
            # "tableOutput(outputId = 'HR'),", # SUBODH ADDITION
            # "h3('Assessing the Proportional Hazards Assumption'),", # SUBODH ADDITION
            # "tableOutput(outputId = 'PHA')", # SUBODH ADDITION
            table.code$ui.code, ### NEW
            ")))") # SUBODH CHANGED THE FLOW
  
  #########################
  # generate server code
  server.code=c("server=function(input,output)",
                "{",
                "          observeEvent(input$app.exit, {stopApp()}) # Exit when exit button is pressed",
                "          observeEvent(input$go, {",
                
                input.data.code$server.code,
                KM.plot.code$server.code,
                table.code$server.code, ### NEW
                "predProbTable <- cox.times.table(KM.hat,input$predProbTimes)", # SUBODH ADDITION
                "if (is.null(predProbTable)) output$noPredTimes <- renderText('No input times detected. If you provided times, check that you separated numbers with a single comma and you provided valid numbers.') else output$noPredTimes <- renderText(invisible())", # SUBODH ADDITION
                "output$cox.times=renderTable(predProbTable,rownames=TRUE)", # SUBODH ADDITION
                "output$HR=renderTable(cox.fit.list[[1]]$HR.table,rownames=TRUE)", # SUBODH ADDITION
                "output$PHA=renderTable(cox.fit.list[[1]]$PHA.table$table,rownames=TRUE)", # SUBODH ADDITION
                "colors=hcl.colors(length(cox.fit.list), input$clrs, alpha = 1)", # colors
                "})",
                "          observeEvent(input$reset, {output$KM <- output$HR <- output$PHA <- output$cox.times <- NULL}) # Reset main area", # SUBODH ADDITION
                "}") # SUBODH CHANGED THE FLOW
  
  ##########################
  # app code
  app.code=c("cox.app=shinyApp(ui,server)",
             "runApp(cox.app)")
  
  ###########################
  # write the app file
  app.code.file=paste0(app.dir,"shinyCoxapp.R")
  write(unlist(hdr.code),file=app.code.file,sep="\n")
  write(unlist(ui.code),file=app.code.file,sep="\n",append=T)
  write(unlist(server.code),file=app.code.file,sep="\n",append=T)
  write(unlist(app.code),file=app.code.file,sep="\n",append=T)
  
  
  
  ##########################
  # return result
  
  res=list(app.dir=app.dir,
           app.data.file=paste0(app.dir,"/appData.Rdata"),
           app.code.file=app.code.file,
           coxfit.list=coxfit.list,
           hdr.code=hdr.code,
           ui.code=ui.code,
           server.code=server.code)
  
  return(res)
  
}

################################
# write the shiny code to generate plots

write.KM.plot.code=function(cox.fit.list,clrs)
{
  n.models=length(cox.fit.list)
  ui.code=server.code=NULL
  
  ############
  # server code to initialize KM.hat object
  
  initialize.KM.hat=c("n.models=length(cox.fit.list)",
                      "KM.hat=vector('list',n.models)",
                      "lp=rep(NA,n.models)",
                      "names(KM.hat)=names(cox.fit.list)") # here
  server.code=c(server.code,
                initialize.KM.hat)
  
  ##########
  # server code to compute KM.hat object
  
  compute.KM.hat=c("for (i in 1:n.models)",
                   "{",
                   "   km.hat=predict.one.coxfit(cox.fit.list[[i]],new.data)",
                   "   lp[i]=attr(km.hat,'lp')",
                   "   sfit=list(time=km.hat$time,surv=km.hat$surv)",
                   "   class(sfit)='survfit'",
                   "   KM.hat[[i]]=sfit",
                   "}")
  server.code=c(server.code,
                compute.KM.hat)
  
  #########
  # server and ui code to display KM plots
  
  display.KM.server=c("output$KM=renderPlot({cox.KM.plots(KM.hat,clrs=colors)})")
  display.KM.ui=c("plotOutput(outputId = 'KM')")
  
  ui.code=c(ui.code,
            display.KM.ui)
  server.code=c(server.code,
                display.KM.server)
  
  res=list(ui.code=ui.code,
           server.code=server.code)
  
  return(res)
}


#############################
# Generate Cox predicted KM plots

cox.KM.plots=function(KM.hat,clrs=NULL)
  
{
  n.models=length(KM.hat)
  if (is.null(clrs))
    clrs=rainbow(n.models)
  
  if (is.null(names(KM.hat)))
    names(KM.hat)=paste0("model ",1:n.models)
  
  max.time=0
  for (i in 1:n.models)
    max.time=max(max.time,
                 max(KM.hat[[i]]$time,na.rm=T))
  
  plot(c(0,1.2*max.time),
       c(0,1),xlab="Time",las=1,
       ylab="Prob",type="n")
  
  for (i in 1:n.models)
    lines(KM.hat[[i]],col=clrs[i])
  
   legend(1.05*max.time,1,
          col=clrs,lwd=1,
          legend=names(KM.hat),
          cex=1)
}

#############################
# Generate Cox predicted times table: SUBODH NEW ADDITION

predSurvTime <- function(kmIn,timeIn) { # expects a data frame with columns of time and surv 
  kmIn$surv[max(which(kmIn$time <= timeIn))]
}

cox.times.table=function(KM.hat,fixTimes=NULL)
  
{
  n.models=length(KM.hat)
  
  if (is.null(names(KM.hat)))
    names(KM.hat)=paste0("model ",1:n.models)
  
  if (is.null(fixTimes) | fixTimes=="") {
    return(NULL)
  } else {
    predTimes <- as.numeric(unlist(strsplit(fixTimes,split=","))) # expects an input character string of numbers each separated by a comma
    if (any(is.na(predTimes))) return(NULL)
  }
  
  tabOut <- matrix(nrow=n.models,ncol=length(predTimes))
  rownames(tabOut) <- names(KM.hat)
  colnames(tabOut) <- paste("Time:",predTimes)
  
  for (i in 1:n.models){
    for (j in 1:length(predTimes)){
      tabOut[i,j] <- predSurvTime(KM.hat[[i]],predTimes[j])
    }
  }
  return(tabOut)
}


#################################
# write the shiny code to obtain user inputs

write.coxfit.input.data.code=function(cox.fit.list)
  
{
  
  if(is.null(names(cox.fit.list)))
    names(cox.fit.list)=paste0("model ",1:length(cox.fit.list))
  
  ###############
  # Get the set of input variables across all models
  vnames=get.vnames.cox.fits(cox.fit.list)
  
  ############
  # Get the range of numeric predictor variables
  num.x.rng.mtx=get.xrng.cox.fits(cox.fit.list,
                                  vnames)
  
  ###########
  # Get the levels of categorical predictor variables
  cat.lvls=get.levels.cox.fits(cox.fit.list,
                               vnames)
  # NEW
  #######################################################################
  # Get levels of logic predictors
  logic.lvls <- get.logic.cox.fits(cox.fit.list, vnames)
  ########################################################################
  #############
  # Generate shiny code for each variable
  
  ui.code=server.code=NULL # initialize shiny code for ui and server
  
  if(!is.null(cat.lvls))
  {
    for (i in 1:length(cat.lvls))
    {
      cat.pick=ez.pickone(names(cat.lvls)[i],
                          tools::toTitleCase(names(cat.lvls)[i]),
                          cat.lvls[i])
      ui.code=c(ui.code,cat.pick$ui.code)
      server.code=c(server.code,
                    cat.pick$server.code)
    }
  }
  #NEW
  ######################################################################
  if(!is.null(logic.lvls))
  {
    for (i in 1:length(logic.lvls))
    {
      logic.pick=ez.pickone.logic(names(logic.lvls)[i],
                          tools::toTitleCase(names(logic.lvls)[i]),
                          logic.lvls[i])
      ui.code=c(ui.code,logic.pick$ui.code)
      server.code=c(server.code,
                    logic.pick$server.code)
    }
  }
  ##########################################################################
  
  if(!is.null(num.x.rng.mtx))
  {
    for (i in 1:ncol(num.x.rng.mtx))
    {
      x.slider=ez.slider(colnames(num.x.rng.mtx)[i],
                         colnames(num.x.rng.mtx)[i],
                         num.x.rng.mtx[1,i],
                         num.x.rng.mtx[2,i],
                         mean(num.x.rng.mtx[,i])) # Harrison fixed index
      ui.code=c(ui.code,x.slider$ui.code)
      server.code=c(server.code,
                    x.slider$server.code)
    }
  }
  
  new.data.code=paste0("new.data = cbind.data.frame(",
                       paste0(server.code,collapse=","),")")
  
  code.res=list(ui.code=ui.code,
                server.code=new.data.code)
  
  return(code.res)
  
}

#####################################
# get the set of unique predictor 
# variable names
# from a list of cox.fit objects

get.vnames.cox.fits=function(cox.fit.list)
{
  n.models=length(cox.fit.list)
  var.name=NULL
  var.type=NULL
  for (i in 1:n.models) # loop over models
  {
    var.name=c(var.name,
               names(cox.fit.list[[i]]$types))
    var.type=c(var.type,
               cox.fit.list[[i]]$types)
  }
  dup.name=duplicated(var.name)
  var.name=var.name[!dup.name]
  var.type=var.type[!dup.name]
  # Find better place for this, covers issue with interactions
  #############################
  var.name=na.omit(var.name)
  var.type=na.omit(var.type)
  ###############################
  res=cbind.data.frame(var.name=var.name,
                       var.type=var.type)
  
  return(res)
}

###############################
# Get the levels for the categorical variables

get.levels.cox.fits=function(cox.fit.list,vnames)
{
  
  cat.vars=which(vnames[,"var.type"]!="numeric" & vnames[,"var.type"]!="logical") 
  if (length(cat.vars)==0)
    return(NULL)
  
  n.vars=length(cat.vars)
  n.models=length(cox.fit.list)
  cat.lvls=vector("list",n.vars)
  cat.names=vnames[cat.vars,"var.name"]
  names(cat.lvls)=cat.names
  
  for (i in 1:n.models)
  {
    mod.lvls=cox.fit.list[[i]]$xlevels
    mod.vars=names(mod.lvls)
    for (j in mod.vars)
    {
      cat.lvls[[j]]=c(cat.lvls[[j]],
                      mod.lvls[[j]])
    }
  }
  
  for (j in 1:length(cat.lvls))
    cat.lvls[[j]]=unique(cat.lvls[[j]])
  
  cat.names=gsub("strata(","",cat.names,fixed=T)
  cat.names=gsub(")","",cat.names,fixed=T)
  names(cat.lvls)=cat.names
  
  return(cat.lvls)
  
}

####################################
# Get the range of the numeric variables
# across a list of cox.fit objects
# Error, rng.mtx[1,x.name]=min(x.rng[1,j],rng.mtx[1,x.name],na.rm=T)
#        rng.mtx[2,x.name]=min(x.rng[2,j],rng.mtx[2,x.name],na.rm=T) fixed
get.xrng.cox.fits=function(cox.fit.list,vnames)
  
{
  num.vars=which(vnames[,"var.type"]=="numeric")
  if (length(num.vars)==0)
    return(NULL)

  n.models=length(cox.fit.list)
  rng.mtx=matrix(NA,2,length(num.vars))
  rng.mtx[1,]=NA
  rng.mtx[2,]=NA
  colnames(rng.mtx)=vnames[num.vars,"var.name"]
  
  
  for (i in 1:n.models)
  {
    x.rng=cox.fit.list[[i]]$num.x.rng
    for (j in 1:ncol(rng.mtx))
    {
      x.name=colnames(rng.mtx)[j]
      rng.mtx[1,x.name]=min(x.rng[1,x.name],rng.mtx[1,x.name],na.rm=T)
      rng.mtx[2,x.name]=max(x.rng[2,x.name],rng.mtx[2,x.name],na.rm=T) #changed from min()
    }
  }
  return(rng.mtx)
}


# add tab with plot hazard ratio table
# NEW function
########################################
get.logic.cox.fits <- function(cox.fit.list, vnames) {
  logic.vars=which(vnames[,"var.type"]=="logical")
  if (length(logic.vars)==0)
    return(NULL)

  n.vars <- length(logic.vars)
 
  logic.levels <- vector("list", n.vars)
  logic.names <- vnames[logic.vars, "var.name"]
  names(logic.levels) <- logic.names

  for (i in 1:n.vars) {
    logic.levels[[i]] <- c("TRUE", "FALSE")
  }

  return(logic.levels)
}

# NEW function, proportional hazard for each
##################################################################
prop.haz.tables <- function(cox.fit.list) {
  ui.code=c("tabsetPanel(")
  server.code=c()
  for (i in 1:length(cox.fit.list)) {
    
    server.code = c(server.code,paste0("output$HR", i, "=renderTable(cox.fit.list[[",i, "]]$HR.table,rownames=TRUE)"),
                    paste0("output$PHA", i, "=renderTable(cox.fit.list[[",i, "]]$PHA.table$table,rownames=TRUE)"),
                    paste0("output$title", i, "=renderText(paste(cox.fit.list[[",i,"]]$nsample, 'samples,', cox.fit.list[[",i, "]]$nevents, 'events'))"))
    
    if(i < length(cox.fit.list)){
    ui.code = c(ui.code,paste0("tabPanel('",names(cox.fit.list)[i],"',"),
                paste0("h5(textOutput(outputId = 'title", i, "')),"),
                "'Hazard Ratio Summary Table',",
                paste0("column(12, align = 'center', tableOutput(outputId = 'HR", i, "')),"),
                "h4('Assesing the Proportional Hazards Assumption'),",
                paste0("column(12, align = 'center', tableOutput(outputId = 'PHA", i, "')),"),
                "),") } else
                  ui.code = c(ui.code,paste0("tabPanel('",names(cox.fit.list)[i],"',"),
                              paste0("h5(textOutput(outputId = 'title", i, "')),"),
                              "'Hazard Ratio Summary Table',",
                              paste0("column(12, align = 'center', tableOutput(outputId = 'HR", i, "')),"),
                              "h4('Assesing the Proportional Hazards Assumption'),",
                              paste0("column(12, align = 'center', tableOutput(outputId = 'PHA", i, "')),"),
                              ")")
  }
  ui.code=c(ui.code, ")")
  code.res=list(ui.code=ui.code,
                server.code=server.code)
  return(code.res)
}
