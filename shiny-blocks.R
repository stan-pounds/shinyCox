########## SUBODH CHANGED ez.pickone

#####################################################################
# Generate ui and server code for to pick one choice from a dropdown

ez.pickone=function(vname,    # Variable name
                    label,    # Variable label in the GUI
                    choices)  # possible values of the variable
  
  
{
  ui.code=paste0("selectInput(inputId = '",vname,
                 "', label = '",label,"'",
                 ", choices = ", 
                 paste0(choices,
                        collapse=", "),
                 ")") # SUBODH CHANGED FLOW AND REMOVED THE BELOW:
  
                 # ", choices = c(",  # this adds an extra c() 
                 # paste0(choices,
                          # paste0("'",choices,"'"), # this causes quotes around everything
                 #   collapse=", "),"))") # SUBODH CHANGED FLOW
  server.code=paste0(vname," = input$",vname)
  return(list(ui.code=ui.code,
              server.code=server.code))
}


#####################################################################
# Generate ui and server code for a slider bar (numeric data input)

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


###################################################################################
# Generate ui and server code for an action button; 
# embeds server.lines into the action of the button
# NOT USED IN CODE
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


###################################
# Generate an exit app button
# NOT USED IN CODE
ez.exit=function(id="app.exit",
                 label="Exit App")
{
  exit.code="stopApp()"
  res=ez.button(id,label,exit.code)
  res$server.code=paste0(res$server.code,collapse="")
  res$server.code=paste0(res$server.code,"# Exit when exit button is pressed")
  return(res)
}

####################################
# Create a sidebar panel layout
# NOT USED IN CODE
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


ez.pickone.logic=function(vname,    # Variable name
                    label,    # Variable label in the GUI
                    choices)  # possible values of the variable
  
  
{
  ui.code=paste0("radioButtons(inputId = '",vname,
                 "', label = '",label,"'",
                 ", choices = ", 
                 paste0(choices,
                        collapse=", "),
                 ")") # SUBODH CHANGED FLOW AND REMOVED THE BELOW:
  
  # ", choices = c(",  # this adds an extra c() 
  # paste0(choices,
  # paste0("'",choices,"'"), # this causes quotes around everything
  #   collapse=", "),"))") # SUBODH CHANGED FLOW
  server.code=paste0(vname," = as.logical(input$",vname, ")")
  return(list(ui.code=ui.code,
              server.code=server.code))
}



ui.code = paste0("tabPanel")



