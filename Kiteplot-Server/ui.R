
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage( 
    
     titlePanel("Kiteplot Generator"),
     sidebarLayout(
          sidebarPanel(
               fileInput('file1', 'Choose excel File'),
               textInput("title","Choose Title for Plot"),
               textInput("interval", "Height Interval of Measurements"),
               radioButtons("method","Method of Research",c("Proportions"="prop","Individuals"="individ", "Biomass" = "biomass")),
               # Set surface of measurement if research method is Biomass
               conditionalPanel(
                    condition = "input.method == 'biomass'",
                    textInput("unit", HTML(paste("Size of Surface in m",tags$sup(2), sep="")))
               ),
               downloadButton('downloadPlot', label = "Download Plot as PDF")
          ),
          
          mainPanel(
               plotOutput('algaeplot')
          )
     )
))
