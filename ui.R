
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
               conditionalPanel(
                    condition = "input.method == 'biomass'",
                    textInput("unit", "Size of Surface in m^2")
               ),
               downloadButton('downloadPlot', label = "Download Plot as PDF")
          ),
          
          mainPanel(
               plotOutput('algaeplot')
          )
     )
))
