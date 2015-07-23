
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
               fileInput('file1', 'Choose xlsx file'),
               textInput("sheet",label="Sheet nr."),
               textInput("title","Plot title"),
               textInput("interval", "Height interval of measurements"),
               textInput("above_sea", "Height above lowest astronomical tide"),
               radioButtons("method","Method of research",c("Proportions"="prop","Individuals"="individ", "Biomass" = "biomass")),
               # Set surface of measurement if research method is Biomass
               conditionalPanel(
                    condition = "input.method == 'biomass'",
                    textInput("unit", HTML(paste("Size of surface in m", tags$sup(2), sep="")))
               ),
<<<<<<< HEAD
               
               actionButton('go',label="Plot Kiteplot"),
               br(),
               br(),
               br(),
=======
               textInput("above_sea", "Height Above Lowest Astronomical Tide"),
>>>>>>> 0bcaca7ebd932d467b68a416b7d43563ff3a4a38
               downloadButton('downloadPlot', label = "Download Plot as PDF")
          ),
          
          mainPanel(
               plotOutput('algaeplot')
               #plotOutput('algaeplot',hover = hoverOpts(id = "plot_hover"))
               #verbatimTextOutput("hover_info")
          )
     )
))
