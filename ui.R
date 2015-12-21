library(shiny)

    shinyUI(fluidPage( 
    
     titlePanel("Kiteplot Generator"),
     sidebarLayout(
          sidebarPanel(
                fileInput('file1', 'Choose xlsx file'),
                textInput("sheet",label="Sheet nr."),
                textInput("title","Plot title"),
                textInput("ylab","y-label"),
                radioButtons("TypeOfYAxis","Type of y-axis",c("Length from highest station"="LengthFromHighest","Height above sea level"="Height")),
                conditionalPanel(
                    condition = "input.TypeOfYAxis == 'Height'",
                    textInput("above_sea", "Initial height above lowest astronomical tide")
                 ),
                textInput("interval", "Interval of measurements"),
                radioButtons("method","Method of research",c("Proportions"="prop","Individuals"="individ", "Biomass" = "biomass")),
               # Set surface of measurement if research method is Biomass
               conditionalPanel(
                    condition = "input.method == 'individ'",
                    textInput("legendScale", "Number of individuals in legend")
               ),
                conditionalPanel(
                    condition = "input.method == 'biomass'",
                    textInput("unit", HTML(paste("Size of surface in m", tags$sup(2), sep="")))
                ),
                
                actionButton('go',label="Plot Kiteplot"),
                br(),
                br(),
                br(),
                downloadButton('downloadPlot', label = "Download Plot as PDF")
        ),
          
          mainPanel(
               #plotOutput('algaeplot'),
               textOutput('debug'),
               plotOutput('algaeplot',hover = hoverOpts(id = "plot_hover")),
               verbatimTextOutput("hover_info")
          )
     )
    ))
