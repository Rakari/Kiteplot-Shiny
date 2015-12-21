library(shiny)
library(xlsx)
library(SirKR)
source("Kiteplot.R")
source("Kiteplot1.R")

#Setting up the Shiny Server
shinyServer(function(input, output) {
     algaeplot<-eventReactive(input$go,{ 
          inFile <- input$file1
          
          if (is.null(inFile))
               return(NULL)
          sheetnr=as.numeric(input$sheet)
          if(is.na(sheetnr)){
              sheetnr=1
          }
          kitedata = read.xlsx(inFile$datapath, sheetIndex=sheetnr)
          if(ncol(kitedata)<=3){
              Kiteplot1(data=inFile$datapath,interval=input$interval,unit=input$unit,
                       above_sea = input$above_sea,sheetnr= input$sheet,title=input$title,
                       method=input$method,ylab=input$ylab,TypeOfYAxis=input$TypeOfYAxis,legendScale = input$legendScale)    
          }else{
          Kiteplot(data=inFile$datapath,interval=input$interval,unit=input$unit,
                   above_sea = input$above_sea,sheetnr= input$sheet,title=input$title,
                   method=input$method,ylab=input$ylab,TypeOfYAxis=input$TypeOfYAxis,legendScale = input$legendScale)
          }

     })   
     
     output$algaeplot <- renderPlot({
          if (is.null(algaeplot()))
               return(NULL)
          algaeplot()
          
     })

     
     output$downloadPlot <- downloadHandler(
          filename<-function(){
                paste(gsub(" ","-",input$title),"-",format(Sys.Date(), "%b-%d-%Y"), ".pdf", sep="")
          },
          content=function(file=NULL) {
               pdf(file,width=9,height=6)
               if (is.null(algaeplot())) return(NULL)
               Kiteplot(data=input$file1$datapath,interval=input$interval,unit=input$unit,above_sea = input$above_sea,sheetnr= input$sheet,
                        title=input$title,method=input$method,ylab=input$ylab,TypeOfYAxis=input$TypeOfYAxis,legendScale = input$legendScale)          
               dev.off()
          }
     )
#      output$hover_info <- renderPrint({
#           if(!is.null(input$file1)){
#                if(!is.null(input$plot_hover)){
#                     hover=input$plot_hover
#                     data=algaeplot()$realdata
#                     end=ncol(data)
#                     maxdata=max(data[,2:end],na.rm=T)
#                     comparedata=cbind(data[,2:end],data[,2:end])
#                     height=data[,1]*algaeplot()$interval
#                     
#                     if(input$method == "prop"){
#                          data=cbind(height,data[,2:end]/100)
#                     } else {
#                          data=cbind(height,data[,2:end]/maxdata)
#                          
#                     }
#                     translation=seq(1,1+3*(end-2),3)
#                     graph=t(t(data[,2:end])+translation)
#                     graphmirror=t(translation-t(data[,2:end]))
#                     graphdata=cbind(graph,graphmirror)
#                     disthover=sqrt((hover$x-graphdata)^2+(hover$y-height)^2)
#                     if(min(disthover,na.rm=T)<1){
#                         rowvalue=which.min(disthover)
#                         colvalue=which.min(disthover[rowvalue,])
#                         info=cbind(colnames(graphdata)[colvalue],paste(height[rowvalue]*0.25,"m",sep=" "),paste(comparedata[rowvalue,colvalue],"%",sep=""))
#                         colnames(info)=c("Species","Height","Coverage")
#                     }
#                     else{return(NULL)
#                          }
#                }
#           }
#           
#           
#           
#      })
#
})
