library(shiny)
library(xlsx)
library(SirKR)
source("Kiteplot.R")
source("Kiteplot1.R")
library(shinyBS)

#Setting up the Shiny Server
shinyServer(function(input, output, session) {
     algaeplot<-eventReactive(input$go,{ 
          inFile <- input$file1
          if (is.null(inFile))  return(NULL)

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
          return(kitedata)
     })   
     
     output$algaeplot <- renderPlot({
          if (is.null(algaeplot()))
               return(NULL)
          algaeplot()
          
     })
#      output$debug <- renderTable({
#      })

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
     output$hover_info <- renderPrint({
          if(!is.null(input$file1)){
               if(!is.null(input$plot_hover)){
                    hover=input$plot_hover
                    data=algaeplot()
                    names(data) = gsub(x = names(data),
                                       pattern = "\\.",
                                       replacement = " ")
                    end=ncol(data)
                    maxdata=max(data[,2:end],na.rm=T)
                    comparedata=as.matrix(cbind(data[,2:end],data[,2:end]))
                    
                    interval = as.numeric(input$interval)
                    if(is.na(interval)) interval = 0.25
                    
                    above_sea = as.numeric(input$above_sea)
                    if(is.na(above_sea)) above_sea = 0
                    
                    height=(data[,1]-1) * interval + above_sea
                    ylim=floor(max(height)) + 1
                    if(input$TypeOfYAxis != "Height"){
                        initial=height[1]
                        if(is.na(initial)){
                            initial=0
                        }
                        height= height + ylim - 2 * (height-initial)
                    } 
                    
                    if(input$method == "prop"){
                         data=cbind(height,data[,2:end]/100)
                    } else {
                         data=cbind(height,data[,2:end]/maxdata)
                    }
                    
                    translation=seq(1,1+3*(end-2),by=3)
                    graph=t(t(data[,2:end])+translation)
                    graphmirror=t(translation-t(data[,2:end]))
                    graphdata=cbind(graph,graphmirror)
                    disthover=sqrt((hover$x-graphdata)^2+(hover$y-height)^2)
                    
                    legend = ""
                    legendScale=as.numeric(input$legendScale)
                    if(is.na(legendScale)){
                        legendScale=maxdata
                    }
                    
                    method = input$method
                    if(method == "prop"){
                        legend = "%"
                    }
                    if(method == "individ"){
                        legend = " individuals"
                    }
                    if(method == "biomass"){
                        unit = input$unit
                        if(unit == "1")  {
                            legend = paste(" g/","m","^2", sep="")
                        } else {
                            legend = paste(" g/",unit, "m","^2", sep="")
                        }
                    }
                    
                    
                    if(min(disthover,na.rm=T)<0.2){
                        matindex=which.min(disthover)
                        rowvalue = (matindex-1)%%nrow(graphdata) + 1
                        colvalue = (matindex-1)%/%nrow(graphdata) + 1
                        
                        info=cbind(colnames(graphdata)[colvalue],paste(height[rowvalue],"m",sep=" "),paste(comparedata[matindex],legend,sep=""))
                        colnames(info)=c("Species","Height","Coverage")
                        return(info)
                    }else{return("")}
               }    
          }
          
          
          
     })

})
