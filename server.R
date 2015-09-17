
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(xlsx)
library(plyr)
library(devtools)
library(SirKR)

shinyServer(function(input, output) {
     algaeplot<-eventReactive(input$go,{ 
          inFile <- input$file1
          
          if (is.null(inFile))
               return(NULL)
          
          interval = as.numeric(input$interval)
          if (is.na(interval)) {
               interval = 0.25
          }
          
          unit = as.numeric(input$unit)
          if (is.na(unit)) {
               unit = 1
          }
          
          above_sea = as.numeric(input$above_sea)
          if(is.na(above_sea)) {
               above_sea = 0
          }
          sheetnr=as.numeric(input$sheet)
          if(is.na(sheetnr)) {
               sheetnr= 1
          }
          #reads in xlsx or xls file chosen
          realdata = read.xlsx(inFile$datapath, sheetIndex=sheetnr)
          
          if(is.null(realdata)) return(NULL)
          
          end=ncol(realdata)
          
          #Extract duplicates of same elements in first column
          #height=seq(0,end*interval,interval)
          #height=unique(realdata[,1])  
          #realdata=ddply(realdata,names(realdata)[1],colwise(meanNA))
          data=clean(realdata)
          height=data[,1]
          maxdata=max(data[,2:end],na.rm=T)
          
          if(input$method == "prop"){
               data=cbind(height,data[,2:end]/100)
          } else {
               data=cbind(height,data[,2:end]/maxdata)
          }
          
          translation=seq(1,1+3*(end-2),by=3)
          graph=t(t(data[,2:end])+translation)
          graphmirror=t(translation-t(data[,2:end]))
          graphdata=cbind(graph,graphmirror)
          ylim=(interval/2)*max(height)+1+above_sea
          xlim=tail(translation,1)+2
          cex=0.7
          
          legend = ""
          #legend customized by input$method from user interface
          if(input$method == "prop"){
               legend = "100%"
          }
          if(input$method == "individ"){
               legend = paste(maxdata," individuals") 
          }
          if(input$method == "biomass"){
               legend = bquote(.(legend))
               if(unit == 1)  {
                    legend = bquote(.(paste(maxdata," g/","m", sep=""))^2)
               } else {
                    legend = bquote(.(paste(maxdata," g/",unit, "m", sep=""))^2)
               }
          }
               
          return(list("graphdata"=graphdata,
               "interval"=interval,
               "cex"=cex,
               "translation"=translation,
               "legend"=legend,
               "ylim"=ylim,
               "xlim"=xlim,
               "height"=height,
               "end"=end,
               "data"=data,
               "above_sea"=above_sea,
               "realdata"=realdata))
     })   
     
     output$algaeplot <- renderPlot({
          if (is.null(algaeplot()))
               return(NULL)
          
          plotlist=algaeplot()
          with(plotlist, matplot(graphdata,(interval/2)*height + above_sea,type="l",col=rep("black",ncol(graphdata)), ylim=c(floor(above_sea),floor(ylim)),xlim=c(0,xlim),lty=rep(1,ncol(graphdata)),xlab="",ylab="Height (m)",xaxt="n",bty="n",cex.axis=cex,cex.lab=cex))
          #at=seq(floor(plotlist$above_sea),round(2*plotlist$ylim)/2,0.5)
          #axis(side=2,at=at)
          with(plotlist,mtext(input$title, side=3, adj=0.4, line=1.2, cex=1, font=2))
          with(plotlist,mtext(names(data[2:end]),side=1,at=translation,cex=cex))
          legendline=with(plotlist, c(tail(translation,1)-2,tail(translation,1)))
          with(plotlist,lines(legendline,c(ylim,ylim)))
          with(plotlist,mtext(legend,side=3,at=tail(translation,1)-1,cex=cex))
          
     })
     
     output$downloadPlot <- downloadHandler(
          filename<-function(){
                paste(gsub(" ","-",input$title),"-",format(Sys.Date(), "%b-%d-%Y"), ".pdf", sep="")
          },
          content=function(file=NULL) {
               pdf(file)
               if (is.null(algaeplot()))
                    return(NULL)
               plotlist=algaeplot()
               with(plotlist, matplot(graphdata,(interval/2)*height + above_sea,type="l",col=rep("black",ncol(graphdata)), ylim=c(floor(above_sea),floor(ylim)),xlim=c(0,xlim),lty=rep(1,ncol(graphdata)),xlab="",ylab="Height (m)",xaxt="n",bty="n",cex.axis=cex,cex.lab=cex))
               #at=seq(floor(plotlist$above_sea),round(2*plotlist$ylim)/2,0.5)
               #axis(side=2,at=at)
               with(plotlist,mtext(input$title, side=3, adj=0.4, line=1.2, cex=1, font=2))
               with(plotlist,mtext(names(data[2:end]),side=1,at=translation,cex=cex))
               legendline=with(plotlist, c(tail(translation,1)-2,tail(translation,1)))
               with(plotlist,lines(legendline,c(ylim,ylim)))
               with(plotlist,mtext(legend,side=3,at=tail(translation,1)-1,cex=cex))
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
#                     rowvalue=which.min(disthover)
#                     colvalue=which.min(disthover[rowvalue,])
#                     info=cbind(colnames(graphdata)[colvalue],paste(height[rowvalue]*0.25,"m",sep=" "),paste(comparedata[rowvalue,colvalue],"%",sep=""))
#                     colnames(info)=c("Species","Height","Coverage")
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
