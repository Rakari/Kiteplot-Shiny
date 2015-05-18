
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
     algaeplot<-reactive({ 
          inFile <- input$file1
          
          if (is.null(inFile))
               return(NULL)
          
          interval = as.numeric(input$interval)
          if (is.na(interval)) {
               interval = 0.25
          }
          
          data = read.xlsx(inFile$datapath, sheetIndex=1)
          unit = as.numeric(input$unit)
          if (is.na(unit)) {
               unit = 1
          }
          
          end=ncol(data)
          
          #Extract duplicates of same elements in first column
          height=unique(data[,1])  
          data=ddply(data,names(data)[1],colwise(meanNA))
          data=clean(data)
          height=data[,1]
          maxdata=max(data[,2:end],na.rm=T)
          
          if(input$method == "prop"){
               data=cbind(height,data[,2:end]/100)
          } else {
               data=cbind(height,data[,2:end]/maxdata)
               
          }
          
          translation=seq(1,1+3*(end-2),3)
          graph=t(t(data[,2:end])+translation)
          graphmirror=t(translation-t(data[,2:end]))
          graphdata=cbind(graph,graphmirror)
          ylim=interval/2*max(height)+1
          xlim=tail(translation,1)+2
          cex=0.7
          
          legend = ""
          
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
               "data"=data))
     })   
     
     output$algaeplot <- renderPlot({
          if (is.null(input$file1))
               return(NULL)
          
          plotlist=algaeplot()
          with(plotlist, matplot(graphdata,interval/2*height,type="l",col=rep("black",ncol(graphdata)), ylim=c(0,ylim),xlim=c(0,xlim),lty=rep(1,ncol(graphdata)),xlab="",ylab="Height (m)",xaxt="n",bty="n",cex.axis=cex,cex.lab=cex,main = input$title))
          with(plotlist,mtext(names(data[2:end]),side=1,at=translation,cex=cex))
          legendline=with(plotlist, c(tail(translation,1)-2,tail(translation,1)))
          with(plotlist,lines(legendline,c(ylim-0.375,ylim-0.375)))
          with(plotlist,text(x=tail(translation,1)-1,y=ylim,legend,cex=cex))
     })
     
     output$downloadPlot <- downloadHandler(
          filename=paste(paste(sample(c(0:9, letters, LETTERS), 10, replace=TRUE), collapse = ""), "pdf", sep="."), 
          content=function(file=NULL) {
               pdf(file, height=5,width=ncol(readData()))
               if (is.null(input$file1))
                    return(NULL)
                    plotlist=algaeplot()
                    
                    with(plotlist, matplot(graphdata,interval/2*height,type="l",col=rep("black",ncol(graphdata)), ylim=c(0,ylim),xlim=c(0,xlim),lty=rep(1,ncol(graphdata)),xlab="",ylab="Height (m)",xaxt="n",bty="n",cex.axis=cex,cex.lab=cex,main = input$title))
                    with(plotlist,mtext(names(data[2:end]),side=1,at=translation,cex=cex))
                    legendline=with(plotlist, c(tail(translation,1)-2,tail(translation,1)))
                    with(plotlist,lines(legendline,c(ylim-0.375,ylim-0.375)))
                    with(plotlist,text(x=tail(translation,1)-1,y=ylim,legend,cex=cex))
               dev.off()
          }
     )

})
