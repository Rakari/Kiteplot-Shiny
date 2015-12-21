Kiteplot1 <- function(data,interval=0.25,unit=1,above_sea = 0,sheetnr= 1,title="",method="prop",ylab="",TypeOfYAxis="Height",legendScale=""){
    require(SirKR)
    require(xlsx)
    interval = as.numeric(interval)
    if (is.na(interval)) {
        interval = 0.25
    }
    
    unit = as.numeric(unit)
    if (is.na(unit)) {
        unit = 1
    }
    
    above_sea = as.numeric(above_sea)
    if(is.na(above_sea)) {
        above_sea = 0
    }
    sheetnr=as.numeric(sheetnr)
    if(is.na(sheetnr)) {
        sheetnr= 1
    }
    #reads in xlsx or xls file chosen
    realdata = read.xlsx(data, sheetIndex=sheetnr)
    
    # if(is.null(realdata)) return(NULL)
    
    end=ncol(realdata)
    
    data=clean(realdata)
    height=(data[,1]-2)*interval/2 + above_sea
    maxdata=max(data[,2:end],na.rm=T)
    names=names(data[2:end])
    if(method == "prop"){
        data=cbind(height,data[,2:end]/100)
    } else {
        data=cbind(height,data[,2:end]/maxdata)
    }
    
    translation=seq(1,1+3*(end-2),by=3)
    graph=t(t(data[,2:end])+translation)
    graphmirror=t(translation-t(data[,2:end]))
    graphdata=cbind(graph,graphmirror)
    ylim=floor(max(height)+1)
    xlim=tail(translation,1)+4
    cex=0.7
    
    legend = ""
    legendScale=as.numeric(legendScale)
    if(is.na(legendScale)){
        legendScale=maxdata
    }
    #legend customized by input$method from user interface
    if(method == "prop"){
        legend = "100%"
    }
    if(method == "individ"){
        legend = paste(legendScale," individuals") 
    }
    if(method == "biomass"){
        legend = bquote(.(legend))
        if(unit == 1)  {
            legend = bquote(.(paste(maxdata," g/","m", sep=""))^2)
        } else {
            legend = bquote(.(paste(maxdata," g/",unit, "m", sep=""))^2)
        }
    }
    
    if(TypeOfYAxis!="Height"){
        initial=height[1]
        if(is.na(initial)){
            initial=0
        }
        height=height+ylim+2*interval/2-2*(height-height[1])
        #graphdata=graphdata[nrow(graphdata):1,]
    }
    matplot(graphdata,height,type="l",
            col=rep("black",ncol(graphdata)),xlim=c(0,xlim),lty=rep(1,ncol(graphdata)),xlab="",
            ylab=ylab,yaxt="n", xaxt="n",bty="n",cex.axis=cex,cex.lab=cex)
    mtext(title, side=3, adj=0.4, line=1.2, cex=1, font=2)
    mtext(names,side=1,at=translation,cex=cex)
    legendline=c(xlim-(1+legendScale/maxdata),xlim-1+legendScale/maxdata)
    lines(legendline,c(ylim,ylim))
    mtext(legend,side=3,at=xlim-1,cex=cex)
    #axis(2, at = c(ylim),label=ylim)
    yAxisValues=par()$yaxp
    if(TypeOfYAxis=="Height"){
        axis(side=2,at=c(setdiff(seq(yAxisValues[1],yAxisValues[2],length.out=yAxisValues[3]+1),yAxisValues[2]),ylim),
             labels=c(setdiff(seq(yAxisValues[1],yAxisValues[2],length.out=yAxisValues[3]+1),yAxisValues[2]),ylim),cex.axis=cex,cex.lab=cex)
        
    }else{
        #axis(2, at = c(ylim),label=0)
        axis(side=2,at=sort(c(ylim-setdiff(seq(yAxisValues[1],yAxisValues[2],length.out=yAxisValues[3]+1),yAxisValues[2]),0)),
             labels=c(ylim,setdiff(seq(yAxisValues[2],yAxisValues[1],length.out=yAxisValues[3]+1),yAxisValues[2])),cex.axis=cex,cex.lab=cex)
    }
}