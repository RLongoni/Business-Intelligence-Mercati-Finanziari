# See the guide : https://google.github.io/styleguide/Rguide.xml 
# Business Intelligece Project
# Riccardo Longoni 806853
# 
# ToInstall:
# install.packages("quantmod")
# install.packages("dygraphs")
# install.packages('tseries')
# install.packages('PerformanceAnalytics')
# install.packages('forecast')
require(quantmod)
require(dygraphs)
require(tseries)
require(zoo)
library(PerformanceAnalytics)
require(forecast)

 #data used in the function plotSymbol() to showing returns, historigrams, plots, boxplots etc... 
Name_Titles <-c("AAPL","MSFT","SNE","EBAY","AMZN","FOXA","NFLX")
Symbols <- list(APPLE=    "Apple",
             MICROSOFT="Microsoft",
             SONY=     "Sony",
             EBAY=     "Ebay",
             AMAZON=   "Amazon",
             FOX=      "Fox",
             NETFLIX=  "Netflix"
            )

for(i in 1:length(Name_Titles)){
  data.z<-get.hist.quote(instrument=Name_Titles[i],
                         start="2007-10-01",
                         end= "2017-10-31",
                         quote="AdjClose", 
                         provider="yahoo", 
                         origin="1970-01-01", 
                         compression="m", 
                         retclass="zoo"
                        )
  data.z <- data.z[-length(data.z),]
  index(data.z) <- as.yearmon(index(data.z))
  colnames(data.z) <- Name_Titles[i]
  if(!exists("dataset.z"))
    dataset.z <- data.z
  else
    dataset.z <- merge(dataset.z, data.z)
}


# used to visualize titles 
for(i in 1:length(Name_Titles)){
    Symbols[i] <- getSymbols(Name_Titles[i],
                     src="yahoo",
                     from=as.Date("2007/10/01"),
                     to=as.Date("2017/10/31") 
                    )
}

########## Settore Elettronica
 Symbols[[1]] <- AAPL[-length(AAPL), ] 
 Symbols[[2]] <- MSFT[-length(MSFT), ] 
 Symbols[[3]] <- SNE[-length(SNE), ] 
 
 DatasetElettronica <- list(AAPL, MSFT, SNE)
 Symbols_Elettronica <- c("AAPL", "MSFT", "SNE")
 SettoreElettronica.z <- cbind(dataset.z[, 1], dataset.z[, 2], dataset.z[, 3])
 colnames(SettoreElettronica.z)<-c("AAPL", "MSFT", "SNE")
######### Settore Vendite Online  
 Symbols[[8]] <- AMZN[-length(AMZN), ] 
 Symbols[[9]] <- EBAY[-length(EBAY), ] 
 
 DatasetOnlineStore <- list(AMZN, EBAY)
 Symbols_OnlineStore <- c("AMZN", "EBAY")
 SettoreVenditeOnline.z <- cbind(dataset.z[, 4], dataset.z[, 5])
 colnames(SettoreVenditeOnline.z)<-c("EBAY", "AMZN")
#########Settore Intrattenimento
 Symbols[[10]] <- FOXA[-length(FOXA), ] 
 Symbols[[11]] <- NFLX[-length(NFLX), ] 
 
 DatasetIntrattenimento <- list(FOXA, NFLX)
 Symbols_Intrattenimento <- c("FOXA", "NFLX")
 SettoreIntrattenimento.z <- cbind(dataset.z[, 6], dataset.z[, 7])
 colnames(SettoreIntrattenimento.z)<-c("FOXA", "NFLX")

drawTitle <- function(title, symbol, column){
    # Args: 
    # title = object that contains a set of values, 
    #         the values represent every AdjustedClose, Close, Open, High or Low Value of a title
    # symbols = string like "AAPL", "MSFT", "UBI.MI"
    # column = string like "Adjusted", "Close", "Open" etc
    #
    # Returns:
    # The function will open an HTML containing the title plotted

    dygraph(filterDataset(title, column))%>%
    dySeries(name=rmSpaces(paste(symbol, ".", column)), 
                label=symbol)%>%
    dyOptions(stackedGraph=FALSE,fillGraph=TRUE)%>%
    dyRangeSelector(height=125)
    
    
}
drawTitles <- function(dataset, titles, column){
    # given a dataset and a list of titles, the function will draw in an HTML file
    #
    # Args:
    # dataset = is list of object
    # titles = is a vector of strings
    # column = is a single string (Open, Closed, Adjusted .... )
    #
    # Returns:
    # The function will open an HTML File in which there will be all the information of a dataset.
    if (!exists(deparse(substitute(dataset)))){
            #### dataset <- cbind(AAPL,MSFT) # WARNING!! at the end of the function this variable will be deleted
            return ("dataset non presente!")
        }
    #datasetFiltered <- filterDataset(dataset, column)
    toEvaluate <- rmSpaces(paste("dygraph(",
                                 deparse(substitute(filterDataset(dataset, column))),
                                 ")")) # toEvaluate = 'dataset(<dataSetName>)'
    if (length(dataset) > 12){
        drawTitle(dataset, titles, column) 
    }else{
        for(title in titles){
            toEvaluate <- paste(toEvaluate,
                                "%>%",
                                paste("dySeries(name='",
                                      rmSpaces(paste(title,
                                                     paste(".",
                                                           paste(column, "',")
                                                          )
                                                    )
                                               ),
                                rmSpaces(paste("label='",title)), 
                                         paste(column, "')")
                                         )) # toEvaluate = 'dataset(<dataSetName>) %>% dySeries(name='<nameTitle>.Open',label='<nameTitle> Open') %>% repeat as long as titles ends
            toEvaluate <- rmSpaces(toEvaluate) # toEvaluate = 'dataset(...)%>%dySeries(...)
            toEvaluate <- paste(toEvaluate,
                                "%>%dyOptions(stackedGraph=FALSE,fillGraph=TRUE)",
                                "%>%dyRangeSelector(height=125)") # toEvaluate = 'dataset(...)%>%dySeries(...)%>%dyOptions(stackedGraph=TRUE)%>%"%>%dyRangeSelector(height=125)" NB: you can change the parameters of the last 2 functions
        }
    }
    eval(parse(text=toEvaluate)) # this will open an HTML page with all information you have insert
}

drawReturns <- function(titles, typeOfReturn, symbol){
    # This Function is used to calculate and plot the returns, simpler or compounded, of a set of titles 
    #
    # Args: 
    # titles = .zoo which contains every AdjustedClose of any Titles 
    # typeOfReturns = "Simple", "Compounded"
    # symbol = it's a string used in the function dySeries to set the label name
    # Returns: 
    # the function will return a .zoo object which containing the simple or compoundend returns.
    # Furthermore the function will use dygraph to plot the result
    
    if(typeOfReturn == "Simple" || typeOfReturn == "Semplice"){
        titlesreturns <- diff(titles) / lag(titles, k = -1)
    }else if(typeOfReturn == "Compounded" || typeOfReturn == "Composto"){
        titlesreturns <- diff(log(titles))
    }else{
       cat("Non è stato selezionato alcun tipo di ritorno!")
    }
    dygraph(titlesreturns)%>%
    dySeries(label=symbol)%>%
    dyOptions(stackedGraph=FALSE,fillGraph=TRUE)%>%
    dyRangeSelector(height=125)
}
drawTitleData <- function(title, typeReturn, name, color){ 
    # The function is used to calculate and to show the returns of a given title
    # Args:
    # title = a column of a .zoo object 
    # typeRetrun = it's a string and it can be "Simple"/"Semplice" or "Compounded"/"Composto"
    # name = it's a string and it's the title for the plot data
    # color = it's a string and it gives the color to all the plots

    if(typeReturn == "Simple" || typeReturn == "Semplice"){
        TitlesReturns <- diff(title) / lag(title, k = -1)
    }
    if(typeReturn == "Compounded" || typeReturn == "Composto"){
      TitlesReturns <- diff(log(title))
    }
    par(mfrow=c(2, 2))
    hist(coredata(TitlesReturns),
         main="Histogram",
         xlab = "Returns", 
         probability=TRUE,
         col=color, 
         ylim = c(0, 5) 
        )
    plot(density(coredata(TitlesReturns), na.rm=TRUE),
         type="l",xlab="cc return", 
         lwd=2,
         col=color, 
         ylab="density estimate", 
         main="Smoothed density"
        )
    boxplot(coredata(TitlesReturns), 
            main="Boxplot of monthly cc returns",
            col=color,
            ylab="monthly cc return"
           )
    qqnorm(coredata(TitlesReturns), 
           main="QQ-plot of monthly cc returns",
           col=color
          )
    qqline(coredata(TitlesReturns))
    title(name, outer = TRUE)
    par(mfrow=c(1,1))
}

#DESCRIPTIVE STATISTICS
#Esempio titleDescritptiveStats(dataset.z, "Compounded", Name_Titles)
titleDescritptiveStats <- function (titlesData, typeReturn, name_Titles ){
    # This function will show all the descriptive statistics of a given zoo object
    # Args: 
    # titlesData = it's a zoo object wich contains all the titles AdjustedClose
    # typeReturn = it's a string used to decide which type of return the user wants
    # name_Title = it's a list of strings, every name corresponds a specific title
    
    nameTitles <- name_Titles
    if(typeReturn == "Simple" || typeReturn == "Semplice"){
        TitlesReturns <- diff(titlesData) / lag(titlesData, k = -1)
    }
    if(typeReturn == "Compounded" || typeReturn == "Composto"){
      TitlesReturns <- diff(log(titlesData))
    }
    cat("\n","Statistica Descrittiva","\n")
    descriptivestatistics<-matrix(nrow = 7, ncol = 10)
    colnames(descriptivestatistics)<-c("MEAN", 
                                       "VARIANCE", 
                                       "SD", 
                                       "SKEWNESS", 
                                       "KURTOSIS", 
                                       "0%", 
                                       "25%", 
                                       "50%", 
                                       "75%", 
                                       "100%"
                                      )
    rownames(descriptivestatistics)<-nameTitles
    for(i in 1:length(nameTitles)){
      descriptivestatistics[i,1]<-mean(coredata(TitlesReturns[,i]))
      descriptivestatistics[i,2]<-var(coredata(TitlesReturns[,i]))
      descriptivestatistics[i,3]<-sd(coredata(TitlesReturns[,i]))
      descriptivestatistics[i,4]<-skewness(coredata(TitlesReturns[,i]))
      descriptivestatistics[i,5]<-kurtosis(coredata(TitlesReturns[,i]))
      descriptivestatistics[i,6:10]<-quantile(coredata(na.remove(TitlesReturns[,i])))
    }
    print(descriptivestatistics)
}
 
showCovarCorr <- function(titlesData ,name_Titles, typeReturn){
    # The Function is used to calculate and to show the covariance and correlation of a zoo object
    # Args: 
    # titlesData = it's a zoo object which contains all the title AdjustedClose
    # name_Title = it's a list of strings, every string it's a title name
    # typeReturn = it's a string used to calculate the type of return that we want to use 
    nameTitles <- name_Titles
    if(typeReturn == "Simple" || typeReturn == "Semplice"){
        TitlesReturns <- diff(titlesData) / lag(titlesData, k = -1)
    }
    if(typeReturn == "Compounded" || typeReturn == "Composto"){
      TitlesReturns <- diff(log(titlesData))
    }
    #COVARIANCE
    covariance<-matrix(nrow = 7, ncol = 7)
    colnames(covariance)<-nameTitles
    rownames(covariance)<-nameTitles
    for(i in 1:length(nameTitles)){
      for(j in 1:length(nameTitles)){
        covariance[i,j]<- var(TitlesReturns[,i],TitlesReturns[,j])
      }
    }
    cat("\n","COVARIANCE","\n")
    print(covariance)

    cat("\n","CORRELATION","\n")
    print(cor(coredata(dataset.z)))

}

drawScatterPlots <- function(dataset, mainTitle, colour){
    # this function will show th ScatterPlots of a give .zoo object
    # Args: 
    # dataset = it's a zoo object 
    # mainTitle = it's a string used in the plot, "Title1-Title2-..."
    # colour = it's a string used to set the colour of the plot
    pairs(coredata(dataset), main=mainTitle, col=colour, cex=0.7)
}

armaModel <- function(title, titleArmaModel, typeReturn, color){
    # The function is used to show the arma model
    # Args: 
    # title = it's a title of a give .zoo object
    # titleArmaModel = it's a string used in the plot
    # typeReturn = it's a string used 
    if(typeReturn == "Simple" || typeReturn == "Semplice"){
        TitleReturns <- diff(title) / lag(title, k = -1)
    }
    if(typeReturn == "Compounded" || typeReturn == "Composto"){
      TitleReturns <- diff(log(title))
    }
    fitRet <- stl(TitleReturns, s.window="period")
    plot(fitRet, main=titleArmaModel, col=color)
}

singleArimaModel <- function(title, titleArimaModel) {
    # The function is used to show the arima model 
    # Args: 
    # title = it's a title taken from a given .zoo object
    # titleArimaModel = it's a string used in the main title of the plot
    TitleReturns <- diff(log(title))
    par(mfrow=c(2,3))
    TrainingSet <- TitleReturns[1:80]  
    TestSet <- TitleReturns[80:110]
    #Showing the predictions
    for (j in 1:6){
        fit <- arima(TrainingSet, order = c(j, 1, j))
        arima.preds <- predict(fit, 
                               n.ahead=(length(TitleReturns) - 10))$pred
        arima.forecast <- forecast(fit, h = 25)
        plot(arima.forecast, main = titleArimaModel)
        accuracy(arima.preds, TestSet)[j] 
        lines( TestSet )
        abline(h=0)
    }
    par(mfrow=c(1,1))

}

#############################
# Utils and Other functions #
############################# 
rmSpaces <- function(string){
    # remove the spaces in a string
    return (gsub(" ","",string))
}

filterDataset <- function(dataset, column){
    # function used to filter the dataset, it return a column of the previous dataset
    # Args:
    # dataset = is a list of titles (AAPL,SNE, ...)
    # column =  is a string 
    # returns:
    # a cbinded object
    tmp <- 0
    if(column == "Open"){
        tmp <- 1
    }else if(column == "High"){ 
        tmp <- 2 
    }else if(column == "Low"){ 
        tmp <- 3
    }else if(column == "Close"){ 
        tmp <- 4 
    }else if(column == "Volume"){
        tmp <- 5
    }else if(column == "Adjusted"){
        tmp <- 6
    }else if(column == "All")
    {
        element <- dataset[[1]]
        FilteredDataset <- element[, ]
        for(i in 2:length(dataset)){
            FilteredDataset <- cbind(FilteredDataset,dataset[[i]][, ])
        }
        return (FilteredDataset)
    }
    if(length(dataset)>12){
        FilteredDataset <- cbind(dataset[, tmp])
    }else{
        element <- dataset[[1]]
        FilteredDataset <- element[, tmp]
        for(i in 2:length(dataset)){
            element <- dataset[[i]]
           FilteredDataset <- cbind(FilteredDataset, element[, tmp])
        }
        return (FilteredDataset)
    }
}

