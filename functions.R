###################
# functions.R
# 
# - DATA HANDLING
# - UI FUNCTIONS
# -- SIDEBAR
# -- BODY
# - SERVER FUNCTIONS
#
##################

#################################
# DATA HANDLING
#################################
dataNetwork <- function (inputNetwork, inputData){
  
  # create edgeList
  edgeNetwork <- graph.adjacency(inputNetwork)
  edgeList <- get.edgelist(edgeNetwork)
  edgeList <- data.frame(edgeList, rep (1,nrow(edgeList)))
  colnames(edgeList) <- c("SourceName", "TargetName", "Weight")
  getNodeID <- function(x){which(x == V(edgeNetwork)$name) - 1}
  edgeList <- plyr::ddply(edgeList, .variables = c("SourceName", "TargetName", "Weight"), 
                          function (x) data.frame(SourceID = getNodeID(x$SourceName), 
                                                  TargetID = getNodeID(x$TargetName)))
  
  #create nodeList
  nodeData <- inputData
  nodeData$ID <- nodeData$ID - 1
  nodeList <- data.frame(nName = as.character(V(edgeNetwork)$name), nodeData)
  
  outNodeoutEdge <- list("edgeList" = edgeList,"nodeList" = nodeList)
  return (outNodeoutEdge)
}

#nodeList <- dataNetwork(toynetwork, toydata)$nodeList
#edgeList <- dataNetwork(toynetwork, toydata)$edgeList

#################################
# DATA HANDLING (END)
#################################



################################################################
# UI FUNCTIONS
################################################################

##########################################
## SIDEBAR
##########################################
AddHelpText <- function(content){
  helpText(
    HTML(
      paste0('<p style="color:#3c8dbc; padding: 0px 15px;">', content, '</p>')
    )
  )  
}

##########################################
## BODY
##########################################
AddIntroductionTab <- function (tabId, mainTitle, introText){
  tabItem(tabName = tabId,
          fluidRow(
            box(title = mainTitle, width = 12,
                status = "primary", solidHeader = TRUE,
                htmlOutput(introText)
            )
          )
  )
}

AddSimpleTab <- function (tabId, mainTitle, introText, descList, plotList){
  tabItem(tabName = tabId,
          fluidRow(
            box(
              title = mainTitle, width = 12,
              status = "primary", solidHeader = TRUE,
              htmlOutput(introText)
            )
          ),
          fluidRow(
            box(title = "Description", width = 7,
                status = "primary", solidHeader = TRUE,
                htmlOutput(descList)
            ),
            box(title = "Plot", width = 5,
                status = "primary", solidHeader = TRUE,
                plotOutput(plotList)
            )
          )
  )
}

AddMultiTab <- function (tabId, mainTitle, introText, headList, descList, plotList){
  tabItem(tabName = tabId,
          fluidRow(
            box(title = mainTitle, width = 12,
                status = "primary", solidHeader = TRUE,
                htmlOutput(introText)
            )
          ),
          fluidRow(
            do.call(tabBox,
                    c(id = paste0(tabId, "TB"), width = 12, side = "left",
                      lapply(1:length(headList), function(i){
                        tabPanel(
                          title = headList[[i]],
                          box(title = "Description", width = 7,
                              status = "primary", solidHeader = TRUE,
                              htmlOutput(descList[[i]])
                          ),
                          box(title = "Data", width = 5,
                              status = "primary", solidHeader = TRUE,
                              plotOutput(plotList[[i]])
                          )
                        )
                      })
                    )
            )
          )
  )
}

################################################################
# SERVER FUNCTIONS
################################################################
createIntroduction <- function(...) {
  content <- list(...)
  contentHTML <- ''
  for (i in seq_along(content)){
    contentHTML <- paste0(contentHTML, '<p>', content[[i]], '</p>')
  }
  return (HTML(contentHTML))
}

createHist <- function (userToken, data, color, xlabel){
  hist(data,
       col = color,
       main = NULL,
       xlab = xlabel
  ) 
  if(userToken %in% userPassword){
    lineValue <- data[userPassword == userToken]
    if(!is.na(lineValue)){
      abline(v = lineValue, col = "red", lwd = 2)
    }
  }
}



createBar <- function (userToken, data, uniform, color, xlabel,xlabs=NULL, single = FALSE, ylims=NULL, ownscore=NULL, grouped=FALSE, grouplabs=NULL,groupsets=NULL){
  varnames<-colnames(data)
  if(is.null(ylims)){
    ylims<-c(0,max(data,na.rm=T)+0.1*max(data,na.rm=T))
  } 
  
  if(!is.null(xlabs)){
    if(grouped==F){
      colnames(data)<-xlabs
    } else {
      colnames(data)<-rep(xlabs,length(unique(groupsets)))
    }
  }
  
  if((userToken %in% userPassword)&(single==TRUE)){
    data<-data[userPassword == userToken,]
  } else {
    datatemp<-data[1,]
    datatemp[1,]<-colMeans(data)
    data<-datatemp
  }
  data[is.na(data)]<-0
  
  if(grouped==T){
    tempdata<-data.frame(matrix(unlist(data),nrow=length(unique(groupsets)),byrow=T))
    colnames(tempdata)<-xlabs
    tempdata<-cbind(grouplabs,tempdata)
    #tempdata$grouplabs<-as.character(tempdata$grouplabs)
    data<-melt(tempdata,id.vars = 'grouplabs')
  } else {
    data <- melt(data)
  }
  
  if(uniform){
    if(missing(color)){
      color = COLOR_DEFAULT
    }
    fillColor = color
  } else {
    if(missing(color)){
      color = brewer.pal(length(data),"Paired")
    }
    fillColor = data$variable
  }
  
  
  
  if(grouped==F){
    ggplot(data, aes(x = variable, y = value, fill = fillColor)) +
      geom_bar(stat = "identity") +
      scale_fill_manual("LEGEND", values = color) +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.title.y = element_blank(),
            legend.position="right",
            axis.text =element_text(size=12),
            text =element_text(size=12),
            axis.text.x=element_blank(),
            legend.text=element_text(size=12)) +
      labs(x = xlabel)+lims(y=ylims)
    
  } else {
    if(grouped==T){
      ggplot(data, aes(x = grouplabs, y = value, fill = fillColor)) +
        geom_bar(position="dodge", stat = "identity") +
        scale_fill_manual("LEGEND", values = color) +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black"),
              axis.title.y = element_blank(),
              legend.position="right",
              axis.text =element_text(size=12),
              text =element_text(size=12),
              legend.text=element_text(size=12)) +
        labs(x = xlabel)+lims(y=ylims)
      
    }
  }
  
  
}

# Helper function for radar plot
coord_radar <- function(theta='x', start=0, direction=1){
  match.arg(theta, c('x','y'))
  
  ggproto(
    NULL, CoordPolar, 
    theta=theta, r=ifelse(theta=='x','y','x'),
    start=start, direction=sign(direction),
    is_linear=function() TRUE)
}

createRadar <- function (userToken, data){
  ylimMin <- min(data, na.rm = TRUE)
  ylimMax <- max(data, na.rm = TRUE)
  if(userToken %in% userPassword){
    data <- data[userToken == userPassword,]
    data <- melt(data)
    ggplot(data, aes(x = variable, y = value, group=1)) +
      ylim(ylimMin, ylimMax) +
      geom_point(aes(colour = "YOU"), size = 3) + 
      geom_polygon(colour = "red", size = 1, fill = NA) +
      scale_colour_manual("", values = c("YOU" = "red"))  +
      coord_radar() +
      theme_bw() + 
      theme(axis.line=element_blank(),
            axis.text.x = element_text(size=12,hjust=100),
            axis.text.y =element_blank(),
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            legend.position="top",
            panel.background=element_blank(),
            panel.border=element_blank(),
            plot.background=element_blank())
  }
}


createRadarComp <- function (userToken, data, dataComp, colors, dim = " ",ylimMin=0,labels, varlabels){
  
  ylimMax <- max(data, dataComp, na.rm = TRUE)
  
  
  if(userToken %in% userPassword){
    data <- data[userToken == userPassword,]
    dataComp <- dataComp[userToken == userPassword,]
    
    colnames(data)<-varlabels
    
    data <- melt(data)
    dataComp <- melt(dataComp)
    
    data <- data.frame(data, valueComp = dataComp$value)
    xlab_face <- ifelse(data$variable == dim, "bold", "plain")
    xlab_size <- ifelse(data$variable == dim, 15, 12)
    
    ggplot(data, aes(x = variable)) +
      ylim(ylimMin, ylimMax) +
      geom_point(aes(y = value, group=1, colour = labels[1]), size = 3) + 
      geom_point(aes(y = valueComp, group=1, colour = labels[2]), size = 3) + 
      geom_polygon(aes(y = value, group=1), colour = colors[1], size = 1, fill = NA) +
      geom_polygon(aes(y = valueComp, group=1), colour = colors[2], size = 1, fill = NA) +
      # scale_colour_manual("", values = c(labels[1] = colors[1], labels[2] = colors[2]))  +
      coord_radar() +
      theme_bw() + 
      theme(axis.line=element_blank(),
            axis.text.x = element_text(size = xlab_size, hjust = 100, face = xlab_face),
            axis.text.y =element_blank(),
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.background=element_blank(),
            panel.border=element_blank(),
            plot.background=element_blank(),
            legend.position = "top",
            legend.title=element_blank(),
            legend.text=element_text(size=12))
  }
}


meltLineData <- function(userToken, data){
  data <- data[userToken == userPassword,]
  for(i in 1:length(data)){
    names(data)[i] = paste0('col',i)
  }
  data <- melt(data)
  return (data)
}

formatLineData <- function (userToken, data, xlength, dimnames=NULL){
  df_end <- 0
  df_num <- length(data)/xlength
  dataList <- list()
  for (i in 1:df_num){
    df_start <- df_end + 1
    df_end   <- df_start+xlength - 1
    dataList[[i]] <- meltLineData(userToken, data[,df_start:df_end])
    if(is.null(dimnames)){
      names(dataList[[i]])[names(dataList[[i]]) == 'value'] <-  paste0('line',i)
    } else {
      names(dataList[[i]])[names(dataList[[i]]) == 'value'] <-  dimnames[i]
    }
    
  }
  
  data <- Reduce(function(x, y) merge(x, y, all=T,by="variable"), dataList, accumulate=F)
  return (data)
}

createLine <- function(userToken, data, xlength, dim = " ",xlabs=NULL,dimnames=NULL){
  if(userToken %in% userPassword){
    
    data <- formatLineData(userToken, data, xlength, dimnames=dimnames) 
    names(data)[names(data) == 'variable'] <- 'xlabel'
    
    if(!is.null(xlabs)){
      data$xlabel<-xlabs
    }
    
    data <- melt(data, id.vars = "xlabel")
    data$line_size <- ifelse(data$variable == dim, 2, 0.8)
    
    ggplot(data, aes(x=xlabel, y=value, group=variable, color = variable)) + 
      geom_line(aes(size = line_size)) +
      geom_point(aes(size = line_size)) +
      theme_bw() + 
      theme(axis.text=element_text(size=12),
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.background=element_blank(),
            panel.border=element_blank(),
            plot.background=element_blank(),
            axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.text=element_text(size=12)
      ) +
      guides(size=FALSE)+
      labs(color = "LEGEND")+
      scale_x_discrete(limits=xlabs)
  }
}
