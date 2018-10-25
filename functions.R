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

#' Make network data usable in the form of edge list and node list.
#' 
#' @param inputNetwork A network as a two dimensional array with edges showns as matrix values of 1 and nodes as row/column names.
#' @param inputData The data of all the nodes in the above network.
#' 
#' @return Edge list and Node list of the given data.

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

#' Convert input text into HTML.
#' 
#' @param content The text as a character string.
#' 
#' @return HTML code snippet of the input string as a padded paragraph with custom font color.
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

#' Add Introduction tab in the body.
#' 
#' @param tabId A unique id to identify the tab.
#' @param mainTitle The header of the tab.
#' @param introText The introduction of the tab.
#' 
#' @return HTML code snippet to add an intro tabItem to main body.

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

#' Add a simple tab in the body.
#' 
#' @param tabId A unique id to identify the tab.
#' @param mainTitle The header of the tab.
#' @param introText The introduction of the tab.
#' @param descList The description of the plot.
#' @param plotList The plot of the tab.
#' 
#' @return HTML code snippet to add a simple tabItem to main body with 3 boxes:
#' * An introduction box
#' * A description box
#' * A plot box

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

#' Add multiple tabs in the body.
#' 
#' @param tabId A unique id to identify the tab.
#' @param mainTitle The header of the tab.
#' @param introText The introduction of the tab.
#' @param descList The list of descriptions of the plots.
#' @param plotList The list of plots of the tab.
#' 
#' @return HTML code snippet to add a multiple tabItem to main body with 3 boxes:
#' * An introduction box
#' * A description box with multiple tabs
#' * A plot box with multiple tabs

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

#' Add introduction to the tab with multiple paragraphs.
#' 
#' @param ... Any number of characters strings to be converted into separate HTML paragraphs.
#' 
#' @return HTML code snippet for single/multiple paragraphs.
createIntroduction <- function(...) {
  content <- list(...)
  contentHTML <- ''
  for (i in seq_along(content)){
    contentHTML <- paste0(contentHTML, '<p>', content[[i]], '</p>')
  }
  return (HTML(contentHTML))
}


#' Create a histogram plot.
#' 
#' @param userToken A user-specific password to show user position on the plot.
#' @param data A 1-dimensional array/ data frame of data.
#' @param color The color of histogram bars.
#' @param xlabel The x-axis label of the histogram.
#' 
#' @return A histogram plot with vertical line to show user position.

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


#' Theme for ggplot bar plot

barTheme <- function(){
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.y = element_blank(),
        legend.position="bottom",
        axis.text =element_text(size=12),
        text =element_text(size=12),
        axis.text.x=element_blank(),
        legend.text=element_text(size=12))
}

#' create a bar plot either single bar for each column or ...
#' 
#' @param userToken A user-specific password to show user position on the plot.
#' @param data An input data frame.
#' @param uniform The color of the bars, same for all bars or not.
#' @param color A single color variable or a vector of colors for the plot.
#' @param xlabel The x-axis label.
#' @param xlabs The x-axis tick labels.
#' @param single A boolean variable for single value for bar when TRUE, and mean column values when FALSE. (optional)
#' @param ylims The limits of y axis. (optional)
#' @param grouped A boolean variable for grouped bars. (optional)
#' @param grouplabs A character vector of labels of grouped bars. (optional)
#' @param groupsets An numeric vector showing which column belongs to which group. (optional)
#' 
#' @return a bar plot

createBar <- function (userToken, data, uniform, color, xlabel, xlabs = NULL, single = FALSE, ylims = NULL, 
                       grouped = FALSE, grouplabs = NULL, groupsets = c(1), legendtitle = "LEGEND"){
  
  varnames <- colnames(data)
  
  # check if limits are set, else set to 0 - 1.1 * data
  if(is.null(ylims)){
    ylims <- c(0, 1.1 * max(data, na.rm = T))
  } 
  
  # check if x-axis tick labels are set, set them for columns of each group.
  if(!is.null(xlabs)){
    colnames(data) <- rep(xlabs, length(unique(groupsets)))
  }
  
  # check if your user token exist in user password column, needed for filtering the data.
  if(userToken %in% userPassword){
    # if single value (for a particular user) is needed, filter that row.
    if(single == TRUE) {
      data <- data[which(userPassword == userToken),]
    }
    else {
      # calculate column means for all rows; removing NAs from data.
      data <- data.frame(rbind(colMeans(data, na.rm = TRUE)))
    }
    # set NAs to 0.
    data[is.na(data)] <- 0
    
    # if bars are to be grouped.
    if(grouped){
      # format data into rows for each group.
      tempdata <- data.frame(matrix(unlist(data), nrow = length(unique(groupsets)), byrow = TRUE))
      colnames(tempdata) <- xlabs
      tempdata <- cbind(grouplabs, tempdata)
      data <- melt(tempdata, id.vars = 'grouplabs')
    } else{
      data <- melt(data)
      }
    
    # if colors of all bars is same.
    if(uniform){
      # if color is not set.
      if(missing(color)){
        color = COLOR_DEFAULT
      }
      fillColor = color
    } else {
      if(missing(color)){
        color = brewer.pal(length(data), "Paired")
      }
      fillColor = data$variable
    }
    
    # plot if NO grouping.
    if(!grouped){
      ggplot(data, aes(x = variable, y = value, fill = fillColor)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(legendtitle, values = color) +
        barTheme() +
        guides(fill = guide_legend(nrow = length(varnames)))+
        labs(x = xlabel) + lims(y = ylims)
      
    } else if(grouped){
        ggplot(data, aes(x = grouplabs, y = value, fill = fillColor)) +
          geom_bar(position = "dodge", stat = "identity") +
          scale_fill_manual(legendtitle, values = color) +
          barTheme() +
          guides(fill = guide_legend(nrow = length(varnames) / length(unique(groupsets))))+
          labs(x = xlabel) + lims(y = ylims)
    }
  } 
}

#' Helper function for radar plot.
coord_radar <- function(theta='x', start=0, direction=1){
  match.arg(theta, c('x','y'))
  ggproto(
    NULL, CoordPolar, 
    theta=theta, r=ifelse(theta=='x','y','x'),
    start=start, direction=sign(direction),
    is_linear=function() TRUE)
}

#' Create a Radar Plot.
#' 
#' @param userToken A user-specific password to show user position on the plot.
#' @param data An input data frame.
#' 
#' @return A ggplot radar plot.

createRadar <- function (userToken, data){
  # determine y axis min and max from data.
  ylimMin <- min(data, na.rm = TRUE)
  ylimMax <- max(data, na.rm = TRUE)
  
  # if user exists
  if(userToken %in% userPassword){
    # filter user-specific data
    data <- data[userToken == userPassword,]
    data <- melt(data)
    
    # make the plot
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

#' Create 2 radar plots for comparison.
#' 
#' @param userToken A user-specific password to show user position on the plot.
#' @param data An input data frame.
#' @param data An input data frame for comparison.
#' @param colors A vector of both colors for radar plots.
#' @param dim Manually selected dimension (tab) highlighted when the relevant tab is selected. (optional)
#' @param ylimMin Lower limit of y-axis. (optional)
#' @param labels A character vector for legend values for two radars.
#' @param varlabels A character vector for axis labels.
#' 
#' @return Two radar plots with different colors.

createRadarComp <- function (userToken, data, dataComp, colors, dim = " ", ylimMin = 0, labels, varlabels){
  
  # set max limit of y-axis from data.
  ylimMax <- max(data, dataComp, na.rm = TRUE)
  
  if(userToken %in% userPassword){
    # filter row data for a specific user.
    data <- data[userToken == userPassword,]
    dataComp <- dataComp[userToken == userPassword,]
    
    # set column names.
    colnames(data) <- varlabels
    
    # format data into long format.
    data <- melt(data)
    dataComp <- melt(dataComp)
    
    # combine both data frames.
    data <- data.frame(data, valueComp = dataComp$value)
    
    # highlight data for selected tab by font weight and font size.
    xlab_face <- ifelse(data$variable == dim, "bold", "plain")
    xlab_size <- ifelse(data$variable == dim, 15, 12)
    
    # plot the radars.
    ggplot(data, aes(x = variable)) +
      ylim(ylimMin, ylimMax) +
      geom_point(aes(y = value, group = 1, colour = labels[1]), size = 3) + 
      geom_point(aes(y = valueComp, group = 1, colour = labels[2]), size = 3) + 
      geom_polygon(aes(y = value, group = 1), colour = colors[1], size = 1, fill = NA) +
      geom_polygon(aes(y = valueComp, group = 1), colour = colors[2], size = 1, fill = NA) +
      coord_radar() +
      theme_bw() + 
      theme(axis.line = element_blank(),
            axis.text.x = element_text(size = xlab_size, hjust = 100, face = xlab_face),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            plot.background = element_blank(),
            legend.position = "top",
            legend.title = element_blank(),
            legend.text = element_text(size=12))
  }
}

#' Melt user-specific filtered data.
#' 
#' @param userToken A user-specific password to show user position on the plot.
#' @param data An input data frame.
#' 
#' @return A two-columns data frame with variable named as colx and column 2 containing values. 

meltLineData <- function(userToken, data){
  data <- data[userToken == userPassword, ]
  
  # rename column names to colX
  for(i in 1:length(data)){
    names(data)[i] <- paste0('col',i)
  }
  data <- melt(data)
  return (data)
}

#' Format raw data into plot-able multi-line plot data.
#' 
#' @param userToken A user-specific password to show user position on the plot.
#' @param data An input data frame.
#' @param xlength Th length of x-axis/ The number of variables.
#' @param dimnames A chacter vector representing line labels in the legend. (optional)
#' 
#' @return A data frame with first column as x-axis tick labels and one line per column for the rest of the columns. 

formatLineData <- function (userToken, data, xlength, dimnames = NULL){
  df_end <- 0
  df_num <- length(data)/xlength
  dataList <- list()
  for (i in 1:df_num){
    df_start <- df_end + 1
    df_end   <- df_start + xlength - 1
    dataList[[i]] <- meltLineData(userToken, data[, df_start:df_end])
    
    # rename value column to lineX
    if(is.null(dimnames)){
      names(dataList[[i]])[names(dataList[[i]]) == 'value'] <-  paste0('line', i)
    } else {
      names(dataList[[i]])[names(dataList[[i]]) == 'value'] <-  dimnames[i]
    }
  }
  
  data <- Reduce(function(x, y) merge(x, y, all = TRUE, by = "variable"), dataList, accumulate = FALSE)
  return (data)
}

#' Create line plot with user-specific line highlighted.
#' 
#' @param userToken A user-specific password to show user position on the plot.
#' @param data An input data frame.
#' @param xlength Th length of x-axis/ The number of variables.
#' @param dim Manually selected dimension (tab) highlighted when the relevant tab is selected. (optional)
#' @param xlabs A character vector of x-axis tick labels with length equal to xlength. (optional)
#' @param dimnames A chacter vector representing line labels in the legend. (optional)
#' 
#' @return A plot with multiple lines.

createLine <- function(userToken, data, xlength, dim = " ", xlabs = NULL, dimnames = NULL){
  
  if(userToken %in% userPassword){
    data <- formatLineData(userToken, data, xlength, dimnames = dimnames)
    names(data)[names(data) == 'variable'] <- 'xlabel'
    
    if(!is.null(xlabs)){
      data$xlabel <- xlabs
    }
    
    data <- melt(data, id.vars = "xlabel")
    data$line_size <- ifelse(data$variable == dim, 2, 0.8)
    
    ggplot(data, aes(x = xlabel, y = value, group = variable, color = variable)) + 
      geom_line(aes(size = line_size)) +
      geom_point(aes(size = line_size)) +
      theme_bw() + 
      theme(axis.text = element_text(size=12),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            plot.background = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.text = element_text(size = 12)
           ) +
      guides(size = FALSE) +
      labs(color = "LEGEND") +
      scale_x_discrete(limits = xlabs)
  }
}
