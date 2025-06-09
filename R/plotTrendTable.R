 

plotTrendTable <- function(csite, timepoint = NULL, display_type = "Trend", 
                       color_type = "All",substance = "NULL") {
  
  
  
  
  
  #if (is.null(timepoint) || class(timepoint) != "Date")
  if(is.null(timepoint) || !inherits(timepoint,"Date"))
    stop("Need to specify valid timepoint of class \"Date\".")
  
  
  if (display_type == "Trend") {
    
    temp.traffic.Beta.ND.Check <- csite$Traffic.Lights$Beta.ND.Check[,, as.character(timepoint), drop = F]
    temp.traffic.Betas <- csite$Traffic.Lights$Betas[,,as.character(timepoint), drop = F]
    Well.Names <- dimnames(temp.traffic.Betas)[[1]]
    Cont.Names <- dimnames(temp.traffic.Betas)[[2]]
  }
  
  if (display_type == "Threshold - Statistical") {
    
    temp.traffic.Beta.ND.Check <- csite$Traffic.Lights$Beta.ND.Check[,,as.character(timepoint), drop = F]
    temp.traffic.Ulims <- csite$Traffic.Lights$Smooth.Upper.lims[,, as.character(timepoint), drop = F]
    Well.Names <- dimnames(temp.traffic.Ulims)[[1]]
    Cont.Names <- dimnames(temp.traffic.Ulims)[[2]]
    
  }
  
  if (display_type == "Threshold - Absolute") {
    
    temp.traffic.Abs.Thresh.Check <- csite$Traffic.Lights$Abs.Thresh.Check[,, as.character(timepoint), drop = F]
    Well.Names <- dimnames(temp.traffic.Abs.Thresh.Check)[[1]]
    Cont.Names <- dimnames(temp.traffic.Abs.Thresh.Check)[[2]]
  }

  
  #
  # Define lookup list to retrieve 
  #

  col_trend <- list()
  col_trend[["#1A9641"]] <- "Strong downward trend"  # green
  col_trend[["#A6D96A"]] <- "Downward trend"         # light-green
  col_trend[["White"]]   <- "Stable"                 # white
  col_trend[["#FDAE61"]] <- "Upward trend"           # orange
  col_trend[["#D7191C"]] <- "Strong upward trend"    # red
  col_trend[["grey"]]    <- "Not available"          # grey
  col_trend[["blue"]]    <- "Non-Detect data"        # blue
  
  col_thres <- list()  
  col_thres[["grey"]]    <- "Not available"          # grey
  col_thres[["blue"]]    <- "Non-Detect data"        # blue
  col_thres[["#1A9641"]] <- "Below threshold limit"  # green
  col_thres[["#D7191C"]] <- "Above threshold limit"  # red
  
  # DON'T USE SUBSET! ITS A BASE PCK FCT.
  # if (subset == TRUE) {
  #   
  #   if(length(Cont.Names)==1){Cont.Names<-Cont.Names[1]}else{Cont.Names<-GWSDAT.select.list(Cont.Names,multiple =T,title = "Select Solutes to Plot",preselect=Cont.Names)}
  #   if(length(Cont.Names)==0){stop("No Solutes selected!")}
  #   
  #   Well.Names<-GWSDAT.select.list(Well.Names,multiple =T,title = "Select Wells to Plot",preselect=Well.Names)
  #   if(length(Well.Names)==0){stop("No Wells selected!")}
  #   
  # }
  Cont.Names <- substance   ### Obtaining the selected substances
  if (display_type == "Trend") {
    
    temp.traffic.Beta.ND.Check <- csite$Traffic.Lights$Beta.ND.Check[Well.Names, Cont.Names, as.character(timepoint), drop = F]
    temp.traffic.Betas <- csite$Traffic.Lights$Betas[Well.Names, Cont.Names, as.character(timepoint), drop = F]
    temp.traffic.Betas[!is.finite(temp.traffic.Betas)] <- NA
    temp.traffic.Betas <- zapsmall(temp.traffic.Betas)
    
    Well.Names <- dimnames(temp.traffic.Betas)[[1]]
    Cont.Names <- dimnames(temp.traffic.Betas)[[2]]
    Num.Conts <- length(Cont.Names)
    Num.Wells <- length(Well.Names)
    
    my.palette <- c("#1A9641","#A6D96A","White","#FDAE61","#D7191C")
    my.breaks <- c(-Inf, -0.0019,  -0.000949, 0.000949,0.0019 ,Inf) # 
    temp.traffic.light <- rep("grey", length(temp.traffic.Betas))
    tr <- my.palette[as.numeric(cut(as.numeric((temp.traffic.Betas)),
                                    breaks = my.breaks, include.lowest = T))]
    
    ##ND filter!
    tr[is.na(tr)] <- "grey"
    tr[as.numeric(temp.traffic.Beta.ND.Check) == 1] <- "blue"
    temp.traffic.light <- tr
    
    # my.title    <- c("Green (Strong downward trend)", "Light green (Downward trend)", "White (Stable)", "Orange (Upward trend)", "Red (Strong upward trend)")
    # temp.titles <- rep("Not available", length(temp.traffic.Betas))
    # titletmp   <- my.title[as.numeric(cut(as.numeric((temp.traffic.Betas)),
    #                                 breaks = my.breaks, include.lowest = T))]
    # titletmp[is.na(titletmp)] <- "Not available"
    # titletmp[as.numeric(temp.traffic.Beta.ND.Check) == 1] <- "Non-Detect Data"
    # temp.titles <- titletmp
  }
  
  if (display_type == "Threshold - Statistical") {
    
    temp.traffic.Beta.ND.Check <- csite$Traffic.Lights$Beta.ND.Check[Well.Names, Cont.Names, as.character(timepoint), drop = F]
    temp.traffic.Ulims <- csite$Traffic.Lights$Smooth.Upper.lims[Well.Names, Cont.Names, as.character(timepoint), drop = F]
    Well.Names <- dimnames(temp.traffic.Ulims)[[1]]
    Cont.Names <- dimnames(temp.traffic.Ulims)[[2]]
    Num.Conts <- length(Cont.Names)
    Num.Wells <- length(Well.Names)
    temp.traffic.Ulims <- as.numeric(temp.traffic.Ulims)
    
    
    Stat.Lim <- csite$ui_attr$conc_thresh[Cont.Names]
    Stat.Lim <- rep(Stat.Lim, each = length(Well.Names))

    temp.traffic.light <- rep("grey", length(Stat.Lim))
    temp.traffic.light[temp.traffic.Ulims <= Stat.Lim] <- "#1A9641"  # "green"
    temp.traffic.light[temp.traffic.Ulims > Stat.Lim]  <- "#D7191C"  # "red"
    temp.traffic.light[as.numeric(temp.traffic.Beta.ND.Check) == 1] <- "blue"
    
  }
  
  
  if (display_type == "Threshold - Absolute") {
    
    temp.traffic.Abs.Thresh.Check <- csite$Traffic.Lights$Abs.Thresh.Check[Well.Names,Cont.Names, as.character(timepoint), drop = F]
    Well.Names <- dimnames(temp.traffic.Abs.Thresh.Check)[[1]]
    Cont.Names <- dimnames(temp.traffic.Abs.Thresh.Check)[[2]]
    Num.Conts <- length(Cont.Names)
    Num.Wells <- length(Well.Names)
    temp.traffic.Abs.Thresh.Check <- as.numeric(temp.traffic.Abs.Thresh.Check)
    
    Stat.Lim <- csite$ui_attr$conc_thresh[Cont.Names]
    Stat.Lim <- rep(Stat.Lim,each = length(Well.Names))
    
    temp.traffic.light <- rep("grey", length(Stat.Lim))
    temp.traffic.light[temp.traffic.Abs.Thresh.Check <= Stat.Lim] <- "#1A9641" # green
    temp.traffic.light[temp.traffic.Abs.Thresh.Check > Stat.Lim] <- "#D7191C"  #red
    temp.traffic.light[temp.traffic.Abs.Thresh.Check == -1] <- "blue"
    
  }
  
  
  
  if (color_type != "All") {
    
    if (color_type == "White")  {my.temp.col <- "White"}
    if (color_type == "Reds")   {my.temp.col <- c("#FDAE61","#D7191C")}
    if (color_type == "Greens") {my.temp.col <- c("#1A9641","#A6D96A")}
    if (color_type == "Non-Detects") {my.temp.col <- "blue"}
    if (color_type == "Greys")  {my.temp.col <- "grey"}
    
    Traffic.col.mat <- matrix(temp.traffic.light, ncol = Num.Conts, nrow = Num.Wells)
    rownames(Traffic.col.mat) <- Well.Names
    colnames(Traffic.col.mat) <- Cont.Names
    Traffic.col.mat <- Traffic.col.mat[apply(Traffic.col.mat, 1, function(x) {any(my.temp.col %in% x)}),
                                       apply(Traffic.col.mat, 2, function(x) {any(my.temp.col %in% x)}),drop = FALSE]
    Well.Names <- rownames(Traffic.col.mat)
    Cont.Names <- colnames(Traffic.col.mat)
    Num.Wells  <- length(Well.Names)
    Num.Conts  <- length(Cont.Names)
    temp.traffic.light <- as.character(Traffic.col.mat)
  }
 
  
  #
  # Create the table starting with the header
  #   
  date_to_print <- pasteAggLimit(timepoint, csite$GWSDAT_Options$Aggby)
  html <- paste0("<h4>", display_type, ", ", date_to_print, "</h4>")
  
  # If there is no data, return immediately. 
  if (length(temp.traffic.light) == 0) {
    
    #return( div(HTML(paste0("<h4>", display_type, ", ", date_to_print, "</h4>"))) )
    html <- paste0(html, "Nothing to display.")
    return(div(style = "margin-bottom: 20px", HTML(html)))
  }
  
    
  # Build vector with title of each cell using the color as lookup
  temp.titles <- c()
  for (i in 1:length(temp.traffic.light)) {
    if (display_type == "Trend") 
      temp.titles <- c(temp.titles, col_trend[[temp.traffic.light[i]]])
    else
      temp.titles <- c(temp.titles, col_thres[[temp.traffic.light[i]]])
  }
        
  # Convert vector to matrix with contaminants in the column header.
  matt <- matrix(temp.traffic.light, ncol = Num.Conts)
  titt <- matrix(temp.titles, ncol = Num.Conts)
 
  # Add CSS table style
  ###$  User needs ability to scroll to the right to see all the analyte columns in the  Trends and Threshold table
  html <- paste0(html, "<style type='text/css' media='screen'>
     table{
       border-collapse:collapse;
       border:1px solid #000066;
       table-layout: fixed;
       width: 100%;
       height: 100%;
     }
     table td{
       border:1px solid #000066;
       padding: 5px;
       white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
     }
     table thead th:first-child,
      table tbody td:first-child {
     position: -webkit-sticky;
     position: sticky;
     left: 0;
     #z-index: 1;
     #background-color: red;
     font-weight: bold;
 }
    table thead th {
   position: -webkit-sticky;
   position: sticky;
   top: 0;
   left: 0;
   #z-index: 1;
   background-color: #f7f7f7;
   font-weight: bold;
 }
 table tbody td:first-child {
   height: 200px;
   top: auto;
 }
.header-color {
                 background-color: #f7f7f7; /* This sets the background color to red, but you can change it to any color you want */
                 #color: #FFFFFF; /* This sets the text color to white */
}
                 
 </style> ")
  
  

  html <- paste0(html, "<table>")
  html <- paste0(html, "<col width=\"70\">")

  for (cont_name in Cont.Names)
    html <- paste0(html, "<col width=\"100\">")
  
  html <- paste0(html, "<thead><tr><td>&nbsp;</td>")
  
  for (cont_name in Cont.Names)
    html <- paste0(html, "<th style=\"height: 30px; text-align:center\" >", cont_name, "&nbsp;</th>")
  
  html <- paste0(html,"</tr></thead><tbody>")
  
  for (i in nrow(matt):1) {
    html <- paste0(html,"<tr><td style=\"height: 25px; text-align:center\">", Well.Names[i],"</td>")          
    
    for (j in 1:ncol(matt)) {
      
          html <- paste0(html,"<td title=\"", titt[i,j], "\", bgcolor=\"", matt[i,j],"\"></td>")
    }
    html <- paste0(html,"</tr>")          
  }
  html <- paste0(html,"</tbody></table>")
  
  return(div(#style = "margin-bottom: 20px", 
    HTML(html))) ### Adding Scroll Bar in trends and threshold
  
  
}

plotTrendTableLegend <- function() {
 
  
  # Define CSS table style
  html <- paste0("<style type='text/css' media='screen'>
                 table{
                 border-collapse:collapse;
                 border:1px solid #000066;
                 }
                 table td{
                 border:1px solid #000066;
                 } </style> ")


  html <- paste0(html, "<div style=\"display: inline-block; margin-left:20px\">")
  html <- paste0(html, "<div style=\"margin-bottom:10px; text-align:center\"><h4> Color Key: Trend Table </h4></div>")
  html <- paste0(html, "<table>") 
  html <- paste0(html, "<col width=\"310\">")
  html <- paste0(html, "<tr><td style=\"height: 30px; text-align:center; color:white\", bgcolor=\"grey\">  Not Available </td></tr>") 
  html <- paste0(html, "<tr><td style=\"height: 30px; text-align:center; color:white\", bgcolor=\"blue\"> Non-Detect Data </td></tr>") 
  html <- paste0(html, "<tr><td style=\"height: 30px; text-align:center; color:white\", bgcolor=\"#1A9641\"> Strong Downward Trend: Half-Life < 1 Year</td></tr>") 
  html <- paste0(html, "<tr><td style=\"height: 30px; text-align:center\", bgcolor=\"#A6D96A\"> Downward Trend: 1 Year < Half-Life < 2 Years </td></tr>") 
  html <- paste0(html, "<tr><td style=\"height: 30px; text-align:center\", bgcolor=\"white\"> Stable: Doubling-Time > 2 Years, Half-Life > 2 Years </td></tr>") 
  html <- paste0(html, "<tr><td style=\"height: 30px; text-align:center\", bgcolor=\"#FDAE61\"> Upward Trend: 1 Year < Doubling-Time < 2 Years </td></tr>") 
  html <- paste0(html, "<tr><td style=\"height: 30px; text-align:center; color:white\", bgcolor=\"#D7191C\"> Strong Upward Trend: Doubling-Time < 1 Year </td></tr>") 
  html <- paste0(html, "</tbody></table></div>")
  
  html <- paste0(html, "<div style=\"display: inline-block; margin-left:20px\">")
  html <- paste0(html, "<div style=\"margin-bottom:10px; text-align:center\"><h4> Color Key: Threshold Tables </h4></div>")
  html <- paste0(html, "<table>")
  html <- paste0(html, "<col width=\"310\">")
  html <- paste0(html, "<tr><td style=\"height: 30px; text-align:center; color:white\", bgcolor=\"grey\">  Not Available </td></tr>") 
  html <- paste0(html, "<tr><td style=\"height: 30px; text-align:center; color:white\", bgcolor=\"blue\"> Non-Detect Data </td></tr>") 
  html <- paste0(html, "<tr><td style=\"height: 30px; text-align:center; color:white\", bgcolor=\"#1A9641\"> (Significantly) Below Threshold Limit</td></tr>") 
  html <- paste0(html, "<tr><td style=\"height: 30px; text-align:center; color:white\", bgcolor=\"#D7191C\"> Above (Not Significantly below) Threshold Limit</td></tr>") 
  html <- paste0(html, "</tbody></table></div>")
  
  
  return(div(HTML(html))) 
}
  
  

#
# Trend Table Animation is disabled because the format of the table
#  changed to HTML.  (MIGHT DELETE THIS AT SOME POINT)
#
# plotTrendTablePPT <- function(csite, timepoint, display_type, color_type,
#                               subset = FALSE, width = 7, height = 5){
#   
#   # Create temporary wmf file. 
#   mytemp <- tempfile(fileext = ".wmf")
#   
#   win.metafile(mytemp, width = width, height = height) 
#   plotTrendTable(csite, timepoint, display_type, color_type)
#   dev.off()
#   
#   # Put into powerpoint slide.
#   AddPlotPPV2(mytemp, width, height) 
#   
#   try(file.remove(mytemp))
#   
#   
# }

#
# Trend Table Animation is disabled because the format of the table
#  changed to HTML. (MIGHT DELETE THIS AT SOME POINT)
#
# This was the call from server():
#   makeTrendTableAnimation(csite, input$trend_or_threshold, input$color_select_tt)
#
#
#
# makeTrendTableAnimation <- function(csite, display_type, color_type, width = 7, height = 5){
#   
#   # Loop over each time point. 
#   for (i in 1:length(csite$All.Data$All_Agg_Dates)) {
# 
#     timepoint <- csite$All.Data$All_Agg_Dates[i]
#     # Create temporary wmf file. 
#     mytemp <- tempfile(fileext = ".wmf")
#     
#     win.metafile(mytemp, width = width, height = height) 
#     plotTrendTable(csite, timepoint, display_type, color_type)
#     dev.off()
#     
#     AddPlotPPV2(mytemp, width, height) 
#     
#   }
#   
# }