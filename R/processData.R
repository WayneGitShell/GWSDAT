





processData <- function(AG.ALL, sample_loc, GWSDAT_Options, Aq_sel = NULL){

  
  well_tmp_data <- sample_loc$data
  
  
  ################ Input Date field and format #################################
  
  
  if (class(AG.ALL$SampleDate)[1] %in% c("POSIXct","POSIXt","Date")) {
  
    if (class(AG.ALL$SampleDate)[1] %in% c("POSIXct","POSIXt")) {
    
    	AG.ALL$SampleDate <- format(AG.ALL$SampleDate + 12*60*60,"%Y-%m-%d") #Watch for time zone differences here!
    	AG.ALL$SampleDate <- as.Date(AG.ALL$SampleDate,"%Y-%m-%d")
    
    }
  
  } else {
    msg = "Trouble reading Date Format, Please convert Input Excel Data to Date format."
    showModal(modalDialog(title = "Error", msg, easyClose = FALSE))
    return(NULL)
  }
  
  
  #Pick up Electron Acceptors before deleting non-aquifer wells. 
  ElecAccepts <- unique(as.character(AG.ALL[ tolower(as.character(AG.ALL$Flags)) %in% c("e-acc","notinnapl","redox"),"Constituent"]))
  
  
  ############ Well Data Sort  #################################################
  
  AG.ALL$WellName <- factor(rm_spaces(as.character(AG.ALL$WellName)))
  well_tmp_data$WellName <- factor(rm_spaces(as.character(well_tmp_data$WellName)))
  
  
  #### Aquifer Selection #######################################################
  
  # Tranform Aquifer column into characters for easier handling.
  well_tmp_data$Aquifer <- as.character(well_tmp_data$Aquifer)  
  
  
  # Replace empty "" or <NA> elements with "Blank" keyword. 
  well_tmp_data$Aquifer[well_tmp_data$Aquifer == ""] <- "Blank"
  well_tmp_data$Aquifer[is.na(well_tmp_data$Aquifer)] <- "Blank"

  
  # Extract unique Aquifer group names.
  Aq_list <- unique(well_tmp_data$Aquifer)

  # Only show the tk selection choice for Aquifer if not in headless mode.
  if (is.null(Aq_sel) && length(Aq_list) > 1) {
    
    # Returning this list will notify caller that a selection has to be made.
    class(Aq_list) = "Aq_list"
    return(Aq_list)
  }
  
  # Define the first Aquifer to be the one we display as default.	
  if (is.null(Aq_sel))
    Aq_sel <- Aq_list[[1]]
  
  
  ######## Modify well coordinate table #########################################
  
  # Extract the Wells flagged with the Aquifer 'Aq_sel'. 	
  well_tmp_data <- well_tmp_data[well_tmp_data$Aquifer == Aq_sel,]
 
  # Keep only the following columns in the well_tmp_data table.
  well_tmp_data <- well_tmp_data[,c("WellName","XCoord","YCoord")]
  
  well_tmp_data$XCoord <- as.numeric(rm_spaces(as.character(well_tmp_data$XCoord)))
  well_tmp_data$YCoord <- as.numeric(rm_spaces(as.character(well_tmp_data$YCoord)))
  well_tmp_data <- na.omit(well_tmp_data)
  well_tmp_data <- unique(well_tmp_data)
  
  
  if (any(table(well_tmp_data$WellName) > 1)) {

    msg = "Non-Unique Well Names in Well Coords Table."
    showModal(modalDialog(title = "Error", msg, easyClose = FALSE))
    return(NULL)
    
  }
  
  
  if (nrow(unique(well_tmp_data[,c("XCoord","YCoord")])) < nrow(well_tmp_data)) {

    msg = "Warning: Non-Unique Well Coordinates in Well Coords Table. Groundwater elevations for non-unique well coordinates will be substituted by their mean value."
    showNotification(msg, type = "warning", duration = 20)
    
  }
  


  # Keep concentration data that also exists in the well coordinate table.
  AG.ALL <- AG.ALL[as.character(AG.ALL$WellName) %in% as.character(well_tmp_data$WellName),]
  
  # Extract the unique well names from concentration data table.
  sample_loc_names <- sort(unique(as.character(AG.ALL$WellName)))
  
  # Lookup well coordinates for the extracted concentrations.
  well_tmp_data <- well_tmp_data[as.character(well_tmp_data$WellName) %in% sample_loc_names,]
  
  # Check if all wells exist in the well coordinate data table.
  # Delete Code?
  #        This code could be redundant because everything in sample_loc_names
  #        was previously extracted from well_tmp_data$WellName, thus, there is 
  #        no point in checking for missing well coordinates. Concentrations with
  #        corresponding wells that do not appear in the well coord. table are 
  #        simply ignored (although a statistics could be made, how many 
  #        concentration measurements were ignored).
  # 
  # non_existing_coords <- setdiff(sample_loc_names, as.character(well_tmp_data$WellName))
  # if (length(non_existing_coords) != 0) {
  # 
  #     msg = paste("Missing Well Coordinates for: ", paste(non_existing_coords, collapse = ", "))
  #     msg = paste(msg, "Do you wish to continue?", sep = "")
  #     showModal(modalDialog(title = "Warning", msg, easyClose = TRUE,
  #                           footer = modalButton("Ok")))
  #     
  #     # Extract only concentrations with      
  #     AG.ALL <- AG.ALL[as.character(AG.ALL$WellName) %in% sample_loc_names,]
  #     AG.ALL$WellName <- factor(as.character(AG.ALL$WellName))
  # }
  
   

  
  ################ Flags Handling ##############################################
  if (!all(is.na(AG.ALL$Flags))) {  
  
  	AG.ALL <- AG.ALL[tolower(as.character(AG.ALL$Flags)) != "omit",]
  
  
  } else {
  
  	AG.ALL$Flags <- rep("",nrow(AG.ALL))
  
  }
  
  
  AG.ALL$Constituent <- factor(rm_spaces(as.character(AG.ALL$Constituent)))
  
  
  ############# Case Sensitivity of Constituents ###############################
  
  if (length(unique(as.character(AG.ALL$Constituent))) != length(unique(toupper(as.character(AG.ALL$Constituent))))) {

    msg = "Warning: Constituent types have different letter cases (e.g. 'MTBE v mtbe'). GWSDAT will modify all constituent types to upper case."
    showNotification(msg, type = "warning", duration = 20)
    
    AG.ALL$Constituent <- factor(toupper(as.character(AG.ALL$Constituent)))
  }
  
  
  
  AG.ALL$Result <- factor(rm_spaces(as.character(AG.ALL$Result)))
  AG.ALL$Units <- factor(rm_spaces(as.character(AG.ALL$Units)))
  Cont.Data <- AG.ALL[tolower(as.character(AG.ALL$Constituent))!="gw",]
  
  
  ############### Contaminant Data Type Processing #############################
  ContTypeData = "Default"
  
  if(nrow(Cont.Data) == 0) { #GW only type data. 
  
  	ContTypeData <- "NoConcData"
  	Cont.Data <- rbind(Cont.Data,data.frame(WellName = as.character(sample_loc_names[1]),Constituent=" ",#Constituent="Dummy", 
  	SampleDate <- max(AG.ALL$SampleDate), Result = NA, Units="ug/l", Flags=""))
  	
  }
  
  if(nrow(Cont.Data[tolower(as.character(Cont.Data$Constituent))!="napl",])==0){ #NAPL Only type data
  
  	ContTypeData <-"NoConcData"
  	Cont.Data<-rbind(Cont.Data,data.frame(WellName=as.character(sample_loc_names[1]),Constituent=" ",#Constituent="Dummy", 
  	SampleDate=max(AG.ALL$SampleDate),Result=NA,Units="ug/l",Flags=""))
  }
  
  
  Cont.Data$Constituent<-factor(as.character(Cont.Data$Constituent))
  All.Conts<-unique(as.character(Cont.Data$Constituent))
  
  
  ########################## Units Checking ####################################
  if (any(!tolower(as.character(Cont.Data$Units[tolower(as.character(Cont.Data$Constituent)) != "napl"])) %in% c("ug/l","mg/l","ng/l"))) {

    msg = "Solute data must be one of 'ng/l', 'ug/l' or 'mg/l'. Please correct and re-run GWSDAT analysis."
    showModal(modalDialog(title = "Units Error", msg, easyClose = FALSE))
    return(NULL)
  }
  
  
  
  
  
  Cont.Data$ND <- rep(FALSE,nrow(Cont.Data))
  Cont.Data$ND[grep("<",as.character(Cont.Data$Result))]<-TRUE
  Cont.Data$Result.Corr.ND <- rep(NA,nrow(Cont.Data))
  Cont.Data$Result.Corr.ND[!Cont.Data$ND] <- as.numeric(as.character(Cont.Data$Result[!Cont.Data$ND]))
  
  
  ############# Checking for 0 conc concentration data #########################
  if (any(Cont.Data$Result.Corr.ND[tolower(as.character(Cont.Data$Constituent))!="napl"]==0,na.rm=TRUE)){

    msg = "Zero solute concentration data detected in input data - this is not permissible. Please correct and re-run GWSDAT analysis."
    showModal(modalDialog(title = "Error", msg, easyClose = FALSE))
    return(NULL)
    
  }
  
  
  
  
  temp.hold <- sub(".*<", "", as.character(Cont.Data$Result[Cont.Data$ND]))
  
  if (any(grep("nd",temp.hold,ignore.case = T))) {
    msg <- "Warning: '<ND' detected. Non-Detect limits must be specified. Omitting unspecified Non Detect Data."
    showNotification(msg, type = "warning", duration = 20)    
  }
  
  Cont.Data$Result.Corr.ND[Cont.Data$ND] <- as.numeric(temp.hold)
  
  
  
  ############# Solute Unit Handling converts all ng/l and mg/l to ug/l ########
  Cont.Data$Units <- tolower(as.character(Cont.Data$Units))
  
  if (any(grep("mg",Cont.Data$Units))) {
  
    Cont.Data$Units[grep("mg",Cont.Data$Units)] <- "mg/l"
    Cont.Data$Result.Corr.ND[Cont.Data$Units == "mg/l"] <- 1000*Cont.Data$Result.Corr.ND[Cont.Data$Units=="mg/l"]
    Cont.Data$Result <- as.character(Cont.Data$Result)
    Cont.Data$Result[Cont.Data$Units == "mg/l" & !Cont.Data$ND]<-as.character(Cont.Data$Result.Corr.ND[Cont.Data$Units=="mg/l" & !Cont.Data$ND])
    Cont.Data$Result[Cont.Data$Units == "mg/l" & Cont.Data$ND]<-paste("ND<",as.character(Cont.Data$Result.Corr.ND[Cont.Data$Units=="mg/l" & Cont.Data$ND]),sep="")
    Cont.Data$Result <- factor(as.character(Cont.Data$Result))
    Cont.Data$Units[Cont.Data$Units == "mg/l"] <- "ug/l"
    
  }
  
  if (any(grep("ng",Cont.Data$Units))) {
  
    Cont.Data$Units[grep("ng",Cont.Data$Units)]<-"ng/l"
    Cont.Data$Result.Corr.ND[Cont.Data$Units=="ng/l"]<-0.001*Cont.Data$Result.Corr.ND[Cont.Data$Units=="ng/l"]
    Cont.Data$Result<-as.character(Cont.Data$Result)
    Cont.Data$Result[Cont.Data$Units=="ng/l" & !Cont.Data$ND]<-as.character(Cont.Data$Result.Corr.ND[Cont.Data$Units=="ng/l" & !Cont.Data$ND])
    Cont.Data$Result[Cont.Data$Units=="ng/l" & Cont.Data$ND]<-paste("ND<",as.character(Cont.Data$Result.Corr.ND[Cont.Data$Units=="ng/l" & Cont.Data$ND]),sep="")
    Cont.Data$Result<-factor(as.character(Cont.Data$Result))
    Cont.Data$Units[Cont.Data$Units == "ng/l"] <- "ug/l"
    
  }
  
  
  Cont.Data$Units <- factor(as.character(Cont.Data$Units))
  
  
  
  ############## NAPL Handling #################################################
  # Substituted with max observed value on a cont by cont basis 
  
  if ("napl" %in% tolower(as.character(Cont.Data$Constituent))) {
  
    NAPL.Thickness.Data <- Cont.Data[tolower(as.character(Cont.Data$Constituent)) == "napl",]
  
  
    NAPL.Units <- unique(tolower(as.character(NAPL.Thickness.Data$Units)))
    
    if (length(NAPL.Units) > 1) {
      
      msg = "Multiple units detected for NAPL thickness in input dataset. Please ensure same thickness units are used throughout."
      showModal(modalDialog(title = "Error", msg, easyClose = FALSE))
      return(NULL)
    }
    
    if (length(NAPL.Units) > 0) {
      
      if (!NAPL.Units %in% c("level","mm","cm","metres","inches","feet")) {
        
        msg = "NAPL thickness units must be one of 'level', 'mm', 'cm', 'metres', 'inches' or 'feet'.\n\nPlease correct and re-run GWSDAT analysis."
        showModal(modalDialog(title = "Error", msg))
        return(NULL)
      }
    }
    
    #-----------------------------------------------------------------#
    
    
    NAPL.Thickness.Data <- try(NAPL.Thickness.Data[order(NAPL.Thickness.Data$SampleDate),])
    NAPL.Thickness.Data[,c("XCoord","YCoord")] <- well_tmp_data[match(as.character(NAPL.Thickness.Data$WellName),as.character(well_tmp_data$WellName)),c("XCoord","YCoord")]

    
    
    
    
    #msg <- "Do you wish to substitute NAPL values with maximum observed solute concentrations? \nNote: NAPL measurements for electron acceptor, Redox or 'NotInNapl' flagged constituents will be ignored."
    #msg <- paste(msg, "\n(Currently only Yes choice is possible, changing to Yes/No soon.", sep = "")
    subst_napl_vals <- "yes"
    msg <- "NAPL values are substituted with maximum observed solute concentrations (Yes/No choice will soon be supported). \nNote: NAPL measurements for electron acceptor, Redox or 'NotInNapl' flagged constituents will be ignored."
    #showModal(modalDialog(title = "Notification", msg, easyClose = FALSE, footer = modalButton("Ok")))
    showNotification(msg, type = "warning", duration = 20)  
    
    
    if (ContTypeData == "NoConcData" || subst_napl_vals == "yes") {
      
      
      
      All.Conts.No.NAPL <- All.Conts[tolower(All.Conts)!="napl"]
      All.Conts.No.NAPL <- setdiff(All.Conts.No.NAPL,ElecAccepts) #omit e-acc constituent from NAPL set
      
      
      ############################# NAPL and dissolved conflict resolution ##########################################
      NAPLWellandDates <- unique(Cont.Data[tolower(Cont.Data$Constituent)=="napl",c("WellName","SampleDate","Constituent"),drop=FALSE])[,1:2]
      NonNAPLWellandDates <- unique(Cont.Data[tolower(Cont.Data$Constituent)!="napl" & !is.na(Cont.Data$Result.Corr.ND),c("WellName","SampleDate","Constituent"),drop=FALSE])[,1:2]
      NAPLConflictDateandWells <- NonNAPLWellandDates[which(apply(NonNAPLWellandDates,1,paste,collapse="") %in% apply(NAPLWellandDates,1,paste,collapse="")),]
      NAPLConflictDateandWells <- unique(NAPLConflictDateandWells); 
      
      if (nrow(NAPLConflictDateandWells) > 0) {
        
        if (all(Cont.Data[apply(Cont.Data[,c("WellName","SampleDate")],1,paste,collapse = "") %in% apply(NAPLConflictDateandWells,1,paste,collapse="") 
                          & tolower(Cont.Data$Constituent) == "napl",]$Result.Corr.ND == 0)) {

          myans <- "yes"

        } else {

          myans <- "yes"

          # "NAPL Data Conflict"        
          # msg <- "Concentration data reported in presence of NAPL. Do you wish to use concentration data (Yes) or substitue these NAPL values with maximum observed solute concentrations (No)?\nNote: NAPL measurements for electron acceptor, Redox or 'NotInNapl' flagged constituents will be ignored."
          msg <- "Concentration data reported in presence of NAPL. Use it? (Yes/No choice will soon be supported)"
          showNotification(msg, type = "warning", duration = 20)  
        }
        
        if (myans == "no") {
          
  	      for (i in 1:nrow(NAPLConflictDateandWells)){
            Cont.Data <- Cont.Data[-which(as.character(Cont.Data$WellName) == as.character(NAPLConflictDateandWells$WellName)[i] & 
                                            Cont.Data$SampleDate == NAPLConflictDateandWells$SampleDate[i] & tolower(as.character(Cont.Data$Constituent)) != "napl"),]
          }
          
  	    } else {
                    
          for (i in 1:nrow(NAPLConflictDateandWells)){
            Cont.Data <- Cont.Data[-which(as.character(Cont.Data$WellName) == as.character(NAPLConflictDateandWells$WellName)[i] & 
                                            Cont.Data$SampleDate == NAPLConflictDateandWells$SampleDate[i] & tolower(as.character(Cont.Data$Constituent)) == "napl"),]
          }
    	  }
      }
      
      #-------------------------------------------------------------------------------------------------------------#
      
      NAPL.Data <- Cont.Data[tolower(as.character(Cont.Data$Constituent))=="napl",]
      No.NAPL.Data <- Cont.Data[tolower(as.character(Cont.Data$Constituent))!="napl",]
      No.NAPL.Data$Constituent <- factor(as.character(No.NAPL.Data$Constituent))
      No.NAPL.Data <- No.NAPL.Data[,c("WellName","Constituent","SampleDate","Result","Units","ND","Result.Corr.ND")]
      
      
      
      New.NAPL.Data <- data.frame(WellName = 
                                  rep(NAPL.Data$WellName, length(All.Conts.No.NAPL)),
                                  Constituent = rep(All.Conts.No.NAPL,each=nrow(NAPL.Data)),
  	                              SampleDate = rep(NAPL.Data$SampleDate,length(All.Conts.No.NAPL))
                        )
      
      New.NAPL.Data$Result=rep("NAPL",nrow(New.NAPL.Data))
      New.NAPL.Data$Units=rep(NAPL.Units,nrow(New.NAPL.Data)); 
      New.NAPL.Data$ND = rep(FALSE,nrow(New.NAPL.Data))
      New.NAPL.Data$Result.Corr.ND <- tapply(No.NAPL.Data$Result.Corr.ND,No.NAPL.Data$Constituent,max,na.rm=T)[as.character(New.NAPL.Data$Const)]
      
      
      Cont.Data <- rbind(No.NAPL.Data,New.NAPL.Data)
      All.Conts <- unique(as.character(Cont.Data$Constituent))
      
      
    } else {
  
      All.Conts.No.NAPL <- All.Conts[tolower(All.Conts) != "napl"]
      NAPL.Data <- Cont.Data[tolower(as.character(Cont.Data$Constituent)) == "napl",]
      No.NAPL.Data <- Cont.Data[tolower(as.character(Cont.Data$Constituent)) != "napl",]
      No.NAPL.Data$Constituent <- factor(as.character(No.NAPL.Data$Constituent))
      
      Cont.Data <- No.NAPL.Data
      All.Conts <- unique(as.character(Cont.Data$Constituent))
      
    }
  }
  
  
  ##################################### ND correction Handling ##############################################
  
  if (GWSDAT_Options$NDMethod == "Half of ND Value"){Cont.Data$Result.Corr.ND[Cont.Data$ND]<-0.5*Cont.Data$Result.Corr.ND[Cont.Data$ND]}
  Cont.Data[,c("XCoord","YCoord")] <- well_tmp_data[match(as.character(Cont.Data$WellName),as.character(well_tmp_data$WellName)),c("XCoord","YCoord")]
  
  
  
  ####################### Groundwater Data ###############################################################
  
  GW.Data <- AG.ALL[tolower(as.character(AG.ALL$Constituent)) == "gw",]
  GW.Units <- unique(tolower(as.character(GW.Data$Units)))

  
  if (length(GW.Units) > 1) {

    # "Units Error"
    msg <- "Multiple units detected for GroundWater elevation in input dataset. \nPlease ensure same elevation units are used throughout."
    showModal(modalDialog(title = "Units Error", msg))
    return(NULL)
  }
  
  
  if (length(GW.Units) > 0) {
    if (!GW.Units %in% c("level","mm","cm","metres","inches","feet")) {
      
      msg <- "GroundWater elevation units must be one of 'level', 'mm', 'cm', 'metres', 'inches' or 'feet'.\n\nPlease correct and re-run GWSDAT analysis."
      showModal(modalDialog(title = "Units Error", msg))
      return(NULL)
    }
  }
  
  GW.Data$Result <- as.numeric(as.character(GW.Data$Result))
  GW.Data <- GW.Data[!is.na(GW.Data$Result),]
  GW.Data[,c("XCoord","YCoord")] <- well_tmp_data[match(as.character(GW.Data$WellName),as.character(well_tmp_data$WellName)),c("XCoord","YCoord")]

  
  
  ####################### Aggregate Cont, GW and NAPL Data ######################################################
  All.Dates <- sort(unique(c(GW.Data$SampleDate,AG.ALL$SampleDate)))
  
  if (exists("NAPL.Thickness.Data")) { 
    All.Dates <- sort(unique(c(All.Dates,NAPL.Thickness.Data$SampleDate)))
  }
  
  
  agg_data <- aggregateData(GWSDAT_Options, All.Dates, GW.Data, Cont.Data, well_tmp_data, 
                                    NAPL.Thickness.Data = if (exists("NAPL.Thickness.Data")) { NAPL.Thickness.Data } else {NULL} )



  sample_loc$data  <- well_tmp_data
  sample_loc$names <- sample_loc_names
  sample_loc$area  <- areapl(as.matrix(well_tmp_data[chull(well_tmp_data[,c("XCoord","YCoord")]),c("XCoord","YCoord")]))

  
  # Read the shape files.
  ShapeFiles <- initShapeFiles(GWSDAT_Options)
  
  
  
  #
  # This list is way to big!! Make it slimmer and more structured.
  #
  
  All.Data <- list(GW.Data = GW.Data,
                 Agg_GW_Data = agg_data$Agg_GW_Data,
                 NAPL.Thickness.Data = agg_data$NAPL.Thickness.Data,
                 Cont.Data = agg_data$Cont.Data,
                 All.Conts = All.Conts,
                 All.Dates = All.Dates,
                 All.Agg.Dates = agg_data$All.Agg.Dates,
                 Aq.sel = Aq_sel,
                 Aq_list = Aq_list,
                 GW.Units = GW.Units,
                 NAPL.Units = if (exists("NAPL.Units")) { NAPL.Units } else {NULL}, 
                 ElecAccepts = ElecAccepts,
                 solute_data = AG.ALL,
                 ShapeFiles = ShapeFiles,
                 sample_loc = sample_loc
                 )
  
  return(All.Data)
}
