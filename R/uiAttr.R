saveUIAttr <- function(csite, input) {
  
  csite$ui_attr$timepoint_sp_idx <- input$timepoint_sp_idx
  csite$ui_attr$timepoint_tt_idx <- input$timepoint_tt_idx

  csite$ui_attr$solute_select_ts  <- input$solute_select_ts
  csite$ui_attr$solute_select_sp  <- input$solute_select_sp
  csite$ui_attr$solute_select_stp <- input$solute_select_stp
  csite$ui_attr$solute_select_pd  <- input$solute_select_pd
  csite$ui_attr$solute_select_wr  <- input$solute_select_wr

  csite$ui_attr$sample_loc_select_ts  <- input$sample_loc_select_ts
  csite$ui_attr$sample_loc_select_wr  <- input$sample_loc_select_wr
  csite$ui_attr$sample_loc_select_stp <- input$sample_loc_select_stp

  # Note that input$aggregate_select_sp == input$aggregate_select_tt and only 
  # 'ui_attr$aggregate_selec' will set the select control.
  csite$ui_attr$aggregate_select <- input$aggregate_select_sp
  csite$ui_attr$color_select_tt  <- input$color_select_tt
  csite$ui_attr$trend_thresh_selected <- input$trend_or_threshold
  csite$ui_attr$show_thresh_ts <- input$check_threshold
  csite$ui_attr$logscale_wr <- input$logscale_wr
  csite$ui_attr$logscale_stp <- input$logscale_stp
  csite$ui_attr$plume_thresh_pd <- input$plume_thresh_pd
  csite$ui_attr$ground_porosity <- input$ground_porosity
  csite$ui_attr$ground_porosity_pd <- input$ground_porosity_pd
  csite$ui_attr$solute_conc_stp <- input$solute_conc_stp
  
  return(csite)
}


createUIAttr <- function(All.Data, GWSDAT_Options) {
  
  
  ui_attr <- list()

  ui_attr$site_name <- GWSDAT_Options$SiteName
  
  ui_attr$sample_loc_names      <- All.Data$sample_loc$names    
  ui_attr$sample_loc_select_ts  <- ui_attr$sample_loc_names[1] 
  ui_attr$sample_loc_select_wr  <- ui_attr$sample_loc_names
  ui_attr$sample_loc_select_stp <- ui_attr$sample_loc_names
  
  ui_attr$solute_names        <- All.Data$cont_names
  ui_attr$solute_select_ts    <- ui_attr$solute_names[1]
  ui_attr$solute_select_sp    <- ui_attr$solute_names[1]
  ui_attr$solute_select_stp   <- ui_attr$solute_names[1]
  ui_attr$solute_select_pd    <- ui_attr$solute_names[1]
  ui_attr$solute_select_wr    <- ui_attr$solute_names
  
  ui_attr$conc_unit_list      <- list("ng/l","ug/l","mg/l")
  ui_attr$conc_unit_selected  <- ui_attr$conc_unit_list[2]
  ui_attr$solute_conc_stp     <- ui_attr$conc_unit_list[2]
  
  ts_options = NULL  
  ts_options["Conc. Trend Smoother"] <- TRUE
  ts_options["Conc. Linear Trend Fit"] <- FALSE
  ts_options["Show Legend"] <- FALSE
  ts_options["Scale to Conc. Data"] <- FALSE
  ts_options["Log Conc. Scale"] <- TRUE
  ts_options["Overlay GW levels"] <- FALSE
  if (!is.null(All.Data$NAPL.Thickness.Data)) ts_options["Overlay NAPL Thickness"] <- FALSE  
  ui_attr$ts_options <- ts_options
  ui_attr$napl_present <- FALSE
  
  ui_attr$timepoints <- format.Date(All.Data$All_Agg_Dates, "%d-%m-%Y")
  ui_attr$timepoint_sp <- ui_attr$timepoints[length(ui_attr$timepoints)]
  ui_attr$timepoint_sp_idx <- length(ui_attr$timepoints)
  
  ui_attr$timepoint_tt <- ui_attr$timepoints[length(ui_attr$timepoints)]
  ui_attr$timepoint_tt_idx <- length(ui_attr$timepoints)
    
  ui_attr$contour_types <- c("Conc-Terrain", "Conc-Topo","Conc-GreyScale","Conc-Terrain-Circles","Conc-Topo-Circles","Conc-GreyScale-Circles", if (!is.null(All.Data$NAPL.Thickness.Data)) {"NAPL-Circles"})  
  ui_attr$contour_selected <- ui_attr$contour_types[1]
  
  spatial_options <-  NULL
  spatial_options["Show Well Labels"] <- TRUE
  spatial_options["Scale colours to Data"] <- FALSE
  spatial_options["Show Conc. Values"] <- TRUE
  spatial_options["Show GW Contour"] <- FALSE
  if (!is.null(GWSDAT_Options$ShapeFileNames)) spatial_options["Overlay ShapeFiles"] <- TRUE
  spatial_options["Plume Diagnostics"] <- FALSE
  ui_attr$spatial_options <- spatial_options
  
  ui_attr$gw_options  <- c("None", "Same Length", "Weighted Length")
  ui_attr$gw_selected <- ui_attr$gw_options[3]
  ui_attr$aggregate_list <- c("Day", "Month", "Quarter", "Year") # disabled (error in plotSpatialImage(): , "Year")  
  ui_attr$aggregate_select <- GWSDAT_Options$Aggby
  ui_attr$trend_thresh_list <- c("Trend", "Threshold - Absolute", "Threshold - Statistical")
  ui_attr$trend_thresh_selected <- ui_attr$trend_thresh_list[1]
  ui_attr$trend_color_list <- c("All", "Reds", "Greens", "White", "Non-Detects", "Greys")
  ui_attr$color_select_tt  <- ui_attr$trend_color_list[1]
  ui_attr$show_thresh_ts <- FALSE
  ui_attr$logscale_wr    <- "Yes"
  ui_attr$logscale_stp    <- "Yes"
  
  # Define image save options.
  ui_attr$img_width_px       <- 800 
  ui_attr$img_height_px      <- 600  
  ui_attr$img_width_px_wide  <- 1100
  ui_attr$img_height_px_wide <- 500  
  ui_attr$img_jpg_quality    <- 90
  ui_attr$img_ppi            <- 90   
  

  # Related to plume model; read by 'uiPlumeTimeSeries.R'.
  ui_attr$ground_porosity <- GWSDAT_Options$DefPorosity
  ui_attr$ground_porosity_pd <- ui_attr$ground_porosity * 100
  
  # Related to plume model; read by 'uiPlumeTimeSeries.R'.
  ui_attr$plume_thresh <- rep(GWSDAT_Options$DefPlumeThresh, length(ui_attr$solute_names))
  names(ui_attr$plume_thresh) <- ui_attr$solute_names
  ui_attr$plume_thresh_pd <- ui_attr$plume_thresh[1]
    
  # Related to trend/threshold table, not (yet) used in UI. 
  ui_attr$conc_thresh <- rep(GWSDAT_Options$DefContThresh, length(ui_attr$solute_names)) 
  names(ui_attr$conc_thresh) <- ui_attr$solute_names

  # Prediction interval for interpolation, not in UI. 
  ui_attr$pred_interval = "Predicted"
  
  # Used extensivly in plotSpatialImage, never changed, not in UI.
  ui_attr$lev_cut <-  c(0,5,10,25,50,75,100,200, 400, 800, 1500, 3000, 5000, 5000000)
  ui_attr$sd_lev_cut <- 100 * c(seq(0,3,by = 0.25),10000000)
  
  
  return(ui_attr)  
}
