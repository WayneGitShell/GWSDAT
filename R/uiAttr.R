

createUIAttr <- function(All.Data, GWSDAT_Options) {
  

  ui_attr <- list()

  ui_attr$site_name <- GWSDAT_Options$SiteName
  
  ui_attr$sample_loc_names    <- All.Data$sample_loc$names 
  ui_attr$sample_loc_selected <- All.Data$sample_loc$names[1]  
  ui_attr$substance_names     <- All.Data$cont_names
  ui_attr$substance_selected  <- ui_attr$substance_names[1]
  ui_attr$conc_unit_list      <- list("ng/l","ug/l","mg/l")
  ui_attr$conc_unit_selected  <- ui_attr$conc_unit_list[2]

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
  
  ui_attr$timestep_range <- c(1, length(All.Data$All.Agg.Dates))
  ui_attr$spatial_timestep_selected <- ui_attr$timestep_range[2]
  ui_attr$trend_timestep_selected   <- ui_attr$timestep_range[2]
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
  ui_attr$aggregate_list <- c("All Dates","Monthly","Quarterly")  
  ui_attr$aggregate_selected <- ui_attr$aggregate_list[2]
  ui_attr$trend_thresh_list <- c("Trend", "Threshold - Absolute", "Threshold - Statistical")
  ui_attr$trend_thresh_selected <- ui_attr$trend_thresh_list[1]
  ui_attr$trend_color_list <- c("All", "Reds", "Greens", "White", "Non-Detects", "Greys")
  ui_attr$trend_color_selected <- ui_attr$trend_color_list[1]
  
  # Define the image formats that can be saved.
  ui_attr$img_formats <- list("png", "jpg", "pdf", "ps")
  if (.Platform$OS.type == "windows") {
    ui_attr$img_formats[[length(ui_attr$img_formats) + 1]] <- "wmf"
    ui_attr$img_formats[[length(ui_attr$img_formats) + 1]] <- "ppt"
  }
  ui_attr$img_width_px       <- 800 
  ui_attr$img_height_px      <- 600  
  ui_attr$img_width_px_wide  <- 1200
  ui_attr$img_height_px_wide <- 600  
  ui_attr$img_jpg_quality    <- 90
  ui_attr$img_ppi            <- 120   
  
  
  
  
  
 
  # NOT DECIDED YET ON WHERE TO PUT THESE:
  #
  # Maybe into separate model attributes data set.
  #
  #
    # related to plume model; read by 'uiPlumeTimeSeries.R'
    ui_attr$ground_porosity <- GWSDAT_Options$DefPorosity
  
    # related to plume model; read by 'uiPlumeTimeSeries.R'
    ui_attr$plume_thresh <- rep(GWSDAT_Options$DefPlumeThresh, length(ui_attr$substance_names))
    names(ui_attr$plume_thresh) <- ui_attr$substance_names
  
    # related to trend/threshold table, not (yet) used in UI. 
    ui_attr$conc_thresh <- rep(GWSDAT_Options$DefContThresh, length(ui_attr$substance_names)) 
    names(ui_attr$conc_thresh) <- ui_attr$substance_names
  
    # Prediction interval for interpolation, not in UI. 
    ui_attr$pred_interval = "Predicted"
    
    # Used extensivly in plotSpatialImage, never changed, not in UI.
    ui_attr$lev_cut <-  c(0,5,10,25,50,75,100,200, 400, 800, 1500, 3000, 5000, 5000000)
    ui_attr$sd_lev_cut <- 100 * c(seq(0,3,by = 0.25),10000000)
    
  
  
  
  return(ui_attr)
  
  
}