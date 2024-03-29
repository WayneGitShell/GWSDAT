
uiSpatialImage <- function(csite, img_frmt) {
  
  fluidRow(
    shinydashboard::tabBox(width = 3, id="NewTabBox",# status = "warning", title = "Settings",
                           
                           tabPanel("Settings",selectInput("aggregate_select_sp", label = "Aggregate by", 
                                                           choices  = csite$ui_attr$aggregate_list,
                                                           selected = csite$ui_attr$aggregate_select, 
                                                           width = "80%"),
                                    
                                    selectInput("solute_select_sp", label = "Substance", choices = csite$ui_attr$solute_names,
                                                selected = csite$ui_attr$solute_select_sp, width = '80%'),
                                    
                                    radioButtons("solute_conc_contour", label = "Solute Conc. Unit",
                                                 choices  = csite$ui_attr$conc_unit_list, 
                                                 selected = csite$ui_attr$conc_unit_selected),
                                    
                                    selectInput("imageplot_type", label = "Plot Type", choices = csite$ui_attr$contour_types,
                                                selected = csite$ui_attr$contour_selected, width = "80%"),
                                    
                                    
                                    checkboxGroupInput("imageplot_options", label = "Plot Options", 
                                                       choices = names(csite$ui_attr$spatial_options),
                                                       selected = names(which(csite$ui_attr$spatial_options == TRUE))),
                                    
                                    radioButtons("gw_flows", label = "Groundwater Flows",
                                                 choices  = csite$ui_attr$gw_options, 
                                                 selected = csite$ui_attr$gw_selected)
                                    
                           ),

                           tabPanel("Well Redundancy Analysis",

                                    checkboxInput("ImplementReducedWellSet", label = "Use reduced set of wells?", value = csite$ui_attr$ImplementReducedWellSet),

                                    #selectInput("sample_Omitted_Wells", "Select wells to be omitted from analysis", choices = csite$ui_attr$sample_loc_names,
                                    #csite$Fitted.Data[[1]]$Model.tune$best.model$Imetrics$Wellorder
                                    #csite$Fitted.Data[[csite$ui_attr$solute_select_sp]]$Model.tune$best.model$Imetrics$Wellorder
                                    selectInput("sample_Omitted_Wells", "Select wells to be omitted from analysis", 
                                            choices = if(is.null(csite$Reduced.Fitted.Data)){
                                              
                                                        if(!inherits(try(csite$Fitted.Data[[csite$ui_attr$solute_select_sp]]$Model.tune$best.model$Imetrics$Wellorder),"try-error")){
                                                            csite$Fitted.Data[[csite$ui_attr$solute_select_sp]]$Model.tune$best.model$Imetrics$Wellorder}
                                                        else{
                                                            csite$ui_attr$sample_loc_names
                                                        }
                                                  
                                                      }else{
                                                        
                                                        if(!inherits(try(csite$Fitted.Data[[csite$ui_attr$solute_select_sp]]$Model.tune$best.model$Imetrics$Wellorder),"try-error")){
                                                          csite$Reduced.Fitted.Data[[csite$ui_attr$solute_select_sp]]$Model.tune$best.model$Imetrics$Wellorder}
                                                        else{
                                                          csite$ui_attr$sample_loc_names
                                                        }
                                                  
                                                        
                                                      },
                                            selected = csite$ui_attr$sample_Omitted_Wells_selected, multiple = TRUE, selectize = TRUE),

                                    actionButton("UpdateReducedWellFittedModel", "Update Model"),
                                    actionButton("show_WellRedundancy_help", "Help"),p(""),p("Hint: Each time a well is removed or added remember to press update model to refresh the well order.")#, style="clear:left;")


                           )
                           
                           ),
    
    shinydashboard::tabBox(width = 9, id = "plume_tab_box",#status = "primary",
                           
                           tabPanel("Spatial Image", plotOutput("image_plot", height = 500),
                                    #plotOutput("image_plot", height = 500),
                                    
                                    
                                    div(style = "display: inline-block;", 
                                        selectInput("export_format_sp", label = "Image format", 
                                                    choices  = img_frmt, 
                                                    selected = img_frmt[[1]]
                                        )
                                    ),
                                    
                                    div(style = "display: inline-block; vertical-align:top; margin-top: 25px; margin-right: 10px", 
                                        downloadButton("save_spatial_plot", label = "Save Plot")
                                    ),
                                    if (existsPPT()) {
                                      div(id = "save_spatial_ppt_anim", style = "display: inline-block; vertical-align:top; margin-top: 25px;",
                                          
                                          downloadButton("generate_spatial_anim_ppt", label = "Generate PPT Animation")#, icon = icon("file-movie-o")
                                          #actionButton("generate_spatial_anim_ppt", label = "Generate PPT Animation", icon = icon("file-movie-o"))
                                      ) }
                                    
                           ),
                           tabPanel("Plume Mass, Area & Conc.",
                                    
                                    div(id = "plume_diagn_plot_div", withSpinner(plotOutput("plume_diagn_plot"))),
                                    shinyjs::hidden(div(id = "plume_diagn_msg_div", htmlOutput("plume_diagn_msg"))),
                                    
                                    div(id = "plume_save_btn_div",
                                        div(style = "display: inline-block;",
                                            selectInput("export_format_pd", label = "Image format", 
                                                        choices = img_frmt[-which(img_frmt == "tif")], 
                                                        selected = img_frmt[[1]])
                                        ),
                                        
                                        div(style = "display: inline-block; vertical-align:top; margin-top: 25px; margin-right: 10px", 
                                            downloadButton("save_plumestats_plot", label = "Save Plot")
                                        ),
                                        div(style = "display: inline-block; vertical-align:top; margin-top: 25px", 
                                            downloadButton("save_plumestats_csv", label = "Save as .CSV")
                                        )
                                    ),numericInput("plume_thresh_pd", label = "Plume Threshold (ug/l)", 
                                                   value = csite$ui_attr$plume_thresh_pd)),
                           
                           tabPanel("Estimate Boundary", value = "plume_pnl_2",
                                    withSpinner(plotOutput("plume_estimate_plot"))#, numericInput("plume_thresh_pd", label = "Plume Threshold (ug/l)", value = csite$ui_attr$plume_thresh_pd)
                           )
    ),
    # This draggable panel contains the time slider for the spatial heatmap plot.s
    absolutePanel(id = "timecontrol_sp", class = "panel panel-default", 
                  fixed = TRUE, draggable = TRUE, top = "auto", 
                  left = "auto", right = 20, bottom = 20,
                  width = 350, height = 140,  
                  
                  div(style = "margin-left: 15px; margin-top: 5px",
                      h4(textOutput("timepoint_sp_idx_label")),
                      sliderInput("timepoint_sp_idx",
                                  label="",
                                  #                                label = paste0("Time: ", pasteAggLimit(csite$ui_attr$timepoints[csite$ui_attr$timepoint_sp_idx], csite$GWSDAT_Options$Aggby)),
                                  min = 1,
                                  max = length(csite$ui_attr$timepoints),
                                  step = 1,
                                  value = csite$ui_attr$timepoint_sp_idx,
                                  animate = animationOptions(loop = TRUE, interval = 1500)
                      ) # ,
                      
                      # This worked nice for passing a vector of dates to values.
                      # However, update does not work and grid is messed up with too many values.
                      #
                      #sliderValues(
                      #  inputId = "timepoint_sp", label = "Time Point", width = "95%",
                      #  values = csite$ui_attr$timepoints, 
                      #  from = csite$ui_attr$timepoint_sp,
                      #  grid = if (length(csite$ui_attr$timepoints) < 20) {TRUE} else {FALSE},
                      #  animate = animationOptions(interval = 1500, loop = TRUE)
                  )
    ) 
  )
}
