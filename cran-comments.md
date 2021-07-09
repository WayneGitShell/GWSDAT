On release, The warning  with the " Missing or unexported object: 'officer::ph_with_img'" is due to backwards compatibility handing in the package code - see below snippet. 


#' @import officer
addPlotPPT <- function(imgfile, ppt_pres, width, height) {
  
  ppt_pres <- officer::add_slide(ppt_pres, layout = "Title and Content", master = "Office Theme")
  
  
  if(utils::packageVersion("officer")>'0.3.7'){
    
    ## ph_with_img is defunct and replaced with ph_with
    #ppt_pres <- officer::ph_with(x=ppt_pres,external_img(src = imgfile,  width = width / 90 * 0.7, height = height / 90 * 0.7),location = ph_location_left(),use_loc_size = FALSE)
    ppt_pres <- officer::ph_with(x=ppt_pres,external_img(src = imgfile),location = ph_location_type(type = "body"))
    
  }else{
    
    ppt_pres <- officer::ph_with_img(ppt_pres, src = imgfile, height = height / 90 * 0.7, width = width / 90 * 0.7 )
    
  }
  
  return(ppt_pres)

}

