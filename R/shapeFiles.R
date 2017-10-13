#' @import sf 
readShapeFiles_sf <- function(ShapeFileNames) {
  
  
  shdat <- list()
  
  if (!is.null(ShapeFileNames)) {
    for (filein in ShapeFileNames) {
      
      tryCatch({
        
        dat <- sf::st_read(filein)
        shdat[[length(shdat) + 1]] <- dat
        
      }, error = function(e) {
        showNotification(paste0("Failed to load shape file ", filein, ": ", e$msg), type = "error")
      })
    }
  }
  
  if (length(shdat) == 0)
    return(NULL)
  
  return(shdat)
  
}


createShapeFileList <- function(shfiles) {

  if (!is.null(shfiles)) {
    
    MAT <- matrix(nrow = 0, ncol = 2)
    
    # Add shape file details to matrix.
    for (i in 1:length(shfiles$file_details)) {
      MAT <- rbind(MAT, matrix(c(shfiles$file_details[[i]]$name,
                                 shfiles$file_details[[i]]$size), ncol = 2))
    }
    
    MAT <- as.data.frame(MAT)
    colnames(MAT) <- c("Name", "Size")
    
    # Render the table.
    tbl_out <- rhandsontable::rhandsontable(MAT, useTypes = FALSE, rowHeaders = NULL, stretchH = "all") %>%
      hot_col("Name", readOnly = TRUE) %>%
      hot_col("Size", readOnly = TRUE)
  } else {
    # Render Empty table: To leave to blank gap when "Remove All Files" is pressed.
    tbl_out <- rhandsontable::rhandsontable(data.frame(Name = character(), Size = numeric()), 
                                 useTypes = FALSE, rowHeaders = NULL, stretchH = "all") %>%
      hot_col("Name", readOnly = TRUE) %>%
      hot_col("Size", readOnly = TRUE)
    
  }
  
  return(tbl_out)
}


addShapeFiles <- function(input_spf, spf) {
  
  # If no shape file has been added yet (spf is NULL), create new list.
  if (is.null(spf))
    spf <- list(shp_files = c(), file_details = list(input_spf))
  else {
    # Append to already existing shape file collection.
    lst_end <- length(spf$file_details)
    spf$file_details[[lst_end + 1]] <- input_spf
  }
  
  SHP_FILE_DETECTED <- FALSE
  
  # Rename all the temporary files to their original name, so that the sf package
  # reader can read all files an ones.
  for (i in 1:length(input_spf$name)) {
    
    # Split path and replace file name with real file name.
    dp <- strsplit(input_spf$datapath[i], "/")[[1]]
    dp[length(dp)] <- input_spf$name[i]
    
    # Put it back together, now with the new file name in the path.
    new_dp <- paste(dp, collapse = "/")
    
    file.rename(input_spf$datapath[i], new_dp)
    
    # Detect *.shp file and save to shp_files vector. It will be used by the 
    # sf package to read out all other files.
    fa <- strsplit(input_spf$name[i], "\\.")[[1]]
    if (fa[length(fa)] == "shp") {
      SHP_FILE_DETECTED <- TRUE
      spf$shp_files <- c(spf$shp_files, new_dp)
    }
  }
  
  if (!SHP_FILE_DETECTED)
    showNotification("No .shp file detected. Need at least one .shp file to read remaining shape files.",
                     type = "warning", duration = 10)
  
  return(spf)
}


# # If no shape file has been added yet, create new list.
# if (is.null(spf))
#   import_tables$shape_files <<- list(shp_files = c(), file_details = list(input$shape_files_csv))
# else {
#   # Append to already existing shape file collection.
#   lst_end <- length(import_tables$shape_files$file_details)
#   import_tables$shape_files$file_details[[lst_end + 1]] <<- input$shape_files_csv
# }
# 
# SHP_FILE_DETECTED <- FALSE
# 
# # Rename all the temporary files to their original name, so that the sf package
# # reader can read all files an ones.
# for (i in 1:length(input$shape_files_csv$name)) {
#   
#   # Split path and replace file name with real file name.
#   dp <- strsplit(input$shape_files_csv$datapath[i], "/")[[1]]
#   dp[length(dp)] <- input$shape_files_csv$name[i]
#   
#   # Put it back together, now with the new file name in the path.
#   new_dp <- paste(dp, collapse = "/")
#   
#   file.rename(input$shape_files_csv$datapath[i], new_dp)
#   
#   # Detect *.shp file and save to shp_files vector. It will be used by the 
#   # sf package to read out all other files.
#   fa <- strsplit(input$shape_files_csv$name[i], "\\.")[[1]]
#   if (fa[length(fa)] == "shp") {
#     SHP_FILE_DETECTED <- TRUE
#     import_tables$shape_files$shp_files <<- c(import_tables$shape_files$shp_files, new_dp)
#   }
# }
# 
# if (!SHP_FILE_DETECTED)
#   showNotification("No .shp file detected. Need at least one .shp file to read remaining shape files.",
#                    type = "warning", duration = 10)