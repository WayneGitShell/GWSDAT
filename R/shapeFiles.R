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
    tbl_out <- rhandsontable::rhandsontable(MAT, useTypes = FALSE, stretchH = "all") %>%
      hot_col("Name", readOnly = TRUE) %>%
      hot_col("Size", readOnly = TRUE)
  } else {
    # Render Empty table: To leave to blank gap when "Remove All Files" is pressed.
    tbl_out <- rhandsontable::rhandsontable(data.frame(Name = character(), Size = numeric()), 
                                 useTypes = FALSE, stretchH = "all") %>%
      hot_col("Name", readOnly = TRUE) %>%
      hot_col("Size", readOnly = TRUE)
    
  }
  
  return(tbl_out)
}