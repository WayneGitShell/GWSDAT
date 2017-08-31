
check_pkg_availability <- function() {

  pkg_list <- c("shiny", 
              "shinyjs", 
              "shinydashboard", 
              "shinycssloaders", 
              "sm", 
              "zoo", 
              "splancs", 
              "Kendall", 
              "deldir", 
              "maptools", 
              "geometry", 
              "Matrix", 
              "readxl", 
              "MASS", 
              "lattice", 
              "sp", 
              "rhandsontable")

  pkg_ind = c()

  for (i in 1:length(pkg_list)) {
  
    pkg_ind[i] <- require(pkg_list[i], character.only = TRUE)
  }

  return(pkg_list[!pkg_ind])

}
