
ppt <- NULL
myPres <- NULL
mySlides <- NULL

## Not loading on library(GWSDAT), only on install. Also not loading 
#  in a start script such as launchApp() to my suprise. Where is this 
#  effective?
#
#path_to_extdata <- system.file("extdata", package = "GWSDAT")
#cat("* adding ", path_to_extdata, " as resource\n")
#addResourcePath("extdata", path_to_extdata)


## Can't change from outside, as a package the binding is locked
#  (using .GlobalEnv instead)
#sessfile <- ""
#APP_RUN_MODE <- "MultiData"