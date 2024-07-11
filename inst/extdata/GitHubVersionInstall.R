#.libPaths(c(file.path('C:\\Users\\Wayne.W.Jones\\GitHub\\GWSDAT_v3.12\\','library',paste0(as.numeric(R.version$major),'.0')),.libPaths()))
if("C:/Program Files/R/R-3.6.3/library" %in% .libPaths()){.libPaths("C:/Program Files/R/R-3.6.3/library")}
.libPaths(c(file.path('C:\\Users\\Wayne.W.Jones\\OneDrive - Shell\\WayneDocs\\AssetRefresh\\GitHub\\GWSDAT_v3.21\\','library',paste0(as.numeric(R.version$major),'.0')),.libPaths()))

.libPaths()
devtools::install_github("WayneGitShell/GWSDAT",ref="v3.21",upgrade="never")
#devtools::install_github("WayneGitShell/GWSDAT",upgrade="never")
#if("C:/Program Files/R/R-3.6.3/library" %in% .libPaths()){.libPaths("C:/Program Files/R/R-3.6.3/library")}

# remotes::install_github("WayneGitShell/GWSDAT",ref="WellRedundancyAnalysis")
# install_version("geometry", version = "0.3.6", repos = "http://cran.us.r-project.org")
# install_version("officer", version = "0.3.7", repos = "http://cran.us.r-project.org")

# install.packages("magic") 
