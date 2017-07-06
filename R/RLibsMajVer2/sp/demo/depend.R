packages_to_check <- function(dep, which = c("Depends", "Imports", "LinkingTo", "Suggests"), recursive = FALSE){

download.file("http://cran.R-project.org/web/packages/packages.rds", "packages.rds", mode="wb")
    x <- readRDS("packages.rds")
    x <- x[!duplicated(x[,1]),]
    packages <- x[,1]
    rdeps <- tools:::.package_dependencies(packages = dep, x,
                        which = which,
                        recursive = recursive, reverse = TRUE)
    paste(apply(x[x[,1] %in% rdeps[[1]], 1:2], 1, paste, collapse="_"), ".tar.gz", sep="")
}

result <-  packages_to_check("sp")

#RCheck = function(x, URL = "http://ftp5.gwdg.de/pub/misc/cran/src/contrib/") {
RCheck = function(x, URL = "http://cran.r-project.org/src/contrib/") {
	if (!file.exists(x))
		download.file(paste(URL, x, sep=""), x)
	cmd = paste("R CMD check ", x, " > ", x, ".log", sep = "")
	print(cmd)
	ret = system(cmd)
	print(ret)
	ret
}

result = result[-grep("surveill", result)]
result
sel = TRUE
succ = unlist(lapply(result[sel], function(x) RCheck(x)))
x = which(succ != 0)
result[x]
bla = lapply(result[x], function(x) system(paste("tail ",x,".log", sep="")))

#result <-  packages_to_check("sp", recursive=TRUE) 
