"GWSDAT.readShapeFile"<-function (fn, proj4string = CRS(as.character(NA)), verbose = FALSE, 
    repair = FALSE, IDvar = NULL, force_ring = FALSE, delete_null_obj = TRUE,
    retrieve_ABS_null = FALSE){

    shinfo <- getinfo.shape(fn)
    if (verbose){print(shinfo)}
    type <- shinfo[[2]]
    types <- c("Point", NA, "PolyLine", NA, "Polygon", NA, NA, 
        "MultiPoint", NA, NA, "PointZ", NA, "PolyLineZ", NA, 
        "PolygonZ", NA, NA, "MultiPointZ", NA, NA, "PointM", 
        NA, "PolyLineM", NA, "PolygonM", NA, NA, "MultiPointM", 
        NA, NA, "MultiPatch")
    typeSh <- types[type]
    


    if (typeSh == "Point" || typeSh == "PointZ" || typeSh == 
        "MultiPoint") {
        res <- readShapePoints(fn = fn, proj4string = proj4string, 
            verbose = verbose, repair = repair)
    }
    else if (typeSh == "PolyLine" || typeSh == "PolyLineZ") {
        
	if(!is.null(formals(readShapeLines)$delete_null_obj)){
	res <- readShapeLines(fn = fn, proj4string = proj4string,verbose = verbose, repair = repair,delete_null_obj=delete_null_obj)
	}else{
	res <- GWSDAT.readShapeLines(fn = fn, proj4string = proj4string,verbose = verbose, repair = repair)
	}
    }
    else if (typeSh == "Polygon" || typeSh == "PolygonZ") {
        res <- readShapePoly(fn = fn, IDvar = IDvar, proj4string = proj4string, 
            verbose = verbose, repair = repair, force_ring = force_ring, 
            delete_null_obj = delete_null_obj, retrieve_ABS_null = retrieve_ABS_null)
    }
    else stop("File type cannot be read")
    res
}

GWSDAT.readShapeLines<-function (fn, proj4string = CRS(as.character(NA)), verbose = FALSE, repair = FALSE){

    suppressWarnings(Map <- read.shape(filen = fn, verbose = verbose,repair = repair))
    suppressWarnings(GWSDAT.shp2LinesDF(Map, proj4string = proj4string))
}



GWSDAT.shapes2LinesList<-function (shape, ID) 
{
    nParts <- attr(shape, "nParts")
    Pstart <- shape$Pstart
    nVerts <- nrow(shape$verts)
    from <- integer(nParts)
    to <- integer(nParts)
    from[1] <- 1
    for (j in 1:nParts) {
        if (j == nParts) 
            to[j] <- nVerts
        else {
            to[j] <- Pstart[j + 1]
            from[j + 1] <- to[j] + 1
        }
    }
    res <- vector(mode = "list", length = nParts)
    for (i in 1:nParts) {
        res[[i]] <- Line(coords = shape$verts[from[i]:to[i], 
            , drop = FALSE])
    }
    Lines <- Lines(res, ID = ID)
    Lines
}



GWSDAT.shp2LinesDF<-function (shp, proj4string = CRS(as.character(NA)), IDs, delete_null_obj = TRUE) 
{
    if (class(shp) != "Map") 
        stop("shp not a Map object")
    shp.type <- attr(shp$Shapes, "shp.type")
    if (!shp.type %in% c("arc", "poly")) 
        stop("not an arc or poly Map object")
    nullParts <- sapply(shp$Shapes, function(x) x$nParts) == 
        0
    if (delete_null_obj) {
        nullParts <- which(nullParts)
        if (length(nullParts) > 0L) {
            for (i in length(nullParts):1) shp$Shapes[[nullParts[i]]] <- NULL
            attr(shp$Shapes, "nshps") <- attr(shp$Shapes, "nshps") - 
                length(nullParts)
            shp$att.data <- shp$att.data[-nullParts, ]
            warning(paste("Null objects with the following", 
                "indices deleted:", paste(nullParts, collapse = ", ")))
        }
    }
    else {
        if (any(nullParts)) 
            stop(paste("NULL geometry found:", paste(which(nullParts), 
                collapse = ", "), "\n               consider using delete_null_obj=TRUE"))
    }
    df <- shp$att.data
    shapes <- shp$Shapes
    n <- length(shapes)
    LinesList <- vector(mode = "list", length = n)
    if (missing(IDs)) 
        IDs <- as.character(sapply(shapes, function(x) x$shpID))
    if (length(IDs) != n) 
        stop("IDs length differs from number of lines")
    row.names(df) <- IDs
    for (i in 1:n) {
        LinesList[[i]] <- GWSDAT.shapes2LinesList(shapes[[i]], ID = IDs[i])
    }
    SL <- SpatialLines(LinesList, proj4string = proj4string)
    res <- SpatialLinesDataFrame(SL, data = df)
    res
}




GWSDAT.PlotShapeFile<-function(shpfile,add=TRUE,col="lightblue"){


if(length(grep("poly",tolower(class(shpfile))))>0){

	plot(shpfile,border=col,add=add,col = "transparent")

}
else if(length(grep("point",tolower(class(shpfile))))>0)
{

	plot(shpfile,col=col,add=add)

}
else{

	plot(shpfile,col=col,add=add)

}


}
