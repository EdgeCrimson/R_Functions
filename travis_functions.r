quality_threshold <- function(quality_per_unit=2.25,descriptive_output=FALSE){
  # Computes the minimum quality percent level of an item to be worth picking up based on object shape in grid. Defaults to 2.25.\
  # You may specify if you would like a printed description or just the 4-tuple as output. Defaults to 4-tuple output. Intended for\
  # use in the Steam game, Path of Exile.
  classes <- c(3,4,6,8)
  quality <- 0
  restrict <- c(-1,-1,-1,-1)
  cont_check <- rep(TRUE,length(restrict))
  proceed <- TRUE
  while (proceed == TRUE){
    if (quality >= 20){
      proceed <- FALSE
    }
    margin <- quality/classes
    for (shape in c(1:length(margin))){
      if (cont_check[shape] == TRUE){
        if (margin[shape] >= quality_per_unit){
          restrict[shape] <- quality
          cont_check[shape] <- FALSE
        }
      }
    }
    quality <- quality+1
  }
  for (result in c(1:length(restrict))){
    if (restrict[result] == -1){
      restrict[result] <- "NA"
    }
  }
  if (descriptive_output == FALSE){
    Shape <- c("1x3","1x4","2x2","2x3","2x4")
    Quality_Percent <- c(restrict[1:2],restrict[2],restrict[3:4])
    restrict_frame <- data.frame(Shape,Quality_Percent)
    return(restrict_frame)
  }
  else{
    item_x3 <- paste("The minimum quality percentage for items one unit wide by three units tall {1x3} you should pick up are {",
                     restrict[1],"%} quality.",sep="")
    item_x4_1 <- paste("The minimum quality percentage for items one unit wide by four units tall {1x4} you should pick up are {",
                       restrict[2],"%} quality.",sep="")
    item_x4_2 <- paste("The minimum quality percentage for items two units wide by two units tall {2x2} you should pick up are {",
                       restrict[2],"%} quality.",sep="")
    item_x6 <- paste("The minimum quality percentage for items two units wide by three units tall {2x3} you should pick up are {",
                     restrict[3],"%} quality.",sep="")
    item_x8 <- paste("The minimum quality percentage for items two units wide by four units tall {2x4} you should pick up are {",
                     restrict[4],"%} quality.",sep="")
    return(writeLines(paste(item_x3,item_x4_1,item_x4_2,item_x6,item_x8,sep="\n")))
  }
}



load_GIS_lib <- function(){
  # Loads the most common plotting packages for geomapping in R
  x <- c("rgeos","tmap","ggmap","rgdal","maptools","dplyr","tidyr")
  lapply(x,library,character.only=TRUE)
}



append_filepath <- function(addition){
  # Allows a user to set their working directory to a child path by passing a character containing the remaining filepath as input
  setwd(paste(gsub("/","\\\\",getwd()),addition,sep="\\"))
}



selection_by_location <-function(base_layer,selection_layer){
  ###*NOTE: CURRENTLY HAS ISSUES WITH OUTPUT*###
  # Takes a base layer object and selects all objects in a selection layer based on the base layer's ID field. Both input objects
  # must be of one of the three vector SpatialXDataFrame objects (i.e. all spatial data frame types except Grid). If no ID field
  #  exists in the base layer object, one is assigned.
  if (all(lapply(list(base_layer,selection_layer),class) %in% c("SpatialPointsDataFrame","SpatialLinesDataFrame",
                                                                 "SpatialPolygonsDataFrame"))==FALSE){
    return("base_layer input and selection_layer input must be of the 'SpatialXDataFrame' classes.")
  }
  if (require(rgdal) != TRUE){
    library(rgdal)
  }
  if (is.null(base_layer$ID)){
    for (i in c(1:length(county))){
      base_layer$ID <- i
    }
  }
  input <- 0
  if (input %in% base_layer$ID){
    input_logic <- vector(length = length(selection_layer))
    for (i in (1:length(selection_layer))){input_logic[i] <- gIntersects(base_layer[input+1,],selection_layer[i,])}
    # plot(selection_layer) ###Activate this line to graph the selection with respect to full context. If you add this line back in, 
    # be sure to add "add=TRUE" to next line
    plot(selection_layer[input_logic,],col="green")
    if (class(base_layer)=="SpatialPolygonsDataFrame"){
      baselines <- as(base_layer,"SpatialLinesDataFrame")
      plot(baselines[input+1,],col="blue",add=TRUE)}
    else {plot(base_layer[input+1],col="green",add=TRUE)}
  }
}



agg_shared_polypts <- function(input){
  # Collects all points from a SpatialPolygonDataFrame that are not unique to a single polygon
  ne_lin <- as(input,"SpatialLinesDataFrame")
  ne_pts <- as.data.frame(as(ne_lin,"SpatialPointsDataFrame"))
  ne_xcords <- ne_pts[,8]
  ne_ycords <- ne_pts[,9]
  ne_xuniq <- unique(ne_xcords)
  ne_yuniq <- unique(ne_ycords)
  ne_list <- ne_pts[,5]

  # This function collects a list
  long_count <- 0
  linRep_list <- list(mode="numeric",length=0)
  xRep_list <- list(mode="numeric",length=0)
  yRep_list <- list(mode="numeric",length=0)
  for (i in (1:length(unique(ne_xcords)))){
    xtru <- ne_xuniq[i] == ne_xcords
    xtru_val <- ne_xcords[xtru]
    if (length(xtru_val)>length(unique(xtru_val))){
      ytru_val <- ne_ycords[xtru]
      ytru <- ne_yuniq[i] == ne_ycords
      if (length(ytru_val)>length(unique(ytru_val))){
        if (length(ne_list[ne_xuniq[i] == ne_xcords]) == length(unique(ne_list[ne_xuniq[i] == ne_xcords]))){
          long_count <- long_count+1
          linRep_list[long_count] <- ne_list[i]
          xytru <- xtru==TRUE & ytru==TRUE
          xRep_list[long_count] <- unique(ne_xcords[xytru])
          yRep_list[long_count] <- unique(ne_ycords[xytru])
        }
      }
    }
  }

  # Make point list into SpatialPointsDataFrame:
  ne_replist <- cbind(xRep_list,yRep_list)
  ne_replist1 <- as.data.frame(ne_replist,col.names=c("x_cord","y_cord"),row.names = c(1:72))
  ne_replist2 <- data.matrix(ne_replist1, rownames.force = NA)
  ne_spt <- SpatialPoints(ne_replist2)
  ne_rep_pts <- SpatialPointsDataFrame(ne_spt,ne_replist1)
  ne_rep_pts$line <- linRep_list
  return(ne_rep_pts)
}


