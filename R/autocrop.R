#' autocrop
#'
#' Automatically crop away white space form an image.  Can read any image
#' supported by \code{raster}.
#'
#' @param x a file or a raster object
#' @param border
#' @param outfile
#' @import raster
#' @examples
#' x<-rep(0,49)
#' x[18]<-1
#' x[24:25]<-1
#' x[31:32]<-1
#' x<-stack(raster(matrix(x,ncol=7,byrow = T)))
#' extent(x) <- extent(c(1,7,1,7))
#' x_ac<-autocrop(x,border=0)
#' x_ac<-autocrop(system.file("extdata/test.tif",package="autocrop"))
autocrop <- function(x, border = 2, outfile = NULL ){

  if(class(x)=="RasterStack"){
    xf <- x
  } else if(class(x)=="character"){
    xf <- raster::stack(x)
  }
  for(i in slot(xf,"layers")){
    #browser()
    array <- raster::as.matrix(i)
    if(!exists("minx")){minx<-ncol(xf)+1}
    if(!exists("maxx")){maxx<-0}
    if(!exists("miny")){miny<-nrow(xf)+1}
    if(!exists("maxy")){maxy<-0}

    #Get Y's
    for(row in 1:nrow(xf)){
      if(sd(array[row,])!=0){
        if(nrow(xf) + 1 - row > maxy){maxy <- nrow(xf) + 1 - row}
        break()
      }
    }

    for(row in nrow(xf):1){
      if(sd(array[row,])!=0){
        if(nrow(xf) + 1 - row < miny){miny <- nrow(xf) + 1 - row}
        break()
      }
    }

    #Get x's

    for(col in 1:ncol(xf)){
      if(sd(array[,col])!=0){
        if(col<minx){minx <- col}
        break()
      }
    }

    for(col in ncol(xf):1){
      if(sd(array[,col])!=0){
        if(col>maxx){maxx <- col}
        break()
      }
    }
  }
  #Offsets
  minx<-minx-1-border
  maxx<-maxx+1+border
  miny<-miny-1-border
  maxy<-maxy+1+border
  return(raster::crop(xf,raster::extent(matrix(c(minx,miny,maxx,maxy),ncol =2))))
}
