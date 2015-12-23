#' autocrop
#'
#' Automatically crop away white space form an image.  Can read any image
#' supported by \code{raster}.
#'
#' @param x a file or a raster object
#' @param border border of whitespace, in pixels, to leave around image.
#' @param outfile output file to save.  Defaults to NULL
#' @param format file format type.  Guessed by outfile extension.
#' @param width width is specified (defaults to current device width).  Height
#'              is determined by aspect ratio of cropped image.
#' @param units units defualts to inches, but may be any units supported by
#'              the output format.
#' @param res output dpi.  Defaults to 150.
#' @param ... parameters to pass to specfified output device (e.g. tiff()).
#' @import raster
#' @export
#' @examples
#' x<-rep(0,49)
#' x[18]<-1
#' x[24:25]<-1
#' x[31:32]<-1
#' x<-raster::stack(raster::raster(matrix(x,ncol=7,byrow = TRUE)))
#' raster::extent(x) <- raster::extent(c(1,7,1,7))
#' x_ac<-autocrop(x,border=0)
#' x_ac<-autocrop(system.file("extdata/big_test.tiff",package="autocrop"),
#' outfile = "test_devs.tiff", width = 6.1, res = 450, compression = "lzw")
#'
autocrop <- function(x, border = 2, outfile = NULL, format = NULL, width = NULL,
                     units = "in", res = 150, ...){

  if(class(x)=="RasterStack"){
    xf <- x
  } else if(class(x)=="character"){
    xf <- raster::stack(x)
  }
  for(i in slot(xf,"layers")){
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
  crop_img <- raster::stack(raster::crop(xf,raster::extent(matrix(c(minx,miny,
                                                                    maxx,maxy),
                                                                  ncol =2))))
  crop_img <- array_it(crop_img)
  browser()
  #Save file
  if(!is.null(outfile)){
    if(is.null(format)){
      format <- get_format(outfile)
    }

    save_img(crop_img,outfile,format,width,units,res,...)
  }
  return(crop_img)
}

#' Get image format
#' @param infile
#' @keywords internal
array_it<-function(img){
  d<-NULL
  for(i in slot(img,"layers")){
    d <- c(d,raster::as.matrix(i))
  }
  d/255
  return(array(d,c(nrow(img),ncol(img),length(img@layers))))
}

#' Get image format
#' @param infile
#' @keywords internal
get_format<-function(infile){
  ext <- strsplit(infile, "\\.")[[1]]
  ext <- ext[length(ext)]
  if(ext == "tif"){ext<-"tiff"}
  if(ext == "jpg"){ext<-"jpeg"}
  return(ext)
}

#' Save image
#' @param cropped
#' @param outfile
#' @param format
#' @param width
#' @param units
#' @param res
#' @param ...
#' @keywords internal
save_img<-function(cropped,outfile,format,width,units,res,...){
  if(is.null(width)){
    width <- dev.size()[1]
  }
  height <- width/(ncol(cropped)/nrow(cropped))
  if(format == "tiff"){
    tiff(outfile,width,height,units,res=res,...)
    raster::plotRGB(cropped,maxpixels=nrow(cropped)*ncol(cropped),interpolate=TRUE)
    dev.off()
  }
  if(format == "jpeg"){
    jpeg(outfile,width,height,units,res=res,...)
    raster::plotRGB(cropped,maxpixels=nrow(cropped)*ncol(cropped))
    dev.off()
  }
  if(format == "bmp"){
    bmp(outfile,width,height,units,res=res,...)
    raster::plotRGB(cropped,maxpixels=nrow(cropped)*ncol(cropped))
    dev.off()
  }
  if(format == "png"){
    png(outfile,width,height,units,res,...)
    raster::plotRGB(cropped,maxpixels=nrow(cropped)*ncol(cropped))
    dev.off()
  }
}
