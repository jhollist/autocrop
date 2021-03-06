% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autocrop.R
\name{autocrop}
\alias{autocrop}
\title{autocrop}
\usage{
autocrop(x, border = 2, outfile = NULL, format = NULL,
  width = NULL, units = "in", res = 150, ...)
}
\arguments{
\item{x}{a file or a raster object}

\item{border}{border of whitespace, in pixels, to leave around image.}

\item{outfile}{output file to save.  Defaults to NULL}

\item{format}{file format type.  Guessed by outfile extension.}

\item{width}{width is specified (defaults to current device width).  Height
is determined by aspect ratio of cropped image.}

\item{units}{units defualts to inches, but may be any units supported by
the output format.}

\item{res}{output dpi.  Defaults to 150.}

\item{...}{parameters to pass to specfified output device (e.g. tiff()).}
}
\description{
Automatically crop away white space form an image.  Can read any image
supported by \code{raster}.
}
\examples{
x<-rep(0,49)
x[18]<-1
x[24:25]<-1
x[31:32]<-1
x<-raster::stack(raster::raster(matrix(x,ncol=7,byrow = TRUE)))
raster::extent(x) <- raster::extent(c(1,7,1,7))
x_ac<-autocrop(x,border=0)
x_ac<-autocrop(system.file("extdata/big_test.tiff",package="autocrop"), outfile = "test_devs.tiff", width = 6.1, res = 450, compression = "lzw")

}
