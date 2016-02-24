


#' Map values to a 3D legend in RGB colorspace.
#'
#' This function returns a color value for each row of the 3-column dataset
#' supplied, by transforming the input data and using it as RGB values.
#'
#' @param data Matrix or data frame with 3 numeric columns.
#' @param trans Either "fit" (defaut, histogram is rescaled) or "ecdf"
#'   (histogram is flattened).
#' @param order Integer from 1 to 6, each denoting a unique permutation of
#'   variables-to-color band mapping.
#' @param inversion Integer from 1 to 8, each denoting a unique combination of
#'   variables to invert.
#'   @param opacity Not currently used.
#' @return Vector of color values.

colors3d <- function(data, trans="fit", order=1, inversion=1, opacity=NULL){
      require(scales)
      require(combinat)
      data <- apply(data, 2, rescale)
      if(trans=="ecdf") data <- apply(data, 2, function(x)ecdf(x)(x))
      data <- data[,permn(1:3)[[order]]]
      invert <- (1:3)[as.logical(expand.grid(c(F,T), c(F,T), c(F,T))[inversion,])]
      data[,invert] <- 1- data[,invert]
      cols <- rep(NA, nrow(data))
      cols[which(is.finite(rowMeans(data)))] <- rgb(na.omit(data))
      cols
}



#' Map values to a 2D legend interpolated from 4 corner colors.
#'
#' This function returns a color value for each row of the 2-column dataset
#' supplied, based on a 2D color palette interpolated from 4 corner colors.
#'
#' @param data Matrix or data frame with 2 numeric columns; they will map to x
#'   and y.
#' @param colors Vector of 4 corner colors to interpolate, clockwise from upper
#'   right.
#' @return Vector of color values.

colors2d <- function(data, colors=c("yellow", "green", "blue", "magenta")){
      colors <- col2rgb(colors)/255
      data <- apply(data, 2, scales::rescale)
      interpolate <- function(i){
            x <- i[1]
            y <- i[2]
            x1 <- colors[,2] * x + colors[,3] * (1-x)
            x2 <- colors[,1] * x + colors[,4] * (1-x)
            x2 * y + x1 * (1-y)
      }
      rgb(t(apply(data, 1, interpolate)))
}





#' Internal function converting x-y do distance-angle.
#'
#' @param data Matrix or data frame with 2 numeric columns representing x and y.
#' @param xyratio Single number indicating unit ratio in x vs y direction.
#' @param xorigin, yorigin Numbers indicating center of polarization.
#' @return 2-column matrix of distances and angles.

polarize <- function(data, xyratio, xorigin=0, yorigin=0){
      distance <- sqrt((data[,1]-xorigin)^2 + ((data[,2]-yorigin) * xyratio)^2)
      angle <- acos((data[,1]-xorigin) / distance) * 180 / pi
      angle[data[,2]<yorigin] <- 360 - angle[data[,2]<yorigin]
      return(cbind(distance, angle))
}


#' Map values to a 2D colorwheel legend.
#'
#' This function returns a color value for each row of the 2-column dataset
#' supplied, based on a 2D color palette defined by a center color and a series
#' of peripheral colors.
#'
#' @param data Matrix or data frame with 2 numeric columns; they will map to x
#'   and y.
#' @param colors Vector of colors to interpolate: center followed by periphery
#'   counterclockwise from 3 o'clock.
#' @return Vector of color values.

colorwheel2d <- function(data, colors=c("black", "yellow", "green", "blue", "magenta"),
                         origin=NULL, xyratio=NULL){
      if(is.null(origin)) origin <- c(sum(range(data[,1], na.rm=T))/2,
                                      sum(range(data[,2], na.rm=T))/2)

      xrange <- range(data[,1], na.rm=T)
      yrange <- range(data[,2], na.rm=T)
      xmag <- plyr::round_any(max(abs(xrange)), (xrange[2]-xrange[1])/20, ceiling)
      ymag <- plyr::round_any(max(abs(yrange)), (yrange[2]-yrange[1])/20, ceiling)
      if(is.null(xyratio)) xyratio <- xmag / ymag

      pdata <- polarize(data, xyratio=xyratio, xorigin=origin[1], yorigin=origin[2])
      n <- length(colors)-1
      angle <- pdata[,2] / 360
      cl <- ceiling(angle * n) + 1
      fl <- floor(angle * n) + 1
      col <- matrix(NA, length(angle), 3)
      mx <- max(pdata[,1])

      colors <- col2rgb(colors)
      pal <- colors[,c(2:ncol(colors),2)] / 255
      center <- colors[,1] / 255
      center <- as.vector(center)

      for(i in 1:length(angle)){
            interp <- angle[i] * n - fl[i] + 1
            col_angle <- (as.vector(pal[,cl[i]]) * interp +
                                as.vector(pal[,fl[i]]) * (1-interp))
            col[i,] <- col_angle * pdata[,1][i] / mx + center * (1 - pdata[,1][i]/mx)
      }
      d <- rep(NA, nrow(data))
      d[which(!is.na(rowMeans(col)))] <- rgb(na.omit(col))
      return(d)
}

