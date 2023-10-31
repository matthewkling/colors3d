
#' Map values to a 3D legend in RGB colorspace.
#'
#' This function returns a color value for each row of the 3-column dataset
#' supplied, by transforming the input data and using it as RGB values.
#'
#' @param data Matrix or data frame with 3 numeric columns.
#' @param trans Either "none" (defaut, histogram is rescaled) or "rank"
#'   (histogram is flattened).
#' @param order Integer from 1 to 6, each denoting a unique permutation of
#'   variables-to-color band mapping. Under the default value of 1, the 
#'   three variables in `data` are respectively mapped onto the R, G, and B 
#'   bands of colorspace.
#' @param inversion Integer from 1 to 8, each denoting a unique combination of
#'   variables to reverse before mapping. Under the default value of 1, all 
#'   three variables are mapped with positive values at the high end of the
#'   color band. Together with the `order` parameter, this allows all possible
#'   48 unique mappings of a given set of variables onto 3D colorspace.
#' @param opacity Not currently used.
#' @returns Character vector of colors.
#' @examples 
#' d <- expand.grid(x = 1:49, y = 1:49)
#' d$z <- cos(sqrt((d$x-25)^2 + (d$y-25)^2))
#' plot(d[, 1:2], col = colors3d(d), pch = 15, cex = 2)
#' 
#' plot(d[, 1:2], col = colors3d(d, order = 2, inversion = 2), pch = 15, cex = 2)
#' @export
colors3d <- function(data, trans = "none", order = 1, inversion = 1, opacity = NULL){
      rescale <- function(x) (x - min(x)) / (max(x) - min(x))
      trans <- match.arg(trans, c("none", "rank"))
      data <- apply(data, 2, rescale)
      if(trans == "rank") data <- apply(data, 2, function(x) rank(x) / length(x))
      data <- data[, combinat::permn(1:3)[[order]]]
      invert <- (1:3)[as.logical(expand.grid(c(FALSE, TRUE), c(FALSE, TRUE), c(FALSE, TRUE))[inversion, ])]
      data[, invert] <- 1 - data[, invert]
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
#' @param xtrans,ytrans Transformation to apply to x and y variables before
#'   applying a linear color mapping: either "none" (default), "log", or "rank".
#' @returns Character vector of colors.
#' @examples 
#' plot(iris, 
#'      col = colors2d(iris[, c("Sepal.Length", "Sepal.Width")]), 
#'      pch = 19, cex = 2)
#'      
#' plot(iris, 
#'      col = colors2d(iris[, c("Sepal.Length", "Sepal.Width")],
#'                     colors = c("limegreen", "gold", "black", "dodgerblue"),
#'                     xtrans = "rank", ytrans = "rank"), 
#'      pch = 19, cex = 2)
#' @export
colors2d <- function(data, 
                     colors = c("yellow", "green", "blue", "magenta"),
                     xtrans = c("none", "log", "rank"), 
                     ytrans = c("none", "log", "rank")){
      
      xtrans <- match.arg(xtrans, c("none", "log", "rank"))
      ytrans <- match.arg(ytrans, c("none", "log", "rank"))
      
      colors <- col2rgb(colors)/255

      if(xtrans == "rank") data[, 1] <- rank(data[, 1]) / nrow(data)
      if(ytrans == "rank") data[, 2] <- rank(data[, 2]) / nrow(data)
      if(xtrans == "log") data[, 1] <- log(data[, 1])
      if(ytrans == "log") data[, 2] <- log(data[, 2])

      data <- apply(data, 2, scales::rescale)
      interpolate <- function(i){
            x <- i[1]
            y <- i[2]
            x1 <- colors[, 2] * x + colors[, 3] * (1-x)
            x2 <- colors[, 1] * x + colors[, 4] * (1-x)
            x2 * y + x1 * (1-y)
      }
      
      cols <- rep(NA, nrow(data))
      cols[which(is.finite(rowMeans(data)))] <- rgb(t(apply(na.omit(data), 1, interpolate)))
      cols
}



#' Internal function converting x-y do distance-angle.
#'
#' @param data Matrix or data frame with 2 numeric columns representing x and y.
#' @param xyratio Single number indicating unit ratio in x vs y direction.
#' @param xorigin,yorigin Numbers indicating center of polarization.
#' @returns 2-column matrix of distances and angles.
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
#' @param origin Coordindates of color wheel center.
#' @param xyratio Scalar representing how to map the elliptical color wheel in
#'   the data space (the default 1 a circular mapping that weights the two
#'   dimensions equally).
#' @param kernel Optional function describing the shape of radial color
#'   gradients (default is a linear mapping corresponding to a triangular
#'   kernel); this function should take a vector of distances to the center as
#'   its sole input and return a positive number.
#' @returns Character vector of colors.
#' @examples
#' plot(iris, 
#'       col = colorwheel2d(iris[, c("Sepal.Length", "Sepal.Width")]), 
#'       pch = 19, cex = 2)
#'       
#' plot(iris, 
#'       col = colorwheel2d(
#'                   iris[, c("Sepal.Length", "Sepal.Width")],
#'                   origin = c(5.5, 2.5),
#'                   kernel = function(x) x ^ .5), 
#'       pch = 19, cex = 2)
#' @export
colorwheel2d <- function(data, 
                         colors = c("black", "yellow", "green", "cyan", "blue", "magenta", "red"),
                         origin = NULL, xyratio = NULL, kernel = NULL){
      result <- rep(NA, nrow(data))
      a <- which(!is.na(apply(data, 1, sum)))
      data <- na.omit(data)

      if(is.null(origin)) origin <- c(sum(range(data[,1], na.rm=T))/2,
                                      sum(range(data[,2], na.rm=T))/2)

      xrange <- range(data[,1])
      yrange <- range(data[,2])
      xmag <- plyr::round_any(max(abs(xrange)), (xrange[2]-xrange[1])/20, ceiling)
      ymag <- plyr::round_any(max(abs(yrange)), (yrange[2]-yrange[1])/20, ceiling)
      if(is.null(xyratio)) xyratio <- xmag / ymag

      pdata <- as.data.frame(polarize(data, xyratio=xyratio,
                                      xorigin=origin[1], yorigin=origin[2]))
      names(pdata) <- c("distance", "angle")

      if(!is.null(kernel)) pdata$distance <- kernel(pdata$distance)
      pdata$angle <- pdata$angle / 360

      n <- length(colors)-1
      pdata$cl <- ceiling(pdata$angle * n) + 1
      pdata$fl <- floor(pdata$angle * n) + 1
      col <- matrix(NA, length(pdata$angle), 3)
      mx <- max(pdata$distance)

      colors <- col2rgb(colors)
      pal <- colors[,c(2:ncol(colors),2)] / 255
      center <- colors[,1] / 255
      center <- as.vector(center)

      getcol <- function(x){
            interp <- x[2] * n - x[4] + 1
            col_angle <- (as.vector(pal[,x[3]]) * interp +
                                as.vector(pal[,x[4]]) * (1-interp))
            col_angle * x[1] / mx + center * (1 - x[1]/mx)
      }

      col <- t(apply(pdata, 1, getcol))
      col[pdata$distance==0,] <- center
      result[a] <- rgb(col)
      return(result)
}


#' Palettes of dissimilar colors in RGB space.
#'
#' Many standard palette generators use only a slice of color space, which can
#' cause a lack of differentiability in palettes used to visualize categorical
#' factors with many levels. This function attempts to overcome this by
#' generating colors using nearest-neighbor distance maximization in 3D RGB
#' space.
#'
#' @param n Number of colors (integer).
#' @param res Number of distinct values in each RGB dimension (integer).
#' @param maxreps Max number of optimization iterations (integer).
#' @param radius Neighborhood size for potential moves, analagous to heating.
#' @param avoid_white Logical, default is TRUE.
#' @param seed Integer used to seed randomization during search; leave as NULL to generate different results each time, or set a value to generate reproducible results.
#' @returns Character vector of colors.
#' @examples
#' plot(runif(20), runif(20), 
#'      col = distant_colors(20), 
#'      pch = 16, cex = 3)
#' @export
distant_colors <- function(n, res=20, maxreps=1000, radius=10, avoid_white=TRUE, seed=NULL){

      if(!is.null(seed)) set.seed(seed)
      
      if(avoid_white) n <- n + 1

      f <- expand.grid(r=1:res,
                       g=1:res,
                       b=1:res)

      si <- f[sample(nrow(f), n),]

      for(i in 1:maxreps){

            si0 <- si

            for(j in 1:n){

                  # active location
                  sij<- si[j,]

                  # potential moves
                  h <- f[f$r > sij$r-radius & f$r < sij$r+radius &
                                  f$g > sij$g-radius & f$g < sij$g+radius &
                                  f$b > sij$b-radius & f$b < sij$b+radius,]

                  # reference locations
                  sin <- si[-j,]

                  # find the move with max dist to nearest active location
                  dst <- FNN::get.knnx(sin, h, k=1)$nn.dist
                  move <- h[which.max(dst)[1],]

                  # execute optimal move
                  si[j,] <- move
            }

            # check for convergence
            if(all.equal(as.matrix(si0), as.matrix(si)) == TRUE) break
      }

      if(i == maxreps) warning("Algorithm failed to converge, consider increasing maxreps parameter.")

      if(avoid_white) si <- si[setdiff(1:nrow(si), which.max(si$r + si$g + si$b)),]

      rgb(si, maxColorValue=res)
}
