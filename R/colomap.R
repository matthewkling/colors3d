


#' Get colors for a 3D dataset.
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
