#' Create NMDS ordination scree plot to determine number of dimensions
#'
#' @param x Ordination data frame containing only the indicator values (no plot identifiers)
#'
#' @return Plots stress versus number of dimensions in ordination solution
#' @export
#'

NMDS_scree<-function(x) { #x is the name of the data frame
  plot(rep(1,2),
       replicate(2,vegan::metaMDS(x,k=1)$stress),
       xlim=c(1,9),
       ylim=c(0,1),
       xlab="# of Dimensions",
       ylab="Stress",
       main="NMDS stress plot")
  for (i in 1:4) {
    points(rep(i+1,2),
           replicate(2,vegan::metaMDS(x,distance="bray",k=i+1)$stress))
  }}
