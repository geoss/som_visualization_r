##########################################
##PLOT HEXAGONAL U-MATRIX 
##from "kohnonen" library output
##
##
##BY SETH E. SPIELMAN, UNIVERSITY OF COLORADO
##
##NEEDS SOME LOVE.  
##RUDIMENTRARY BUT FUNCTIONAL
##
##BORROWS CODE FROM
##http://nbremer.blogspot.nl/2013/11/how-to-create-hexagonal-heatmap-in-r.html
################################################

library(RColorBrewer) #to use brewer.pal
library(fields) #to use designer.colors
library(network)
library(deldir)

plotUmat <- function(som_obj, type="Equal Interval"){
  if (som_obj$grid$topo != "hexagonal"){
    stop("function assumes hexgonal SOM")
  }
  
  #CALCULATE U-MATRIX
  
  #Delaunay Triangulation to form network of neurons 
  d <- deldir(x=som_obj$grid$pts[,1], y=aSom$grid$pts[,2])
  
  #Build network
  n <- network(x=unique(d$delsgs[,5:6]), directed=FALSE, matrix.type="edgelist")
  
  #calculate u-matrix: the average eudlidean distance between each vertex and its neighbors
  neigh.dists <- NA
  for(vert in network.vertex.names(n)){
    neighs <- get.neighborhood(x=n, v=vert)
    neigh.dists[vert] <- (sum(dist(aSom$codes[c(vert, neighs),][,1]))/length(neighs))
  }
  
  
  #Function to create the polygon for each hexagon
  Hexagon <- function (x, y, unitcell = 1, col = "grey", border=NA) {
    polygon(c(x, x, x + unitcell/2, x + unitcell, x + unitcell, 
              x + unitcell/2), c(y + unitcell * 0.125, y + unitcell * 
                                   0.875, y + unitcell * 1.125, y + unitcell * 0.875, 
                                 y + unitcell * 0.125, y - unitcell * 0.125), 
            col = col, border=border)
  }
  
  
  plot(0, 0, type = "n", axes = FALSE, xlim=c(0, som_obj$grid$xdim), 
       ylim=c(0, som_obj$grid$ydim), xlab="", ylab= "", asp=1, main="U-Matrix")
  
  ColRamp <- rev(designer.colors(n=9, col=brewer.pal(9, "Spectral")))
  
  #color code for each neuron
  ColorCode <- rep("#FFFFFF", length(neigh.dists)) #default is all white
  
  if(type == "Equal Interval") {
    #Equal interval bins
    Bins <- seq(min(neigh.dists), max(neigh.dists), length=length(ColRamp))
  }
  
  if(type == "Quantile") {
    #Quantile colorbins
    Bins <- quantile(x=neigh.dists, probs=cumsum(rep(1/length(ColRamp), length(ColRamp))))
  }
  
  
  for (i in 1:length(neigh.dists))
    if (!is.na(neigh.dists[i])) ColorCode[i] <- ColRamp[which.min(abs(Bins-neigh.dists[i]))] 
  
  offset <- 0.5 #offset for the hexagons when moving up a row
  ind <- 1
  for (row in 1:som_obj$grid$ydim) {
    for (column in 0:(som_obj$grid$xdim - 1)){ 
      Hexagon(column + offset, row - 1, col = ColorCode[ind])
      ind <- ind +1}
    offset <- ifelse(offset, 0, 0.5)
  }  
}


