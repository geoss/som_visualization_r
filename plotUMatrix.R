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
##kohnonen library source
################################################

library(RColorBrewer) #to use brewer.pal
library(fields) #to use designer.colors

##PLOTTING FUNCTION
plotUmat <- function(som_obj, type="Equal Interval"){
  if (som_obj$grid$topo != "hexagonal"){
    stop("function assumes hexgonal SOM")
  }

#CALCULATE U-MATRIX
  nhbrdist <- unit.distances(grid=som_obj$grid, toroidal=FALSE)
  #nhbrdist[nhbrdist > 1.05] <- NA
  if (som_obj$method == "som") {
    for (i in 2:nrow(nhbrdist)) {
      for (j in 1:(i - 1)) {
        if (!is.na(nhbrdist[i, j])) 
          nhbrdist[i, j] <- nhbrdist[j, i] <- dist(som_obj$codes[c(i, 
                                                                j), ])
      }
    }
  }
  
  #NEURON SUM
  neigh.dists <- colSums(nhbrdist, na.rm = TRUE)
  #NEED TO DO SOMETHING ABOUT EDGES/CORNERS
  
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
  for (row in 1:som_obj$grid$ydim) {
    for (column in 0:(som_obj$grid$xdim - 1)) 
      Hexagon(column + offset, row - 1, col = ColorCode[row + som_obj$grid$xdim * column])
    offset <- ifelse(offset, 0, 0.5)
  }  
}


