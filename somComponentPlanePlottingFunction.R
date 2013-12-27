##PLOTTING FUNCTION
plotCplane <- function(som_obj, variable=sample(colnames(som_obj$data), 1), legend=FALSE){
  if (is.numeric(variable)){
    variable <- colnames(aSom$data)[variable]
  }
  
  if (som_obj$grid$topo != "hexagonal"){
    stop("function assumes hexgonal SOM")
  }
  xCoord <- rep(1:som_obj$grid$xdim, som_obj$grid$ydim)
  yCoord <- rep(1:som_obj$grid$ydim, each=som_obj$grid$xdim)
  plane_codebook <- data.frame(xCoord, yCoord, som_obj$codes) #contains vector codebook and position
  
  component_plane_matrix <- function(data=plane_codebook, variable_index_or_name=variable){
    cp <- matrix(nrow=som_obj$grid$ydim, ncol=som_obj$grid$xdim, data=plane_codebook$data[,variable_index_or_name], byrow=TRUE)
    return(cp)
  }
  
  #Function to create the polygon for each hexagon
  #from http://nbremer.blogspot.nl/2013/11/how-to-create-hexagonal-heatmap-in-r.html
  Hexagon <- function (x, y, unitcell = 1, col = "grey", border=NA) {
    polygon(c(x, x, x + unitcell/2, x + unitcell, x + unitcell, 
              x + unitcell/2), c(y + unitcell * 0.125, y + unitcell * 
                                   0.875, y + unitcell * 1.125, y + unitcell * 0.875, 
                                 y + unitcell * 0.125, y - unitcell * 0.125), 
            col = col, border=border)
  }
  
  hm <- component_plane_matrix(data=aSom, variable_index_or_name=variable)
  
  plot(0, 0, type = "n", axes = FALSE, xlim=c(0, SOM_Columns), 
       ylim=c(0, SOM_Rows), xlab="", ylab= "", asp=1, main=substr(x=variable, start=1, stop=10))
  
  ColRamp <- rev(designer.colors(n=50, col=brewer.pal(9, "Spectral")))
  
  ColorCode <- rep("#FFFFFF", length(hm)) #default is all white
  Bins <- seq(-1.5, 1.5, length=length(ColRamp))
  for (i in 1:length(hm))
    if (!is.na(hm[i])) ColorCode[i] <- ColRamp[which.min(abs(Bins-hm[i]))] 
  
  offset <- 0.5 #offset for the hexagons when moving up a row
  for (row in 1:som_obj$grid$xdim) {
    for (column in 0:(som_obj$grid$ydim - 1)) 
      Hexagon(column + offset, row - 1, col = ColorCode[row + SOM_Rows * column])
    offset <- ifelse(offset, 0, 0.5)
  }  
  if (legend==TRUE){
    image.plot(legend.only=TRUE, col=ColRamp, zlim=c(-1.5,1.5))
  }
  
}