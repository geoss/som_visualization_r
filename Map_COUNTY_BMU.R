##MAP HEXAGONS CORRESPONDING TO GEOGRAPHIES
geog_hexa_map<- function(list_of_unit.classif, som_obj, title="") {
  plot(0, 0, type = "n", axes = FALSE, xlim=c(0, som_obj$grid$ydim), 
       ylim=c(0, som_obj$grid$xdim), xlab="", ylab= "", asp=1, main=title)
  
  offset <- 0.5 #offset for the hexagons when moving up a row
  ctr <- 1
  for (row in 1:som_obj$grid$ydim) {
    for (column in 0:(som_obj$grid$xdim - 1)){
      if (ctr %in% list_of_unit.classif){
        color = "black"
      }
      else { 
        color="grey" 
      }
      ctr <- ctr +1  
      Hexagon(column + offset, row - 1, col = color, border="black")}
    offset <- ifelse(offset, 0, 0.5)
  } 
}





