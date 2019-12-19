##########################################
##PLOT CLUSTER BOUNDARIES ON TOP OF C-PLANE OR U-MATRIX
##from "kohonen" library output
##
##
##BY JOS L. TEUNISSEN VRIJE UNIVERSITEIT BRUSSEL
################################################

library(RColorBrewer) #to use brewer.pal
library(fields) #to use designer.colors

add.cluster.boundaries2 <- function(som, cluster){
    if (data.som1$grid$toroidal==TRUE){
        stop("function assumes that SOM is not toroidal")
    }
    
    offset <- 0.5 #offset for the hexagons when moving up a row
    ind <- 1
    nx = som$grid$xdim
    ny = som$grid$ydim
    for (row in 1:ny) {
        for (column in 1:nx){
            text(column + offset - 0.5, row - 0.5, ind)
            x<-column+offset
            y<-row-1
            # except last row:
            if (row<ny){
                # up lines: exclude first hexagon even lines
                if ((ind%%nx!=1 | row%%2==1) && cluster[[ind]]!=cluster[[ind+9+2*offset]])
                    segments(x-1, y + 0.875, x - 1/2, y + 1.125, lwd=4)
                # down lines: exclude last hexagon odd lines
                if ((ind%%nx!=0 | row%%2==0) && cluster[[ind]]!=cluster[[ind+10+2*offset]])
                    segments(x - 1/2, y + 1.125, x, y + 0.875, lwd=4)
            }
            # vertical lines 
            if (column<nx){
                if (cluster[[ind]]!=cluster[[ind+1]])
                    segments(x, y+0.125, x, y+0.875, lwd=4)
            }
            ind <- ind +1
        }
    offset <- ifelse(offset, 0, 0.5)
    }
}

