##########################################
##PLOT TEXT ONTO SOM
##from "kohonen" library output
##
##
##BY JOS L. TEUNISSEN VRIJE UNIVERSITEIT BRUSSEL
################################################

require(hash)

get_counts <- function(ci){
    counts <- hash()
    for (i in 1:length(ci)){
        key <- as.character(ci[i])
        if (is.null(counts[[key]])){
            counts[ci[i]]<-i
        } else{
            counts[ci[i]]<-c(counts[[key]],i)
        }
    }
    counts
}

add.points <- function(som){
    counts<-get_counts(som$unit.classif)
    offset <- 0.5 #offset for the hexagons when moving up a row
    ind <- 1
    nx = som$grid$xdim
    ny = som$grid$ydim
    for (row in 1:ny) {
        for (column in 1:nx){
            count <- counts[[as.character(ind)]]
            if (!is.null(count)){
                x <- column + offset - 0.5
                y <- row - 0.5
                # text(column + offset - 0.5, row - 0.5, length(count))
                if (length(count)==1){
                    text(x, y, 'o')
                }else if(length(count)==2){
                    text(x, y + 0.1875, 'o')
                    text(x, y - 0.1875, 'o')
                }else if(length(count)==3){
                    text(x, y, 'o')
                    text(x, y + 0.28125, 'o')
                    text(x, y - 0.28125, 'o')
                }else if(length(count)==4){
                    text(x + 0.1875, y + 0.1875, 'o')
                    text(x - 0.1875, y + 0.1875, 'o')
                    text(x + 0.1875, y - 0.1875, 'o')
                    text(x - 0.1875, y - 0.1875, 'o')
                }else if(length(count)==5){
                    text(x + 0.2, y + 0.2, 'o')
                    text(x - 0.2, y + 0.2, 'o')
                    text(x + 0.2, y - 0.2, 'o')
                    text(x - 0.2, y - 0.2, 'o')
                    text(x,y,'o')
                }else{
                    text(x, row - 0.5, length(count))
                }
            }
            ind <- ind +1
        }
    offset <- ifelse(offset, 0, 0.5)
    }
}

add.numbers <- function(som, scale=0.8){
    counts<-get_counts(som$unit.classif)
    offset <- 0.5 #offset for the hexagons when moving up a row
    ind <- 1
    nx = som$grid$xdim
    ny = som$grid$ydim
    for (row in 1:ny) {
        for (column in 1:nx){
            ns <- counts[[as.character(ind)]]
            if (!is.null(ns)){
                x <- column + offset - 0.5
                y <- row - 0.5

                if (length(ns)==1){
                    text(x, y, ns[1], cex=scale)
                }else if(length(ns)==2){
                    text(x, y + 0.1875, ns[1], cex=scale)
                    text(x, y - 0.1875, ns[2], cex=scale)
                }else if(length(ns)==3){
                    text(x, y + 0.28125, ns[1], cex=scale)
                    text(x, y, ns[2], cex=scale)
                    text(x, y - 0.28125, ns[3], cex=scale)
                }else if(length(ns)==4){
                    text(x + 0.1875, y + 0.1875, ns[2], cex=scale)
                    text(x - 0.1875, y + 0.1875, ns[1], cex=scale)
                    text(x + 0.1875, y - 0.1875, ns[4], cex=scale)
                    text(x - 0.1875, y - 0.1875, ns[3], cex=scale)
                }else if(length(ns)==5){
                    text(x + 0.2, y + 0.2, ns[2], cex=scale)
                    text(x - 0.2, y + 0.2, ns[1], cex=scale)
                    text(x + 0.2, y - 0.2, ns[5], cex=scale)
                    text(x - 0.2, y - 0.2, ns[4], cex=scale)
                    text(x,y,ns[3], cex=scale)
                }else{
                    text(x, row - 0.5, length(ns))
                }
            }
            ind <- ind +1
        }
    offset <- ifelse(offset, 0, 0.5)
    }
}
