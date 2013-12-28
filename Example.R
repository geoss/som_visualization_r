##CODE FOR BRUNDSON AND SINGLETON BOOK CHAPTER.

##LIBRARIES
library(kohonen)

##Code for Plots
source("somComponentPlanePlottingFunction.R")
source("Map_COUNTY_BMU.R")
source("plotUMatrix.R")


#Load Data
##DATA FOR ALL BLOCKGROUPS IN THE US
##SOURCE ACS 2006-2010 AND CENSUS 2010
load("somInput.rdata")

#Build SOM
aGrid <- somgrid(xdim = 160, ydim = 100, topo="hexagonal")

##NEXT LINE IS SLOW!!!
##Rlen is arbitrarily low
aSom <- som(data=as.matrix(scale(na.omit(usa.bg.som[,1:7]))), grid=aGrid, rlen=1, alpha=c(0.05, 0.01), keep.data=FALSE)

##VISUALIZE RESULTS
##COMPONENT PLANES
dev.off()
par(mar = rep(1, 4))
cplanelay <- layout(matrix(1:8, nrow=4))
vars <- colnames(aSom$data)
for(p in vars) {
  plotCplane(som_obj=aSom, variable=p, legend=FALSE, type="Quantile")
}
plot(0, 0, type = "n", axes = FALSE, xlim=c(0, 1), 
     ylim=c(0, 1), xlab="", ylab= "")
par(mar = c(0, 0, 0, 6))
image.plot(legend.only=TRUE, col=rev(designer.colors(n=50, col=brewer.pal(9, "Spectral"))), zlim=c(-1.5,1.5))
##END PLOT

##PLOT U-MATRIX
dev.off()
plotUmat(aSom)

