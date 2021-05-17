#' @title Visualization of PCA by taxonomic family
#' @description This function graphs a PCA on a user-defined taxonomic family
#' @param data dataframe including numerical data as well as information on taxonomic family and genus
#' @param family.name family name as it appears in the dataframe
#' @keywords PCA taxonomic graph
#' @export
#' @examples graph.family(data = d, family.name = "Cercopithecidae")

graph.family<-function(data, family.name){
  data<-na.omit(data)
  dfam<-filter(data, Family==family.name)
  pcafam<-prcomp(Filter(is.numeric, dfam), center=T, scale=T)
  fviz_pca_ind(pcafam, geom.ind = "point", pointshape = 21,
  pointsize = 2, fill.ind = dfam$Genus, invisible = "quali", palette = "pal8",
  addEllipses = T, legend.title="Genus") + labs(title = "PCA", x = "PC1", y = "PC2")
}
