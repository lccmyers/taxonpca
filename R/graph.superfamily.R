#' @title Visualization of PCA by taxonomic superfamily
#' @description This function graphs a PCA on a user-defined taxonomic superfamily
#' @param data dataframe including numerical data as well as information on taxonomic superfaimly and family
#' @param superfamily.name superfamily name as it appears in the dataframe
#' @keywords PCA taxonomic graph
#' @export
#' @examples graph.superfamily(data = d, superfamily.name = "Ceboidea")

graph.superfamily<-function(data, superfamily.name){
  data<-na.omit(data)
  dsup<-filter(data, Superfamily==superfamily.name)
  pcasup<-prcomp(Filter(is.numeric, dsup), center=T, scale=T)
  fviz_pca_ind(pcasup, geom.ind = "point", pointshape = 21,
  pointsize = 2, fill.ind = dsup$Family, invisible = "quali", palette = "pal8",
  addEllipses = T, legend.title="Family") + labs(title = "PCA", x = "PC1", y = "PC2")
}
