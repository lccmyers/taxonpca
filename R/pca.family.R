#' @title PCA by taxonomic family
#' @description This function runs a PCA on a user-defined taxonomic family
#' @param data dataframe including numerical data as well as information on taxonomic family and genus
#' @param family.name family name as it appears in the dataframe
#' @keywords PCA taxonomic
#' @export
#' @examples pca.family(data = d, family.name = "Cercopithecidae")

pca.family<-function(data, family.name){
  data<-na.omit(data)
  dfam<-filter(data, Family==family.name)
  pcafam<-prcomp(Filter(is.numeric, dfam), center=T, scale=T)
  return(summary(pcafam))
}
