#' @title PCA by taxonomic superfamily
#' @description This function runs a PCA on a user-defined taxonomic superfamily
#' @param data dataframe including numerical data as well as information on taxonomic superfamily and family
#' @param superfamily.name superfamily name as it appears in the dataframe
#' @keywords PCA taxonomic
#' @export
#' @examples pca.superfamily(data = d, superfamily.name = "Ceboidea")

pca.superfamily<-function(data, superfamily.name){
  data<-na.omit(data)
  dsup<-filter(data, Superfamily==superfamily.name)
  pcasup<-prcomp(Filter(is.numeric, dsup), center=T, scale=T)
  return(summary(pcasup))
}
