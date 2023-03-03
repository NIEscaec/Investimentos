#'  Soma de linhas de cada país
#'
#' @param nlinha o nome de um dataframe que contenha dados dos países de 2010 a 2020
#'
#' @return Soma das linhas, transformando em uma linha apenas
#' @export


soma_linhas <- function(nlinha, tam){
  nlinha[is.na(nlinha)] <- 0
  soma <- apply(nlinha[,2:tam], 2, FUN=sum)
  def <- data_frame(anos, soma)
  def <- def %>%
    pivot_wider(names_from = anos, values_from = soma)
}
