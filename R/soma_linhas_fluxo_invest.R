#' Soma linhas de fluxo
#'
#' @param nlinha numero de linhas do arquivo
#'
#'
#'
#' @export


soma_linhas_fluxo_invest <- function(nlinha){
  nlinha[is.na(nlinha)] <- 0
  soma <- apply(nlinha[,2:22], 2, FUN=sum)
  def <- data_frame(anos, soma)
  def <- def %>%
    pivot_wider(names_from = anos, values_from = soma)
}
