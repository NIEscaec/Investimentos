#' Soma de Quantidade de Investidores
#'
#' @param nlinha Data frame
#' @param len Quantidade de colunas
#' @param anos Anos
#'
#' @return Data frame contendo o somatório da quantidade de investidores dos países
#' @export


soma_Qtd_Invest_IDP <- function(nlinha, len, anos){

  nlinha$`2010` <- as.numeric(nlinha$`2010`)
  nlinha$`2015` <- as.numeric(nlinha$`2015`)
  nlinha$`2020` <- as.numeric(nlinha$`2020`)

  nlinha[is.na(nlinha)] <- 0

  soma <- apply(nlinha[,2:len], 2, FUN=sum)

  def <- data_frame(anos, soma)

  def <- def %>%
    pivot_wider(names_from = anos, values_from = soma)
}



