#' Ler linha
#'
#' @param dataframe Data frame
#' @param len Quantidade de colunas
#'
#' @return Data frame filtrado com os países selecionados, colunas renomeadas, e colunas em type numeric
#' @export


ler_linha <- function(dataframe, len){
  dataframe %>%
    renomear_disc() %>%
    selecionarPais() %>%
    mutate(dplyr::across(.cols=2:len, .fns=as.numeric))
}
# Invest_Imediato_IDE <- ler_excel("data-raw/TabelasCompletasPosicaoIDE.xlsx", "3", 4)
#
# pais <- c("Alemanha")
#
# fluxo_Invest_InvEstrp <- fluxo_Invest_InvEstrp %>%
#   renomear_disc()
#
#
#
#
# fluxo_Invest_InvEstrp <- fluxo_Invest_InvEstrp %>%
#   selecionarPais()
#
# fluxo_Invest_InvEstrp <- fluxo_Invest_InvEstrp %>%
#   mutate(dplyr::across(.cols=2:13, .fns=as.numeric))
#
#
#

