#' Soma de Quantidade de Investidores
#'
#' @param nlinha Data frame
#' @param len Quantidade de colunas
#' @param anos Anos
#'
#' @return Data frame contendo o somatório da quantidade de investidores dos países
#' @export


soma_Qtd_Invest_IBD <- function(nlinha, len, anos){

  nlinha$`2007` <- as.numeric(nlinha$`2007`)
  nlinha$`2008` <- as.numeric(nlinha$`2008`)
  nlinha$`2009` <- as.numeric(nlinha$`2009`)
  nlinha$`2010` <- as.numeric(nlinha$`2010`)
  nlinha$`2011` <- as.numeric(nlinha$`2011`)
  nlinha$`2012` <- as.numeric(nlinha$`2012`)
  nlinha$`2013` <- as.numeric(nlinha$`2013`)
  nlinha$`2014` <- as.numeric(nlinha$`2014`)
  nlinha$`2015` <- as.numeric(nlinha$`2015`)
  nlinha$`2016` <- as.numeric(nlinha$`2016`)
  nlinha$`2017` <- as.numeric(nlinha$`2017`)
  nlinha$`2018` <- as.numeric(nlinha$`2018`)
  nlinha$`2019` <- as.numeric(nlinha$`2019`)
  nlinha$`2020` <- as.numeric(nlinha$`2020`)
  nlinha$`2021` <- as.numeric(nlinha$`2021`)

  nlinha[is.na(nlinha)] <- 0

  soma <- apply(nlinha[,2:len], 2, FUN=sum)

  def <- data_frame(anos, soma)

  def <- def %>%
    pivot_wider(names_from = anos, values_from = soma)
}



