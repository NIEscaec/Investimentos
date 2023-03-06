#' Download Planilha
#'
#' @param link Site onde se encontra o arquivo da planilha
#' @param tabela Nome da planilha após ser exportada pro diretório
#'
#' @return Arquivo em formato legível pelo R exportado pra pasta desejada
#' @export


  download_planilha <- function(link, tabela){
    httr::GET(link, config = httr::config(ssl_verifypeer = F),
              httr::write_disk(here::here("data-raw", tabela), overwrite = T))
  }

