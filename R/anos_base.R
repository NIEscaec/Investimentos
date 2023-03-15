#' Anos Base
#'
#' @param ano_inicio o ano que se começa a gerar os dados
#' @param ano_final o último ano em que se tem dados disponíveis
#'
#' @return Lista de todos os anos entre o inicio e o final convertidos em char.
#' @export

 anos_base <- function(ano_inicio, ano_final){
   anos <- c(ano_inicio:ano_final)
   anos <- as.character(anos)
 }
