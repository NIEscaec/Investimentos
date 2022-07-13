#' Relatorio Brasil Pais
#'
#' @param tabela Data frame
#' @param names Teste com o marksown
#'
#' @return Pdf's com a lista de países
#' @export


relatorio_brasil_pais <- function(pais) {
  rmarkdown::render(system.file("rmd", "Investimentos_Relatorios.Rmd", package = "barao2"),
                    params = list(
                      title = paste0("Investimentos Brasil - "),
                      pais = pais
                    ),
                    # intermediates_dir = "temp",
                    output_dir = here::here("data"),
                    output_file = paste0("Investimentos_", pais))
}
