
#' @export

  relatorio_investimentos <- function(pais){
    rmarkdown::render(system.file("rmd", "Investimentos_Brasil_Pais.Rmd", package = "Investimentos"),
                      params = list(
                        title = paste0("Investimentos Brasil -  ", pais),
                        pais = pais),
                      output_dir = here::here("data/relatorios_investimentos"),
                      output_file = paste0("investimentos_", pais))

  }


