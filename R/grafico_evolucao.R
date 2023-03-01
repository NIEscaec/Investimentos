
#' @export

  grafico_evolucao <- function(tabela){

    tabela %>%
      ggplot2::ggplot(ggplot2::aes(ano, value)) +
      ggplot2::geom_point() +
      ggplot2::geom_line(ggplot2::aes(color = Discriminação,
                                      group = Discriminação),
                         size = 1.5,
                         linetype = 4,
                         show.legend = F) +
      ggplot2::geom_line(data = . %>%
                           dplyr::filter(Discriminação == pais),
                         ggplot2::aes(color = Discriminação,
                                      group = Discriminação),
                         size = 3,
                         show.legend = F) +

      ggrepel::geom_label_repel(data = . %>%
                                  dplyr::filter(ano == max(ano) |
                                                  ano == min(ano) |
                                                  ano == max(ano)-(max(ano)-min(ano))/2),
                                ggplot2::aes(ano, value, label = Discriminação),
                                size = 2, show.legend = F) +

      ggplot2::facet_wrap(~ tipo, scales = "free",
                          nrow = 2) +
      ggthemes::scale_color_tableau() +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = NULL,
                    caption = NULL,
                    x = NULL, y = NULL)+
      ggplot2::scale_y_continuous(labels = scales::label_number_si())+
      ggplot2::scale_x_continuous(limits = c(2007, 2021),
                                  breaks = scales::breaks_pretty(15))+
      ggplot2::labs(title = glue::glue("Brasil - {pais}, evolução do investimento até 2021"),
                    caption = "Fonte: Banco Central", x = NULL, y = NULL)
  }
