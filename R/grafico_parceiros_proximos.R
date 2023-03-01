
#' @export


  grafico_parceiros_proximos <- function(tabela){

    tabela %>%
        select(Discriminação, value, rank, tipo, ano)%>%
        filter(ano == max(ano))%>%
        ggplot2::ggplot() +
        ggplot2::geom_col(
          ggplot2::aes(tidytext::reorder_within(Discriminação, value, tipo), value, fill = tipo),
          show.legend = F) +
        ggplot2::geom_col(data = . %>%
                            dplyr::filter(Discriminação == pais),
                          ggplot2::aes(tidytext::reorder_within(Discriminação, value, tipo), value, fill = "red"),
                          show.legend = F) +
        ggplot2::geom_label(ggplot2::aes(tidytext::reorder_within(Discriminação, value, tipo), value,
                                         label = rank)) +
        ggplot2::facet_wrap(~tipo, scales = "free_y") +
        ggplot2::labs(title = NULL,
                      subtitle = NULL,
                      x = NULL, y = NULL, caption = "") +
        ggplot2::coord_flip() +
        ggplot2::theme_minimal() +
        ggplot2::scale_y_continuous(labels = scales::label_number_si()) +
        ggthemes::scale_fill_tableau() +
        tidytext::scale_x_reordered()+
        ggplot2::labs(title = glue::glue("Brasil - {pais}, parceiros de investimento próximos, em 2021"),
                      caption = "Fonte: Banco Central", x = NULL, y = NULL)

  }
