
#' @export

  grafico_ranking_proporcao <- function(tabela){

    proporção <- tabela %>%
      select(Discriminação, percent, rank, tipo, ano)%>%
      filter(ano == max(ano))


    ggplot(proporção, aes(x = percent, y = rank, group = " " ))+
      geom_point()+
      geom_point(data = proporção %>%
                   dplyr::filter(proporção$Discriminação == pais), ggplot2::aes(color = Discriminação), show.legend = F)+
      geom_label_repel(label = proporção$Discriminação)+
      theme_minimal()+
      scale_y_reverse(breaks = scales::breaks_pretty(), labels = scales::label_ordinal())+
      scale_x_continuous(labels = scales::label_percent())+
      facet_wrap(~tipo, scales = "free_y")+
      ggplot2::labs(title = glue::glue("Brasil - {pais}, ranking e proporção de Investimentos, em 2020"),
                    caption = "Fonte: Banco Central", x = NULL, y = NULL)
  }
