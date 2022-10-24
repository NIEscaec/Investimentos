

#' @export
  grafico_investimentos_carteira <- function(tabela){
    tab_grafico4 <- tabela


    if(sum(tab_grafico4$`2010` > 0) || sum(tab_grafico4$`2011` > 0) || sum(tab_grafico4$`2012` > 0) || sum(tab_grafico4$`2013` > 0) || sum(tab_grafico4$`2014` > 0) ||
       sum(tab_grafico4$`2015` > 0) || sum(tab_grafico4$`2016` > 0) || sum(tab_grafico4$`2017` > 0) || sum(tab_grafico4$`2018` > 0) || sum(tab_grafico4$`2019` > 0) ||
       sum(tab_grafico4$`2020` > 0)){
      # -----------------------------------------------------------------------------------------------------------
      tab_grafico4 <- tab_grafico4 %>%
        select(names,"2010","2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")%>%
        pivot_longer(cols = "2010":"2020", names_to = "anos", values_to = "valores")

      a <- tab_grafico4 %>%
        filter("Ações" == names)

      b <- tab_grafico4 %>%
        filter("Renda Fixa de Curto Prazo" == names)

      c <- tab_grafico4 %>%
        filter("Renda Fixa de Longo Prazo" == names)
      # -----------------------------------------------------------------------------------------------------------

      ggplot(a, aes(x = anos, y = valores),
             b, aes(x = anos, y = valores),
             c, aes(x = anos, y = valores)) +

        geom_bar(stat = "identity", aes(y = a$valores, fill = "Ações" ),
                 position = position_nudge(x = -.30), width = .3)+

        geom_bar(stat = "identity", aes(y = b$valores, fill = "Renda Fixa de Curto Prazo" ),
                 position = position_nudge(x = -0), width = .3)+

        geom_bar(stat = "identity", aes(y = c$valores, fill = "Renda Fixa de Longo Prazo" ),
                 position = position_nudge(x = .30), width = .3)+

        scale_y_continuous("US$ milhões")+
        scale_x_yearmon(NULL, format = "%Y", breaks = seq(2010,2020))+
        scale_fill_manual(NULL, values = saturation(c("#191970","#3CB371","#FF8C00"), scalefac(0.8)))+
        theme_classic ()+
        theme(panel.grid = element_blank(), # remove as linhas do corpo do gráfico
              # sem bordas entre os painéis
              panel.spacing = unit(0, "cm"),
              # modifica o texto dos eixos
              axis.text = element_text(size = 12, colour = "black"),
              # cor dos marcadores
              axis.ticks = element_line(colour = "black"),
              # tamanho dos marcadores
              axis.ticks.length = unit(.2, "cm"),
              #cor da borda
              panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
              axis.text.x = element_text(angle = 0, hjust = 0.5),
              legend.position="bottom", legend.box = "vertical")
    }else{
      print("Sem dados suficientes para gerar o gráfico!")
    }
  }
