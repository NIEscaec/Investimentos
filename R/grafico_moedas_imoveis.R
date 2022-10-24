
#' @export
grafico_moedas_imoveis <- function(tabela){
    tab_grafico5 <- tabela

    tab_grafico5 <- tab_grafico5 %>%
      select(names,"2010","2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")%>%
      pivot_longer(cols = "2010":"2020", names_to = "anos", values_to = "valores")
    #------------------------------------------------------------------------------------------------------------------

    a <- tab_grafico5 %>%
      filter("Moedas/Depósitos" == names)

    b <- tab_grafico5 %>%
      filter("Imóveis" == names)
    #------------------------------------------------------------------------------------------------------------------

    if(sum(a$valores) > 0 || sum(b$valores > 0)){
      ggplot(a, aes(x = anos, y = valores ),
             b, aes(x = anos, y = valores )) +
        geom_bar(stat = "identity", aes(y = a$valores, fill = "Moedas/Depósitos" ), position = position_nudge(x = -.15), width = .3)+
        geom_bar(stat = "identity", aes(y = b$valores, fill = "Imóveis"  ), position = position_nudge(x = .15), width = .3) +
        scale_y_continuous("US$ milhões")+
        scale_x_yearmon(NULL, format = "%Y", breaks = seq(2010,2020))+
        scale_fill_manual(NULL, values = saturation(c("#B22222","#5b80ab"), scalefac(0.8)))+
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
      print("Sem dados suficiente para gerar o gráfico")
    }
  }
