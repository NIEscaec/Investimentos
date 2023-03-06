


#' @export
  grafico_IBD <- function(tabela){
    tab_grafico3 <- tabela_IBD

    tab_grafico3 <- tab_grafico3 %>%
      select(names,"2006", "2007", "2008", "2009", "2010","2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")%>%
      pivot_longer(cols = "2006":"2021", names_to = "anos", values_to = "setor")
    #------------------------------------------------------------------------------------------------------------------

    a <- tab_grafico3 %>%
      filter("IBD-Participação no Capital(Invest.Imed)" == names)

    b <- tab_grafico3 %>%
      filter("IBD-Operações Intercompanhia" == names)

    c <- tab_grafico3 %>%
      filter("Fluxo-Participação no Capital(Invest.Imed)" == names)


    qanos<- nrow(tab_grafico3)

    if(sum(a$setor ) > 0 || sum(b$setor) > 0 || sum(c$setor) > 0){
      ggplot(a, aes(x = anos, y = setor ),
             b, aes(y = setor ),
             c, aes(y = setor )
      ) +
        geom_bar(stat = "identity", aes(y = a$setor, fill = "IBD-Participação no Capital(Invest. Imed)" ),
                 position = position_nudge(x = -.15), width = .3)+

        geom_bar(stat = "identity", aes(y = b$setor, fill = "IBD-Operações Intercompanhia"  ),
                 position = position_nudge(x = .15), width = .3)+

        geom_line(aes(y = c$setor*10,group = "",  colour = "Fluxo-Participação no Capital(Invest. Imed)"), size = 1, linetype = 1) +

        scale_y_continuous("US$ milhões", sec.axis = sec_axis(~ . /10 ))+
        scale_x_yearmon(NULL, format = "%Y", breaks = seq(2006,2021))+
        scale_color_manual(NULL, values = saturation(c("#6959CD","#99b765","#5d83ad"), scalefac(0.8)))+
        scale_fill_manual(NULL, values = saturation(c("#99b765","#5d83ad","#6959CD"), scalefac(0.8)))+
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
              axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position="bottom", legend.box = "vertical")
    }else{
      print("Sem dados suficientes para gerar o gráfico!")
    }
  }
