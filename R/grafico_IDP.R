


#' @export
grafico_IDP <- function(tabela){

  tab_grafico1 <- tabela

  tab_grafico1 <- tab_grafico1 %>%
    select(names,"2001":"2021")%>%
    pivot_longer(cols = "2001":"2021", names_to = "anos", values_to = "setores")
  #------------------------------------------------------------------------------------------------------------------

  a <- tab_grafico1 %>%
    filter("IDP-Participação no Capital(Control. Final)" == names)

  b <- tab_grafico1 %>%
    filter("IDP-Operações Intercompanhia" == names)

  c <- tab_grafico1 %>%
    filter("IDP-Participação no Capital(Invest.Imed)" == names)

  #------------------------------------------------------------------------------------------------------------------
  d <- tab_grafico1 %>%
    filter("Fluxo-Participação no Capital(Invest.Imed)" == names)

  e <- tab_grafico1 %>%
    filter("Fluxo Líquido-Operações Intercompanhia" == names)

  # ------------------------------------------------------------------------------------------------------------------

  qanos <- nrow(a)
  if(sum(a$setores) > 0 || sum(b$setores) > 0 || sum(c$setores) > 0 || sum(d$setores) > 0 || sum(e$setores) > 0 ){
  ggplot(a, aes(x = anos, y = setores),
         b, aes(x = anos, y = setores),
         c, aes(x = anos, y = setores),
         d, aes(y = setores),
         e, aes(y = setores))+
    geom_bar( stat = "identity", aes(x = a$anos, y = a$setores, fill = "IDP - Participação no Capital (Controlador Final)"),
              position = position_nudge(x = -.20), width = .2)+

    geom_bar( stat = "identity", aes(x = b$anos, y = b$setores, fill = "IDP - Operações Intercompanhia"),
              position = position_nudge(x = -0), width = .2)+

    geom_bar(stat = "identity", aes(x = c$anos, y = c$setores, fill = "IDP - Participação no Capital (Invest. Imediato)"),
             position = position_nudge(x = .20), width = .2)+

    geom_line(aes(y = d$setores*3, group = " ", color = "Fluxo - Participação no Capital (Invest. Imediato)"),
              size = 1.1, linetype = 1)+

    geom_line(aes(y = e$setores*3, group = " ", color = "Fluxo Líquido - Operações Intercompanhia"),
              size = 1.1, linetype = 1)+

    geom_line(aes(y = -1, group = " " ),
              size = 1, linetype = 2)+

    scale_y_continuous("US$ milhões", sec.axis = sec_axis(~ . /3 ))+
    scale_x_yearmon(NULL, format = "%Y", n = qanos)+
    scale_color_manual(NULL, values =  saturation(c("#B22222","#8A2BE2","#252A52","#FFC465","#66ADE5"), scalefac(0.8)))+
    scale_fill_manual(NULL, values = saturation(c("#252A52","#FFC465","#66ADE5","#B22222","#8A2BE2"), scalefac(0.8)))+

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









