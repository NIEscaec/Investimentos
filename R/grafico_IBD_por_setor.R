#' @export


  grafico_IBD_Por_Setor <- function(tabela){
    if(pais %in% lista_paises_IBD_por_setor){
      tab_grafico6 <- tabela

      # Realiza a soma do controle final
      total_IBD_Por_Setor <- tab_grafico6 %>%
        filter(tab_grafico6$Valores == max(tab_grafico6$Valores)) %>%
        select(-grep("Valores", colnames(tab_grafico6))) %>%
        summarise(var = colSums(tab_grafico6[2]))
      #------------------------------------------------------------------------------------------------------------------

      # Separa a coluna controle final
      coluna_valores_IBD_Por_Setor <- tab_grafico6 %>%
        select(Valores)
      #------------------------------------------------------------------------------------------------------------------

      # Calculo do porcentagem final
      tab_grafico6_porcentagem = coluna_valores_IBD_Por_Setor * 100 / total_IBD_Por_Setor$var
      #------------------------------------------------------------------------------------------------------------------

      # Add a porcentagem na tabela
      tab_grafico6$porcentagem <-  tab_grafico6_porcentagem$Valores
      #------------------------------------------------------------------------------------------------------------------

      #Filtra somente dados maiores que 0 para gerar o grafico
      tab_grafico6 <- tab_grafico6 %>% filter(porcentagem >= 0.01)
      #------------------------------------------------------------------------------------------------------------------

      ggplot(tab_grafico6, aes(x="", y = porcentagem, fill= Setores), label = value)+
        geom_bar(stat = "identity")+
        coord_polar(theta = "y", start = 0)+
        theme_void()+
        geom_label_repel(label = paste(round(tab_grafico6$porcentagem,2),"%"),
                         show.legend = F,
                         size = 5,
                         position =  position_stack(vjust = 0.5))
    }
  }
