#'Gráfico por setor
#'
#' @param tabela Data frame contendo os dados dos setores
#'
#' @return Gráfico pizza contendo as porcentagens dos investimentos em diferentes setores
#' @export
#'

grafico_por_setor <- function(tabela){
    if(pais %in% lista_paises_IDP_CF){
      if(sum(tabela_por_setor$`Valores.Control Final`) >= 0){
        #Criar uma nova tabela igual a de setor para trabalhar no grafico
        tab_grafico2 <- tabela
        #-----------------------------------------------------------------------------------------------------------------
        #Renomeia as colunas para ficar facil de trabalhar
        if(length(pais) == 1){
          #tab_grafico2 <- rename(tab_grafico2, "Setor" = "Setor de atividade econômica (Estoque 2020 - US$ milhões)")
          tab_grafico2 <- rename(tab_grafico2, "Control_Final" = "valor.Control Final")
          tab_grafico2 <- rename(tab_grafico2, "Invest_Imediato" = "valor.Invest Imediato")
        }else{
          #tab_grafico2 <- rename(tab_grafico2, "Setor" = "Setor de atividade econômica (Estoque 2020 - US$ milhões)")
          tab_grafico2 <- rename(tab_grafico2, "Control_Final" = "Valores.Control Final")
          tab_grafico2 <- rename(tab_grafico2, "Invest_Imediato" = "Valores.Invest Imediato")
        }
        #------------------------------------------------------------------------------------------------------------------

        #Realiza a soma do controle final
        total_control_final <- tab_grafico2 %>%
          filter(tab_grafico2$Control_Final == max(tab_grafico2$Control_Final)) %>%
          select(-grep("Control_Final", colnames(tab_grafico2))) %>%
          summarise(var = colSums(tab_grafico2[3]))
        #------------------------------------------------------------------------------------------------------------------

        #Separa a coluna controle final
        coluna_control_final <- tab_grafico2 %>%
          select(Control_Final)
        #------------------------------------------------------------------------------------------------------------------

        #Calculo do porcentagem final
        tab_grafico2_porcentagem = coluna_control_final * 100 / total_control_final$var
        #------------------------------------------------------------------------------------------------------------------

        #Add a porcentagem na tabela
        tab_grafico2$porcentagem <-  tab_grafico2_porcentagem$Control_Final
        #------------------------------------------------------------------------------------------------------------------
        #Filtra somente dados maiores que 0 para gerar o grafico
        tab_grafico2 <- tab_grafico2 %>% filter(porcentagem >= 0.01)

        #------------------------------------------------------------------------------------------------------------------
        #Cria o grafico
        ggplot(tab_grafico2, aes(x="", y = porcentagem, fill= Setor ),
               label = porcentagem )+
          geom_bar(stat = "identity")+
          coord_polar(theta = "y", start = 0)+
          theme_void()+
          geom_label_repel(label = paste(round(tab_grafico2$porcentagem ,2),"%"), show.legend = F, size = 5,
                           position =  position_stack(vjust = 0.5))
      }
    }else{
      print("Sem dados suficientes para gerar o gráfico!")
    }
  }
