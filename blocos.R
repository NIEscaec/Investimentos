
library(tidyverse)


lista_blocos <- comerciobr2::dic_blocos%>%
  select(no_bloco)


  lista_investimentos <- Investimentos::lista_paises



  bloco <- c("G20","Africa", "CPLP", "Países Americanos", "Países Latino-Americanos", "América do Sul", "Mercosul", "OCDE",
             "União Europeia", "EFTA", "ASEAN", "Aliança do Pacífico")

  blocos <- data.frame(bloco)



  lista_blocos <- unique(lista_blocos$no_bloco)




  a <- c("África do Sul", "Alemanha", "Arábia Saudita", "Argentina", "Austrália", "Canadá", "China", "Coreia do Sul", "Estados Unidos", "França", "Índia", "Indonésia", "Itália", "Japão", "México", "Reino Unido", "Rússia", "Turquia", "Áustria", "Bélgica", "Bulgária", "Chipre", "Croácia", "Dinamarca", "Eslováquia", "Eslovênia", "Espanha", "Estônia", "Finlândia", "Grécia", "Hungria", "Irlanda", "Itália", "Letônia", "Lituânia", "Luxemburgo", "Malta", "Polônia", "Portugal", "Romênia", "Suécia", "Países Baixos (Holanda)", "Tcheca, República")
  c("África do Sul", "Angola", "Argélia", "Benin", "Botsuana", "Burkina Faso", "Burundi", "Cabo Verde", "Camarões", "Chade", "Comores", "Costa do Marfim","Egito", "Etiópia", "Gabão", "Gâmbia", "Gana", "Guiné Equatorial", "Djibuti", "Guiné-Bissau", "Madagascar", "Lesoto", "Libéria", "Líbia", "Malavi", "mali",Maurício, "Marrocos", "Quênia",  "Mauritânia", "Moçambique", "Namíbia", "Níger", "Nigéria", "Congo", "República Democrática", "República Centro-Africana","Seicheles", "Ruanda", "Saara Ocidental", "São Tomé e Príncipe", "Senegal", "Serra Leoa", "Somália", "Suazilândia", "Sudão", "Sudão do Sul", "Tanzânia", "Togo","Tunísia", "Uganda", "Zâmbia", "Zimbábue")


  blocos <- blocos[c(2(a))]

  blocos <- blocos[ncol(2)] <- a

blocos[c(2:ncol(blocos)),] <- a



a <- as.list(a)

blocos[1,2] <- c("África do Sul":"Tcheca, República")

blocos[1,2] <- "teste"























