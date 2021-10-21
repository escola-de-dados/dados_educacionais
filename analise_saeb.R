#script

#Pacotes utilizados

#caso nao tenha, instalar com o comando abaixo
install.packages("data.table")

#carregar na biblioteca
library(data.table)

#caso nao tenha, instalar com o comando abaixo
install.packages("dplyr")

#Carregar na biblioteca
library(dplyr)

#Codigo para carregarmos a base de dados do microdados do Saeb 2019

#IMPORTANTE: Devemos estar setados na pasta onde o arquivo esta

dados_1 <- fread(file = "TS_ALUNO_5EF.csv",
                 header = TRUE,
                 select = c("ID_REGIAO",
                            "ID_UF",
                            "PROFICIENCIA_LP_SAEB",
                            "PROFICIENCIA_MT_SAEB",
                            "TX_RESP_Q002",
                            "TX_RESP_Q004",
                            "TX_RESP_Q005"))

#Agrupas as respostas dos estudantes nos itens dos questionarios em relacao as proficiencias
dados_1_1 <- dados_1 %>%
  group_by(TX_RESP_Q002, TX_RESP_Q004) %>%
  summarise(quantidade = n(),
            media_lp = mean(PROFICIENCIA_LP_SAEB),
            desviolp = sd(PROFICIENCIA_LP_SAEB),
            media_mt = mean(PROFICIENCIA_MT_SAEB),
            desvio_mt = sd(PROFICIENCIA_MT_SAEB))


#Ver os dados
View(dados_1_1)

#E agora?!

#vamos passar um parametro nas funcoes de media e desvio
dados_1_1 <- dados_1 %>%
  group_by(TX_RESP_Q002, TX_RESP_Q004) %>%
  summarise(quantidade = n(),
            media_lp = mean(PROFICIENCIA_LP_SAEB, na.rm = TRUE),
            desviolp = sd(PROFICIENCIA_LP_SAEB, na.rm = TRUE),
            media_mt = mean(PROFICIENCIA_MT_SAEB, na.rm = TRUE),
            desvio_mt = sd(PROFICIENCIA_MT_SAEB, na.rm = TRUE))



#---------------------------------------------------------------------
#Vamos limpar os dados

#Vamos retirar os dados faltante
dados_1 <- dados_1 %>% na.omit

#Retirar os * e . do conjunto de dados
dados_1 <- dados_1 %>%
  filter(TX_RESP_Q002 != "*") %>%
  filter(TX_RESP_Q002 != ".") %>%
  filter(TX_RESP_Q004 != "*") %>%
  filter(TX_RESP_Q004 != ".") %>%
  filter(TX_RESP_Q005 != "*") %>%
  filter(TX_RESP_Q005 != ".")

#Agora, vamos agrupar novamente, porem sem os dados sujos
dados_1_2 <- dados_1 %>%
  group_by(TX_RESP_Q002, TX_RESP_Q004) %>%
  summarise(quantidade = n(),
            media_lp = mean(PROFICIENCIA_LP_SAEB),
            desviolp = sd(PROFICIENCIA_LP_SAEB),
            media_mt = mean(PROFICIENCIA_MT_SAEB),
            desvio_mt = sd(PROFICIENCIA_MT_SAEB))

#--------------------------------------------------------------------
#Modificar dados

#Mudar os nomes da variavel TX_RESP_Q002
dados_1_2 <- dados_1_2 %>%
  mutate(TX_RESP_Q002 = case_when(TX_RESP_Q002 == "A" ~ "Branca",
                                  TX_RESP_Q002 == "B" ~ "Preta",
                                  TX_RESP_Q002 == "C" ~ "Parda",
                                  TX_RESP_Q002 == "D" ~ "Amarela",
                                  TX_RESP_Q002 == "E" ~ "Indigena",
                                  TX_RESP_Q002 == "F" ~ "Nao quero declarar"))


#Mudar os nomes da variavel TX_RESP_Q004

dados_1_2 <- dados_1_2 %>%
  mutate(TX_RESP_Q004 = case_when(TX_RESP_Q004 == "A" ~ "Nao terminou o 5ano",
                                  TX_RESP_Q004 == "B" ~ "Fundamental I completo",
                                  TX_RESP_Q004 == "C" ~ "Fundamental completo",
                                  TX_RESP_Q004 == "D" ~ "Ensino medio completo",
                                  TX_RESP_Q004 == "E" ~ "Ensino superior completo",
                                  TX_RESP_Q004 == "F" ~ "Nao sei"))





