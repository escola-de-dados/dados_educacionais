

library(data.table)
library(tidyverse)
library(esquisse)

# verifique se voce esta no diretorio certo
dir() # lista os arquivos. dentre eles deve ter um chamado "TS_ALUNO_9EF.csv"
# caso nao esteja no diretorio certo, vá no menu: session > set working diretory


# importar dados para o R
dados_1 <- fread(file = "TS_ALUNO_9EF.csv", # arquivo com microdados do Saeb para o 9o ano do EF
                 header = TRUE,
                 # selecionar variaveis a serem importadas
                 select = c("ID_REGIAO",
                            "ID_UF",
                            "PROFICIENCIA_LP_SAEB",
                            "PROFICIENCIA_MT_SAEB",
                            "TX_RESP_Q002", # cor da pele
                            "TX_RESP_Q004")) # escolaridade da mae

# dimensoes dos dados (linhas X colunas)
dim(dados_1)

# preparacao dos dados -----------------------------------------

## recodificar variavel: cor da pele -----------------------------------

# conferir as variaveis presentes
table(dados_1$TX_RESP_Q002)                            

dados_1 <- dados_1 %>%
  mutate(TX_RESP_Q002 = case_when(TX_RESP_Q002 == "A" ~ "Branca",
                                  TX_RESP_Q002 == "B" ~ "Preta",
                                  TX_RESP_Q002 == "C" ~ "Parda",
                                  TX_RESP_Q002 == "D" ~ "Amarela",
                                  TX_RESP_Q002 == "E" ~ "Indigena",
                                  TX_RESP_Q002 == "F" ~ NA_character_,
                                  TX_RESP_Q002 == "*" ~ NA_character_,
                                  TX_RESP_Q002 == "." ~ NA_character_))

# checar as recodificacao realizada
table(dados_1$TX_RESP_Q002)  

# QUESTAO 1: 
# acabamos de usar outro método para lidar com asteriscos e pontos nas respostas do questionario.
# Qual é a diferença entre este método e o da aula anterior?
# Qual função você usaria para verificar esta diferença?
# Escreva o código e verifique!



## recodificar variavel: escolaridade da mae -----------------------------------

# conferir as variaveis presentes
table(dados_1$TX_RESP_Q004) 

dados_1 <- dados_1 %>%
  mutate(TX_RESP_Q004 = case_when(TX_RESP_Q004 == "A" ~ "1.Nao terminou o 5ano",
                                  TX_RESP_Q004 == "B" ~ "2.Fundamental I completo",
                                  TX_RESP_Q004 == "C" ~ "3.Fundamental completo",
                                  TX_RESP_Q004 == "D" ~ "4.Ensino medio completo",
                                  TX_RESP_Q004 == "E" ~ "5.Ensino superior completo",
                                  TX_RESP_Q004 == "F" ~ NA_character_,
                                  TX_RESP_Q004 == "*" ~ NA_character_,
                                  TX_RESP_Q004 == "." ~ NA_character_))

# checar as recodificacao realizada
table(dados_1$TX_RESP_Q004) 


## melhorar nome das variaveis -----------------------------------------
# use nomes de facil compreensao

dados_1 <- dados_1 %>% 
  rename('cor_pele'=TX_RESP_Q002,
         'edu_mae'=TX_RESP_Q004)

# filtrar um estado --------------------------------------------
# dado o tamanho e diversidade do Brasil, evite trabalhar com muitos dados, especialmente se estiver começando

# veja os codigos de todos estados (note que há uma lógica nesses códigos)
browseURL('https://atendimento.tecnospeed.com.br/hc/pt-br/articles/360021494734-Tabela-de-C%C3%B3digo-de-UF-do-IBGE')

# criar objeto com os dados de Pernambuco
dados_PE <- dados_1 %>% 
  filter(ID_UF==26)

# analise exploratoria dos dados -------------------------------------

# media das variaveis numericas
mean(dados_PE$PROFICIENCIA_LP_SAEB, na.rm = TRUE)
mean(dados_PE$PROFICIENCIA_MT_SAEB, na.rm = TRUE)

# relacao entre as duas notas - correlacao
cor(dados_PE$PROFICIENCIA_LP_SAEB, dados_PE$PROFICIENCIA_MT_SAEB, use = 'pair')
# relacao entre as duas notas - visualizacao
dados_PE %>%
  ggplot( aes(PROFICIENCIA_LP_SAEB, PROFICIENCIA_MT_SAEB) )+ # define as variaveis do grafico
  geom_point(color='gray')+ # adiciona os pontos
  geom_smooth()+            # adiciona a linha de tendencia
  theme_bw()  # define o layout 


## tabulacao da cor da pele (varios metodos) --------------------------------

# metodo 1
table(dados_PE$cor_pele)

# metodo 2
table(dados_PE$cor_pele, useNA = 'if')

# metodo 3
table(dados_PE$cor_pele, useNA = 'if') %>% 
  prop.table() %>% 
  round(2)

# metodo 4
dados_PE %>% 
  group_by(cor_pele) %>% 
  summarise(quantidade=n()) %>% 
  mutate(percentual= round( quantidade / sum(quantidade),2 ))


# QUESTAO 2
# Qual a diferenca entre os 4 metodos de tabulacao acima?

# QUESTAO 3
# Como voce faria para saber a proporcao de alunos cuja mae tem Ensino Superior completo? Escreva o código!

# QUESTAO 4
# Como voce faria para saber a proporcao de alunos de pele preta cuja mae tem Ensino Superior completo? Escreva o código!




## Nota de matemática X escolaridade da mae ------------------------------------------------

# VISUALIZACAO da relacao entre essas duas variaveis
dados_PE %>% 
  filter(!is.na(edu_mae)) %>% # deixar apenas quem respondeu essa questao
  # aqui comeca o grafico, com o pacote ggplot
  ggplot(aes(edu_mae, PROFICIENCIA_MT_SAEB)) +
  geom_boxplot()+
  coord_flip()


# TESTE ESTATISTICO dessa relação
# para simplificar, vamos comparar apenas duas categorias
categoria1 <- dados_PE$PROFICIENCIA_LP_SAEB [dados_PE$edu_mae=='1.Nao terminou o 5ano']
categoria2 <- dados_PE$PROFICIENCIA_LP_SAEB [dados_PE$edu_mae=='2.Fundamental I completo']
# o teste T verifica se a diferença entre duas médias é significativa
t.test(categoria1, categoria2) # o p-value deve ser menor que 0.05 para ser significativo




# 2a PARTE: interface interativa para fazer graficos ---------------------

# essa linha abrira seu navegador de internet. 
# comece importando <dados_PE> e se divirta!
esquisser(viewer = 'browser')


# QUESTAO 5
# insira abaixo o codigo do grafico que voce fez. O que ele nos informa?




# QUESTAO 6
# E se você quiser fazer o mesmo grafico mas com dados do Paraná? Escreva o código!





