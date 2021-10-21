# script introdutorio ao modulo avancado


# instalação de pacotes ----------------------------

# instalar pacotes necessarios para o modulo
install.packages(c('tidyverse','data.table',"esquisse", 'rio'))

# instalacao adicional
rio::install_formats()


# exemplos de códigos R -------------------------


# carregar dados sobre flores (dados de exemplo do R)
data(iris)

# dimensoes dos dados (linha X coluna)
dim(iris)

# primeiras linhas dos dados
head(iris)

# selecionar uma variavel (coluna)
iris$Sepal.Length

# media
mean(iris$Sepal.Length)

# media arredondada
round( mean(iris$Sepal.Length) )

# tabulação (contagem)
table(iris$Species)

# plot xy
plot(x=iris$Sepal.Length, y=iris$Petal.Length)

# boxplot
boxplot(iris$Sepal.Length ~ iris$Species)

# criar objeto
varivael.qualquer.nome <- iris$Sepal.Length / iris$Petal.Length

# histograma com objeto
hist(varivael.qualquer.nome)

