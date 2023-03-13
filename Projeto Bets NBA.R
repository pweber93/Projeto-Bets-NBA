# Pacotes

require(tidyr)
require(tidyverse)
require(lubridate)
require(ggplot2)
require(dplyr)

# Read csv

df <- read.csv("bet_results.csv")
View(df)
glimpse(df)


# Verifica NA

sapply(df,function(x)sum(is.na(x)))
sapply(df,function(x)sum(is.null(x)))

# Criação DF alternativo sem NA

df2 <- drop_na(df)
sapply(df2,function(x)sum(is.na(x)))

# Criação da coluna unidade (Unt)

df2$Unt <- df2$Net/100

# Gsub duplicados

df2$Name <- gsub('R.J. Barrett', 'RJ Barrett', df2$Name)
df2$Name <- gsub('C.J. McCollum', 'CJ McCollum', df2$Name)
df2$Name <- gsub('Jabari Smith Jr.', 'Jabari Smith Jr', df2$Name)
df2$Name <- gsub('Kelly Oubre Jr.', 'Kelly Oubre', df2$Name)
df2$Name <- gsub('Kevin Porter Jr.', 'Kevin Porter', df2$Name)
df2$Name <- gsub('Lonnie Walker IV', 'Lonnie Walker', df2$Name)
df2$Name <- gsub('Marcus Morris Sr.', 'Marcus Morris', df2$Name)
df2$Name <- gsub('Michael Porter Jr.', 'Michael Porter', df2$Name)
df2$Name <- gsub('Nic Claxton', 'Nicolas Claxton', df2$Name)
df2$Name <- gsub('P.J. Tucker', 'PJ Tucker', df2$Name)

# Desempenho por time

df_team <- df2 %>% select(Team, Unt) %>% group_by(Team) %>% summarise(sum(Unt))

# Desempenho por time e categoria

df_team_cat <- df2 %>% select(Team, Cat, Unt) %>% group_by(Team, Cat) %>% summarise(sum(Unt))

# Desempenho por categoria

df_cat <- df2 %>% select(Cat, Unt) %>% group_by(Cat) %>% summarise(sum(Unt))

# Desempenho por jogador

df_Name <- df2 %>% select(Name, Unt) %>% group_by(Name) %>% summarise(sum(Unt))

# Desempenho por jogador e categoria

df_Name_Cat <- df2 %>% select(Name, Cat, Unt) %>% group_by(Name, Cat) %>% summarise(sum(Unt))

# Renomear colunas

colnames(df_Name_Cat)[3] = "Unt" 
colnames(df_Name)[2] = "Unt"
colnames(df_team_cat)[3] = "Unt" 
colnames(df_team)[2] = "Unt"
colnames(df_cat)[2] = "Unt"

# Result por Cat

df2 %>% select(Cat, Result) %>% group_by(Cat) %>% count(Result)

# Melhor aproveitamento da categoria rebotes com 55,6% de win rate

# Gráficos

# Barplot Unidades por Categoria

ggplot(df_cat, aes(x = Cat, y = Unt, fill = Cat)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Unt), vjust = 1.5) +
  labs(title = "Unidades Por Categoria",
       x = "Categoria",
       y = "Unidades") +
  theme(legend.position = "none")

# Barplot Unidades por Time

ggplot(df_team, aes(x = Team, y = Unt, fill = Team)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Unt), vjust = 1.5) +
  labs(title = "Unidades Por Time",
       x = "Time",
       y = "Unidades") +
  theme(legend.position = "none")

# Função Assistência # Consulta o retrospecto das últimas 10 partidas do jogador nessa categoria

fass <- function(z){
  ca <- df2 %>% select(Date, Name, Cat, Unt) %>% 
    filter(Name == z, Cat == "Ast") 
  pg <- sum(tail(ca$Unt, 10L))
  print(pg)
  ggplot(tail(ca, 10L)) + geom_point(mapping = aes(x = Date, y = Unt), show.legend = T)
}

# Função Rebote # Consulta o retrospecto das últimas 10 partidas do jogador nessa categoria

freb <- function(z){
  ca <- df2 %>% select(Date, Name, Cat, Unt) %>% 
    filter(Name == z, Cat == "Reb") 
  pg <- sum(tail(ca$Unt, 10L))
  print(pg)
  ggplot(tail(ca, 10L)) + geom_point(mapping = aes(x = Date, y = Unt), show.legend = T)
}

# Função Pontos # Consulta o retrospecto das últimas 10 partidas do jogador nessa categoria

fpts <- function(z){
  ca <- df2 %>% select(Date, Name, Cat, Unt) %>% 
    filter(Name == z, Cat == "Pts")
  pg <- sum(tail(ca$Unt, 10L))
  print(pg)
  ggplot(tail(ca, 10L)) + geom_point(mapping = aes(x = Date, y = Unt), show.legend = T)
}







