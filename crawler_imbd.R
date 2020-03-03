install.packages("rvest")
install.packages("xml2")

library(rvest)
library(xml2)
library(dplyr)

lego_movie <- read_html("http://www.imdb.com/title/tt1490017/") #cria o objeto com a função read_html do site em questão

html_nodes(lego_movie, "strong span") #converte para arquivo xml, uma vez que é mais fácl de fazer procuras
  #{xml_nodeset (1)} 1 = encontrou o nome do nó procurado (strong span)

# Posso rodar o comando acima usando o pipe do dplyr também

#nota do filme
rating <- lego_movie %>% 
  html_nodes("strong span") %>% 
  html_text() %>%  #retorna o texto do nó em questão
  as.numeric() #transforma o texto "7.8" em número 7.8.

#nome do filme
title<- lego_movie %>% 
  html_nodes("h1") %>% 
  html_text() #retorna o texto do nó em questão

data.frame ("Título" = title, "Nota" = rating)


#### Criando o Crawler ####
#cria a função crawler

crawler_imdb <-function (imdb_url){
  movie <- read_html(imdb_url) #cria um objeto
  
  #nota do filme
  rating <- movie %>% 
    html_nodes("strong span") %>% 
    html_text() %>%  #retorna o texto do nó em questão
    as.numeric() #transforma o texto "7.8" em número 7.8.
  
  #nome do filme
  title<- movie %>% 
    html_nodes("h1") %>% 
    html_text() #retorna o texto do nó em questão
  
  data.frame ("Título" = title, "Nota" = rating)
}

#Função criada do crawler

#movies info
lego<- crawler_imdb ("http://www.imdb.com/title/tt1490017/")
powerrangers<-crawler_imdb ("https://www.imdb.com/title/tt0113820/?ref_=fn_al_tt_3")
naruto<- crawler_imdb("https://www.imdb.com/title/tt3717532/?ref_=fn_al_tt_2") #ja puxa as info no objeto naruto
boruto<- crawler_imdb("https://www.imdb.com/title/tt4618398/?ref_=fn_al_tt_3")

filmes <- c("http://www.imdb.com/title/tt1490017/", #criando o objeto da lista de links a serem extraidas as info
            "https://www.imdb.com/title/tt0113820/?ref_=fn_al_tt_3",
            "https://www.imdb.com/title/tt7286456/?ref_=fn_al_tt_1",
            "https://www.imdb.com/title/tt4618398/?ref_=fn_al_tt_3")

for (i in 1:2){ # função para colar coisas em cima de outras que já existem.
  #i = "objeto" que tô discriminando com quantidade de linhas
  crawler_imdb((filmes[i])) #usando a função criada (crawler_imdb) para extrair as infos diretamente dos links em "filmes"
}

crawler_imdb((filmes[1])) # o número 1 =  Já puxa as informações do primeiro link
crawler_imdb((filmes[2])) # o número 2 =  Já puxa as informações do primeiro link

### juntando vários links numa mesma tabela ####
# primeiro tem que criar uma tabela inicial
tabela <-data.frame()


for (i in 1:length(filmes)){# length(filmes) = a tabela vai ter o tamanho do objeto "filmes"
  tabela <- tabela %>% #coloca dentro dessa tabela vazia previamente criada os links dentro de filmes
    rbind(crawler_imdb(filmes[i]))
}
tabela #já me dá uma tabela das informações com os links que eu colocar em "filmes"

#Toda vez que eu quiser um filme novo, adiciono o link no objeto "filmes".

##Agora, cria a função para fazer isso automaticamente

title_ratings_imdbR <- function (filmes){
  tabela <- data.frame() #tabela em branco
  for (i in 1:length(filmes)){# length(filmes) = a tabela vai ter o tamanho do objeto "filmes"
  tabela <- tabela %>% #coloca dentro dessa tabela vazia previamente criada os links dentro de filmes
    rbind(crawler_imdb(filmes[i]))
  }
  tabela
}
#função criada

title_ratings_imdbR(filmes) #aplicando no objeto filmes com lista de links