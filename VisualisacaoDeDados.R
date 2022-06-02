#Pacotes
{
library(tidyverse)
library(lubridate)
library(ggthemes)
library(ggpubr)
library(readr)
}
#Lendo a base
{
urlfile<-'https://raw.githubusercontent.com/Gabriel4210/FirstShiny/main/netflix_titles.csv'
banco<-read.csv(urlfile)
}
#Começando o tratamento dos dados
{
  summary(banco)
banco[,2]=as.factor(banco[,2])
banco[,3]=as.factor(banco[,3])
banco[,9]=as.factor(banco[,9])
banco[,7]=mdy(banco[,7])
banco$ndesc=nchar(banco[,12])
}
# Splitando as strings
{
max(str_count(banco$country, ','))
max(str_count(banco$director, ','))
max(str_count(banco$cast, ','))
max(str_count(banco$listed_in, ','))

ctr=banco%>%
  separate(country, into = c('a','b','c','d','e','f','g','h','i','j','k','l')
           ,", ", convert = TRUE)

ctr<-ctr[,6:17]

ctr_list<-ctr%>%
  unlist()

ctr_tibble<-tibble(
  country_name=ctr_list
)

ctr=banco%>%
  separate(director, into = c('a','b','c','d','e','f','g','h','i','j','k','l','m')
           ,", ", convert = TRUE)

ctr<-ctr[,4:16]

ctr_list2<-ctr%>%
  unlist()

ctr_tibble2<-tibble(
  director_name=ctr_list2
)


ctr=banco%>%
  separate(cast, into = c(as.character(1:50))
           ,", ", convert = TRUE)

ctr<-ctr[,5:54]

ctr_list3<-ctr%>%
  unlist()

ctr_tibble3<-tibble(
  cast_name=ctr_list3
)

ctr=banco%>%
  separate(listed_in, into = c('a','b','c')
           ,", ", convert = TRUE)

ctr<-ctr[,11:13]

ctr_list4<-ctr%>%
  unlist()

ctr_tibble4<-tibble(
  genero=ctr_list4
)

ctr=banco%>%
  separate(country, into = c('a','b','c','d','e','f','g','h','i','j','k','l')
           ,", ", convert = TRUE)
ctr<-ctr[,c(6:17,22)]


ctr=ctr%>%
  separate(listed_in, into = c('Ga','Gb','Gc')
           ,", ", convert = TRUE)

ctrn1= ctr %>% filter(a == 'United States'|b == 'United States'|c == 'United States'|d == 'United States'|e == 'United States'|f == 'United States'|g == 'United States'|h == 'United States'|i == 'United States'|j == 'United States'|k == 'United States'|l == 'United States')

ctrn2= ctr %>% filter(a == 'India'|b == 'India'|c == 'India'|d == 'India'|e == 'India'|f == 'India'|g == 'India'|h == 'India'|i == 'India'|j == 'India'|k == 'India'|l == 'India')

ctrn1= ctrn1[13:15]
ctrn2= ctrn2[13:15]

ctr_listn1<-ctrn1%>%
  unlist()

ctr_tibblen1<-tibble(
  genero=ctr_listn1)
  
ctr_listn2<-ctrn2%>%
    unlist()
  
ctr_tibblen2<-tibble(
    genero=ctr_listn2)

}
#Contando
{
ctr1 = ctr_tibble%>%
  group_by(country_name)%>%
  count()%>%
  filter(!is.na(country_name))

ctr2 = ctr_tibble2%>%
  group_by(director_name)%>%
  count()%>%
  filter(!is.na(director_name))

ctr3 = ctr_tibble3%>%
  group_by(cast_name)%>%
  count()%>%
  filter(!is.na(cast_name))

ctr4 = ctr_tibble4%>%
  group_by(genero)%>%
  count()%>%
  filter(!is.na(genero))

ctrn1 = ctr_tibblen1%>%
  group_by(genero)%>%
  count()%>%
  filter(!is.na(genero))

ctrn2 = ctr_tibblen2%>%
  group_by(genero)%>%
  count()%>%
  filter(!is.na(genero))
}
#Fazendo os primeiros gráficos
{
  #Número de obras por país
  plot1 =ctr1%>%
    filter(n>100 && country_name != '')%>%
    ggplot(aes(n, reorder(country_name, FUN=median, n)))+
    geom_bar(stat='identity', show.legend = F, colour="blue")+
    labs(
      x='Número de obras na Netflix',
      y='País',
      title='Países com mais obras na Netflix',
      caption = "Fonte: ")+
      theme_economist()
  
  #Principais diretores na netflix
  plot2 =ctr2%>%
    filter(n>10 && director_name != '')%>%
    ggplot(aes(n, reorder(director_name, FUN=median, n)))+
    geom_bar(stat='identity', show.legend = F, colour="blue")+
    labs(
      x='Número de obras na Netflix',
      y='Diretor',
      title='Diretores com mais obras na Netflix',
      caption = "Fonte: ")+
    theme_economist() 

 
  #Principais atores na netflix
  plot3 =ctr3%>%
    filter(n>25 && cast_name != '')%>%
    ggplot(aes(n, reorder(cast_name, FUN=median, n)))+
    geom_bar(stat='identity', show.legend = F, colour="blue")+
    labs(
      x='Número de obras na Netflix',
      y='Ator',
      title='Atores com mais obras na Netflix',
      caption = "Fonte: ")+
    theme_economist() 

  #Gêneros mais populares na Netflix
  plot4 =ctr4%>%
    filter(n>300 && genero != '')%>%
    ggplot(aes(n, reorder(genero, FUN=median, n)))+
    geom_bar(stat='identity', show.legend = F, colour="blue")+
    labs(
      x='Número de obras na Netflix',
      y='Gêneros',
      title='Gêneros mais populares na Netflix',
      caption = "Fonte: ")+
    theme_economist() 
  
  #Contagem de filmes e séries produzidos por ano
  plot5=banco%>%
    group_by(date_added, type)%>%
    summarise(n=n())%>%
    ggplot(aes(date_added))+
    geom_freqpoly(aes(color=type), size = 1.3)+
    theme_economist()+
    labs(
      x='Ano',
      y='Quantidade',
      title='Filmes e séries produzidos por ano')+
    scale_fill_manual(values = c("Movie" = "#2998CC","TV Show" = "#5229CC"))
  #Proporção de filmes e séries produzidos por ano
  plot6=banco%>%
    group_by(date_added, type)%>%
    summarise(n=n())%>%
    ggplot(aes(date_added, color=type))+
    geom_density(aes(fill=type), alpha=1/10, size=1.3)+
    theme_economist()+
    labs(
      x='Ano',
      y='Proporção',
      title='Proporção de filmes e séries produzidos por ano')+
    scale_fill_manual(values = c("Movie" = "#2998CC","TV Show" = "#5229CC"))
  plot6
  #Porcentagem de filmes e séries presentes no catalogo
  Film_Types = banco %>% group_by(type) %>% count() %>% ungroup() %>% 
    mutate(perc = `n` / sum(`n`)) %>% 
    arrange(perc) %>%
    mutate(labels = scales::percent(perc))
  
  Film_Types$ymax = cumsum(Film_Types$perc)
  Film_Types$ymin = c(0, head(Film_Types$ymax, n= -1))
  
  Film_Types$labelPosition <- (Film_Types$ymax + Film_Types$ymin) / 2
  Film_Types$label <- paste0(Film_Types$type, "\n Porcentagem: ", round(Film_Types$perc, digits = 3))
  
  plot8= ggplot(Film_Types, aes(ymax = ymax, ymin = ymin, xmax= 4, xmin = 3, fill = type)) +
    geom_rect() +
    geom_label(x=3.5, aes(y=labelPosition, label=label), size=4.5)+
    coord_polar(theta = "y") +
    labs(title="Porcentagem de filmes e séries presentes no catálogo") +
    theme_void() +
    scale_fill_manual(values = c("Movie" = "#2998CC","TV Show" = "#5229CC"))+
    xlim(c(2,4))+
    theme(legend.position = "none")
  
  }
#Aprofundando os gráficos
{

  #Porcentagem da quantidade de filmes dirigidos por número de diretores
  ctr5 = rename(ctr2, num = n)
  Diretor_num = ctr5 %>% filter(director_name != '') %>% group_by(num) %>% count() %>% ungroup() %>% 
    mutate(perc = `n` / sum(`n`)) %>% 
    arrange(perc) %>%
    mutate(labels = scales::percent(perc))
  
  Diretor_num$ymax = cumsum(Diretor_num$perc)
  Diretor_num$ymin = c(0, head(Diretor_num$ymax, n= -1))
  
  Diretor_num$labelPosition <- (Diretor_num$ymax + Diretor_num$ymin) / 2
  Diretor_num$label <- paste0(Diretor_num$num, "\n Porcentagem: ", round(Diretor_num$perc, digits = 3),"%")
  
  plot9= ggplot(Diretor_num, aes(ymax = ymax, ymin = ymin, xmax= 4, xmin = 3, fill = n)) +
    geom_rect() +
    geom_label(x=3.5, aes(y=labelPosition, label=Diretor_num$label), size=4.5)+
    coord_polar(theta = "y") +
    labs(title="Porcentagem da quantidade de filmes dirigidos por número de diretores") +
    theme_void() +
    xlim(c(2,4))+
    theme(legend.position = "none")
  
  #Porcentagem do número de participações de membro do elenco por número de membros 
  ctr6 = rename(ctr3, num = n)
  Cast_num = ctr6 %>% filter(cast_name != '') %>% group_by(num) %>% count() %>% ungroup() %>% 
    mutate(perc = `n` / sum(`n`)) %>% 
    arrange(perc) %>%
    mutate(labels = scales::percent(perc))
  
  Cast_num$ymax = cumsum(Cast_num$perc)
  Cast_num$ymin = c(0, head(Cast_num$ymax, n= -1))
  
  Cast_num$labelPosition <- (Cast_num$ymax + Cast_num$ymin) / 2
  Cast_num$label <- paste0(Cast_num$num, "\n Porcentagem: ", round(Cast_num$perc, digits = 3),"%")
  
  plot10= ggplot(Cast_num, aes(ymax = ymax, ymin = ymin, xmax= 4, xmin = 3, fill = n)) +
    geom_rect() +
    geom_label(x=3.5, aes(y=labelPosition, label=Cast_num$label), size=4.5)+
    coord_polar(theta = "y") +
    labs(title="Porcentagem do número de participações de membro do elenco por número de membros") +
    theme_void() +
    xlim(c(2,4))+
    theme(legend.position = "none")
  plot10
  
#Pincipais generos na america
  plot11 =ctrn1%>%
    filter(num>100)%>%
    ggplot(aes(num, reorder(genero, FUN=median, num)))+
    geom_bar(stat='identity', show.legend = F, colour="red")+
    labs(
      x='Número de obras na Netflix',
      y='Gênero',
      title='Principais Gêneros produzidos nos Estados Unidos',
      caption = "Fonte: ")+
    theme_economist()

  
#Principais generos na india
  plot12 =ctrn2%>%
    filter(n>100)%>%
    ggplot(aes(n, reorder(genero, FUN=median, n)))+
    geom_bar(stat='identity', show.legend = F, colour="orange")+
    labs(
      x='Número de obras na Netflix',
      y='Gênero',
      title='Principais Gêneros produzidos na India',
      caption = "Fonte: ")+
    theme_economist()
}
# Shiny
{
ui <- dashboardPage(skin = "red",
                    
                    dashboardHeader(title = "Catálogo da Netflix",
                                    titleWidth = 250),
                    
                    dashboardSidebar(
                      width = 250,
                      sidebarMenu(
                        menuItem("Contexto", tabName = "item1"),
                        menuItem("Sobre Filmes e Séries", tabName = "item2"),
                        menuItem("Sobre Atores e Diretores", tabName = "item3"),
                        menuItem("Sobre Países e Gêneros", tabName = "item4")
                      )),
                    
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "item1",
                                mainPanel(
                                  box(status = "danger", 
                                      width = 50,
                                      height = 400,
                                      solidHeader = T,
                                      title = "Motivação do estudo",
                                      h2("Tendo em vista o crescimento do uso de plataformas de streaming nos últimos anos, é interessante observar qual o perfil geral do conteúdo que é disponibilizado em uma das principais plataformas da atualidade")),
                                  br(),
                                  box(width = 50,
                                      height = 700,
                                      solidHeader = F, status = "info",
                                      plotOutput("ex_a")
                                  )
                                )),
                        tabItem(tabName = "item2",
                                fluidRow(
                                  box(width = 50,
                                      height = 700,
                                      solidHeader = F, status = "info",
                                      plotOutput("ex_b")
                                  )
                                )),
                        tabItem(tabName = "item3",
                                fluidRow(
                                  box(width = 500,
                                      height = 700,
                                      solidHeader = F, status = "info",
                                      plotOutput("ex_c"),
                                      br(),
                                      plotOutput("ex_e")
                                  )
                                )),
                        tabItem(tabName = "item4",
                                fluidRow(
                                  box(width = 50,
                                      height = 700,
                                      solidHeader = F, status = "info",
                                      plotOutput("ex_d"),
                                      br(),
                                      plotOutput("ex_f")
                                  )
                                ))
                      )
                    )
)


server <- function(input, output) { 
  
  output$ex_a <- renderPlot({
       ggplot(Film_Types, aes(ymax = ymax, ymin = ymin, xmax= 4, xmin = 3, fill = type)) +
      geom_rect() +
      geom_label(x=3.5, aes(y=labelPosition, label=label), size=4.5)+
      coord_polar(theta = "y") +
      labs(title="Porcentagem de filmes e séries presentes no catálogo") +
      theme_void() +
      scale_fill_manual(values = c("Movie" = "#2998CC","TV Show" = "#5229CC"))+
      xlim(c(2,4))+
      theme(legend.position = "none")
    
    
  })
  
  output$ex_b <- renderPlot({
    bp3 <- banco%>%
      group_by(date_added, type)%>%
      summarise(n=n())%>%
      ggplot(aes(date_added))+
      geom_freqpoly(aes(color=type), size = 1.3)+
      theme_economist()+
      labs(
        x='Ano',
        y='Quantidade',
        title='Filmes e séries produzidos por ano')+
      scale_fill_manual(values = c("Movie" = "#2998CC","TV Show" = "#5229CC"))
    
    
    bp4 <- banco%>%
      group_by(date_added, type)%>%
      summarise(n=n())%>%
      ggplot(aes(date_added, color=type))+
      geom_density(aes(fill=type), alpha=1/10, size=1.3)+
      theme_economist()+
      labs(
        x='Ano',
        y='Proporção',
        title='Proporção de filmes e séries produzidos por ano')+
      scale_fill_manual(values = c("Movie" = "#2998CC","TV Show" = "#5229CC"))
    
    gg2 <- ggarrange(bp3, bp4, labels = c("", ""), ncol = 2, nrow = 1)
    annotate_figure(gg2, top = text_grob("Comparação de produção de filmes e séries por ano", color = "black", face = "bold", size = 20))
    
  })
  
  output$ex_c <- renderPlot({
    bp5 <- ctr3%>%
      filter(n>25 && cast_name != '')%>%
      ggplot(aes(n, reorder(cast_name, FUN=median, n)))+
      geom_bar(stat='identity', show.legend = F, colour="blue")+
      labs(
        x='Número de obras na Netflix',
        y='Ator',
        title='Atores com mais obras na Netflix',
        caption = "Fonte: ")+
      theme_economist() 
    
    
    bp6 <- ctr2%>%
      filter(n>10 && director_name != '')%>%
      ggplot(aes(n, reorder(director_name, FUN=median, n)))+
      geom_bar(stat='identity', show.legend = F, colour="blue")+
      labs(
        x='Número de obras na Netflix',
        y='Diretor',
        title='Diretores com mais obras na Netflix',
        caption = "Fonte: ")+
      theme_economist() 
    
    gg3 <- ggarrange(bp5, bp6, ncol = 2, nrow = 1)
    annotate_figure(gg3, top = text_grob("Atores e Diretores com mais obras no catálogo", color = "black", face = "bold", size = 20))
    

    
  })
  
  output$ex_d <- renderPlot({
    bp7 <- ctr1%>%
      filter(n>100 && country_name != '')%>%
      ggplot(aes(n, reorder(country_name, FUN=median, n)))+
      geom_bar(stat='identity', show.legend = F, colour="blue")+
      labs(
        x='Número de obras na Netflix',
        y='País',
        title='Países com mais obras na Netflix',
        caption = "Fonte: ")+
      theme_economist() 
    
    
    bp8 <- ctr4%>%
      filter(n>300 && genero != '')%>%
      ggplot(aes(n, reorder(genero, FUN=median, n)))+
      geom_bar(stat='identity', show.legend = F, colour="blue")+
      labs(
        x='Número de obras na Netflix',
        y='Gêneros',
        title='Gêneros mais populares na Netflix',
        caption = "Fonte: ")+
      theme_economist()  
    
    gg4 <- ggarrange(bp7, bp8, labels = c("", ""), ncol = 2, nrow = 1)
    annotate_figure(gg4, top = text_grob("Países e gêneros mais populares", color = "black", face = "bold", size = 20))
    
  })
  
  output$ex_e <- renderPlot({
    bp9 <- ggplot(Diretor_num, aes(ymax = ymax, ymin = ymin, xmax= 4, xmin = 3, fill = n)) +
      geom_rect() +
      geom_label(x=3.5, aes(y=labelPosition, label=Diretor_num$label), size=4.5)+
      coord_polar(theta = "y") +
      labs(title="Porcentagem da quantidade de filmes dirigidos por número de diretores") +
      theme_void() +
      xlim(c(2,4))+
      theme(legend.position = "none")
    
    bp10 <-ggplot(Cast_num, aes(ymax = ymax, ymin = ymin, xmax= 4, xmin = 3, fill = n)) +
      geom_rect() +
      geom_label(x=3.5, aes(y=labelPosition, label=Cast_num$label), size=4.5)+
      coord_polar(theta = "y") +
      labs(title="Porcentagem do número de participações de membro do elenco por número de membros") +
      theme_void() +
      xlim(c(2,4))+
      theme(legend.position = "none")
    
    gg5 <- ggarrange(bp9, bp10, ncol = 2, nrow = 1)
    annotate_figure(gg5, top = text_grob("", color = "black", face = "bold", size = 20))
  })
  
  output$ex_f <- renderPlot({
    bp11 <- ctrn1%>%
      filter(num>100)%>%
      ggplot(aes(num, reorder(genero, FUN=median, num)))+
      geom_bar(stat='identity', show.legend = F, colour="red")+
      labs(
        x='Número de obras na Netflix',
        y='Gênero',
        title='Principais Gêneros produzidos nos Estados Unidos',
        caption = "Fonte: ")+
      theme_economist()
    
    
    bp12 <- ctrn2%>%
      filter(n>100)%>%
      ggplot(aes(n, reorder(genero, FUN=median, n)))+
      geom_bar(stat='identity', show.legend = F, colour="orange")+
      labs(
        x='Número de obras na Netflix',
        y='Gênero',
        title='Principais Gêneros produzidos na India',
        caption = "Fonte: ")+
      theme_economist()
    
    gg6 <- ggarrange(bp11, bp12, labels = c("", ""), ncol = 2, nrow = 1)
    annotate_figure(gg6, top = text_grob("Principais gêneros nos EUS e India", color = "black", face = "bold", size = 20))
    
  })
  
}

runApp(shinyApp(ui, server))

}
