require(tidyverse)
require(dplyr)
require(ggplot2)
require(shiny)
require(readr)
require(lubridate)
require(stringr)
require(ggwordcloud)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
require(tidytext)
require(SentimentAnalysis)




dadostot <- read_csv("reddit_depression_dataset.csv")


NAToEmpty <- function(value){
  if (is.na(value)){
    return('')
  } else{
    return(value)
  }
}

BoolToString <- function(value){
  if(value){
    return('TRUE')
  } else{
    return('FALSE')
  }
}

dados <- dadostot

NAToZero <- function(value){
  if (is.na(value)){
    return(0)
  } else{
    return(value)
  }
}

NegativeToZero <- function(value){
  if (value<0){
    return(0)
  } else{
    return(value)
  }
}


toBool <- function(value){
  if (value == 1){
    return(TRUE)
  } else if (value == 0){
    return(FALSE)
  }
}


dados %>%
  filter(num_comments == -0)



dados <- dados %>%
  mutate(date = format(as_datetime(created_utc), "%Y-%m-%d"))



meses_portugues <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", 
                     "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")


dados_tratados <- dados %>%
  mutate(num_comments = sapply(num_comments, NAToZero)) %>%
  mutate(body = sapply(body, NAToEmpty)) %>%
  mutate(labelbool = sapply(label, toBool)) %>%
  mutate(ano = format(as_datetime(created_utc), "%Y")) %>%
  mutate(bodylen = str_length(body)) %>%
  mutate(titlelen = str_length(title)) %>%
  mutate(num_comments = sapply(num_comments, NegativeToZero)) %>%
  mutate(mes = factor(month(date),  
                      levels = 1:12, 
                      labels = meses_portugues))





ui <- navbarPage(
  shinyWidgets::useShinydashboard(),
  
  title = "ANÁLISE DE SENTIMENTOS DE SUBREDDITS",
  tabPanel(
    "Gráfico 1", icon = icon("home"),
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width = 4,
          sliderInput("yearmin",
                      "Ano inicial do gráfico:",
                      min = 2008,
                      max = 2021,
                      value = 2008),
          sliderInput('yearmax', 
                      'Ano final do gráfico:',
                      min = 2009,
                      max = 2022,
                      value = 2022),
          checkboxGroupInput('subreddits',
                             'Subreddits que vão ser utilizados no gráfico:',
                             choices = c('depression','teenagers','SuicideWatch','happy','DeepThoughts'),
                             selected = c('SuicideWatch','happy','teenagers','DeepThoughts','depression')),
          radioButtons('bodyortitle',
                       'Desenhar comprimento do body ou title do post:',
                       choices = c('body','title'),
                       selected = 'body'),
          radioButtons('suborlabel',
                       'Agrupar por Subreddit ou por Label (subreddit relacionado a depressão ou não):',
                       choices = c('subreddit','label'),
                       selected = 'subreddit')
        ),
        
        mainPanel(
          fluidRow(
            plotOutput('line')
          )
        )
      )
    )
  ),
  tabPanel(
    'Gráfico2', icon = icon('home'),
    fluidPage( 
      sidebarLayout(
        sidebarPanel(
          width = 4,
          selectInput("subreddit_filter", 
                      label = "Escolha o Subreddit", 
                      choices = c("Todos", unique(dados_tratados$subreddit)),
                      selected = "Todos"),
          
          dateRangeInput("data_filter", 
                         label = "Escolha o Intervalo de Data", 
                         start = min(dados_tratados$date, na.rm = TRUE), 
                         end = max(dados_tratados$date, na.rm = TRUE),
                         format = "yyyy-mm-dd"),
          
          sliderInput("upvotes_filter", 
                      label = "Selecione o Intervalo de Upvotes", 
                      min = min(dados_tratados$upvotes, na.rm = TRUE), 
                      max = max(dados_tratados$upvotes, na.rm = TRUE),
                      value = c(min(dados_tratados$upvotes, na.rm = TRUE), max(dados_tratados$upvotes, na.rm = TRUE))),
          
          sliderInput("num_comments_filter", 
                      label = "Selecione o Intervalo de Comentários", 
                      min = min(dados_tratados$num_comments, na.rm = TRUE), 
                      max = max(dados_tratados$num_comments, na.rm = TRUE),
                      value = c(min(dados_tratados$num_comments, na.rm = TRUE), max(dados_tratados$num_comments, na.rm = TRUE))),
          
          textInput("title_filter", 
                    label = "Pesquise por palavra no título", 
                    value = "")
        ),
        mainPanel(
          h3("Gráfico Interativo de Comentários"),
          plotOutput("grafico_comentarios", height = "600px"),  
          width = 9 
        )
      ),
      
      tags$head(
        tags$style(HTML("
      body {
        background-color: #f4f4f9;
        font-family: 'Arial', sans-serif;
      }
      .container-fluid {
        padding: 20px;
      }
      .navbar, .sidebar {
        background-color: #34495e;
      }
      .navbar-header, .sidebar-header {
        color: white;
      }
      .sidebarPanel, .mainPanel {
        padding: 15px;
      }
    ")
        )
      )
    )
  ),
  tabPanel(
    'Gráfico3', icon = icon('home'),
    fluidPage( 
      sidebarLayout(
        sidebarPanel(
          width = 4,
          selectInput("ano_selecionado", 
                      label = "Escolha o ano:", 
                      choices = sort(unique(year(dados$date)), decreasing = TRUE),  
                      selected = sort(unique(year(dados$date)), decreasing = TRUE)[1]),  
          
          checkboxGroupInput("meses_selecionados", 
                             label = "Escolha os Meses:", 
                             choices = c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", 
                                         "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"), 
                             selected = c("Janeiro", "Fevereiro", "Março", "Abril", "Maio")), 
          
          checkboxGroupInput("label_selecionado", 
                             label = "Escolha o tipo de subreddit:", 
                             choices = c("Depressivo" = TRUE, "Não Depressivo" = FALSE), 
                             selected = c(TRUE, FALSE)),  
          
          selectInput("metrica_selecionada", 
                      label = "Escolha a métrica:", 
                      choices = c("Média de Upvotes" = "upvotes", 
                                  "Média de Comentários" = "num_comments"), 
                      selected = "upvotes")
        ),
        mainPanel(
          plotOutput("barplot"),
          textOutput("correlation_text")
        )
      )
    )
  ),
  tabPanel(
    'Gráfico4', icon = icon('home'),
    fluidPage( 
      sidebarLayout(
        sidebarPanel(
          width = 4,
          selectInput("var_x", 
                      label = "Escolha a variável para o eixo X", 
                      choices = c("Upvotes" = "upvotes", 
                                  "Número de Comentários" = "num_comments", 
                                  "Número de Palavras no Título" = "titlelen", 
                                  "Número de Palavras no Body" = "bodylen")),
          
          selectInput("var_y", 
                      label = "Escolha a variável para o eixo Y", 
                      choices = c("Upvotes" = "upvotes", 
                                  "Número de Comentários" = "num_comments", 
                                  "Número de Palavras no Título" = "titlelen", 
                                  "Número de Palavras no Body" = "bodylen")),
          
          dateRangeInput("data_range", 
                         label = "Escolha o intervalo de datas", 
                         start = min(dados$date),  
                         end = max(dados$date),    
                         min = min(dados$date),    
                         max = max(dados$date),    
                         format = "dd/mm/yyyy"),   
          
          selectInput("subreddit", 
                      label = "Escolha o Subreddit", 
                      choices = c("Todos" = "Todos", unique(dados$subreddit)), 
                      selected = "Todos"),
          
          actionButton("calc_cor", "Calcular Correlação")
        ),
        mainPanel(
          plotOutput('scatter')
          
        )
      )
    )
  ),
  tabPanel(
    'Gráfico5', icon = icon('home'),
    fluidPage( 
      sidebarLayout(
        sidebarPanel(
          width = 4,
          
        ),
        mainPanel(
          
        )
      )
    )
  )
)



server <- function(input, output){
  
  
  
  
  output$line <- renderPlot({
    
    
    if(input$suborlabel == 'subreddit'){
      
      dados_subyear <- dados_tratados
      
      
      
      yearmin <- input$yearmin
      yearmax <- input$yearmax
      
      if (yearmin>=yearmax){
        yearmin <- 2008
        yearmax <- 2022
      }
      
      dados_subyear <- dados_subyear %>%
        filter(ano >= yearmin) %>%
        filter(ano <= yearmax)
      
      
      g1 <- ggplot(dados_subyear, aes(x = Year, y = meanvalue, group = 1))+
        theme_classic()
      
      colorscale <- c('depression' = 'blue', 'happy' = 'yellow', 'SuicideWatch' = 'black', 'teenagers' = 'red', 'DeepThoughts' = 'orange')
      
      subreddits <- input$subreddits
      
      if (input$bodyortitle == 'body'){
        type <- "bodylen"
      } else{
        type <- "titlelen"
      }
      
      
      
      
      dados_subyear <- dados_subyear %>%
        filter(subreddit %in% subreddits) %>%
        group_by(subreddit,ano) %>%
        summarise(meanvalue = mean(get(type)))
      
      newscale <- c()
      for(i in subreddits){
        datas <- dados_subyear %>% 
          filter(subreddit == i)
        newscale <- append(newscale, colorscale[i])
        
        
        
        g1 <- g1+geom_line(data = datas, mapping = aes(x = ano, y = meanvalue, color = subreddit))
      }
      
      
      g1+scale_color_manual(values = newscale)
    } else{
      dados_subyear <- dados_tratados
      
      
      
      yearmin <- input$yearmin
      yearmax <- input$yearmax
      
      
      if (yearmin>=yearmax){
        yearmin <- 2008
        yearmax <- 2022
      }
      
      dados_subyear <- dados_subyear %>%
        filter(ano >= yearmin) %>%
        filter(ano <= yearmax) %>%
        mutate(label = toString(label))
      
      colorscale <- c('non-depressed' = 'green', 'depressed' = 'black')
      
      
      
      if (input$bodyortitle == 'body'){
        type <- "bodylen"
      } else{
        type <- "titlelen"
      }
      
      datas <- dados_tratados %>%
        group_by(label, ano) %>%
        summarise(meanvalue = mean(get(type))) %>%
        select(ano, label, meanvalue)
      
      datas_0 <- datas %>%
        filter(label == '0')
      
      datas_1 <- datas %>%
        filter(label == '1')
      
      datas_0 <- datas_0 %>%
        mutate(label = 'non-depressed')
      
      datas_1 <- datas_1 %>%
        mutate(label = 'depressed')
      
      {
        g1 <- ggplot(datas, aes(x = ano, y = meanvalue, group = 1))+
          theme_classic()
        g1 <- g1+geom_line(data = datas_0, mapping = aes(x = ano, y = meanvalue, color = label))
        g1 <- g1+geom_line(data = datas_1, mapping = aes(x = ano, y = meanvalue, color = label))
        g1 <- g1+scale_color_manual(values = colorscale)
        
        }
      
      
      
      
      
      
      
      
    }
    
    g1
    
  })
  
  
  
  output$scatter <- renderPlot({
    stat1 <- input$var_x
    stat2 <- input$var_y
    
    
    if (input$subreddit == 'Todos'){
      subs <- c('depression','teenagers','SuicideWatch','happy','DeepThoughts')
    } else{
      subs <- input$subreddit
    }
    
    
    dados <- dados_tratados %>%
      filter(date >= input$data_range[1], 
             date <= input$data_range[2]) %>%
      filter(subreddit %in% subs)
    
    dados_stat1 <- dados %>%
      select(stat1)
    
    dados_stat2 <- dados %>%
      select(stat2)
    
    dados_scatter <- cbind(dados_stat1, dados_stat2)
    colnames(dados_scatter) <- c('stat1','stat2')
    
    ggplot(dados_scatter, aes(x = stat1, y = stat2))+
      geom_point()+
      xlab(stat1)+
      ylab(stat2)+
      theme_classic()+
      theme(axis.text = element_text(face = 'bold', size = 20))
    
    
  })
  
  
  
  dados_filtrados <- reactive({
    dados <- dados_tratados
    
    if (input$subreddit_filter != "Todos") {
      dados <- dados %>% filter(subreddit == input$subreddit_filter)
    }
    
    dados <- dados %>% 
      filter(date >= input$data_filter[1] & date <= input$data_filter[2]) %>%
      filter(upvotes >= input$upvotes_filter[1] & upvotes <= input$upvotes_filter[2]) %>%
      filter(num_comments >= input$num_comments_filter[1] & num_comments <= input$num_comments_filter[2])
    
    if (input$title_filter != "") {
      dados <- dados %>% filter(grepl(input$title_filter, title, ignore.case = TRUE))
    }
    
    return(dados)
  })
  
  output$grafico_comentarios <- renderPlot({
    dados_para_grafico <- dados_filtrados()
    
    ggplot(dados_para_grafico, aes(x = as.Date(date), y = num_comments)) +
      geom_line(color = "#3498db", size = 1.2) +  
      labs(title = paste("Evolução de Comentários - Subreddit:", input$subreddit_filter),
           x = "Data", y = "Número de Comentários") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, color = "#34495e", size = 10),  
        axis.text.y = element_text(color = "#34495e", size = 10),  
        axis.title = element_text(color = "#34495e", size = 12),  
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "#2c3e50"),  
        panel.grid.major = element_line(color = "#ecf0f1", size = 0.5),  
        panel.grid.minor = element_line(color = "#ecf0f1", size = 0.25)  
      )
  })
  
  meses_portugues <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", 
                       "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
  
  dados_processados <- reactive({
    dados_filtrados <- dados_tratados %>%  
      filter(ano == input$ano_selecionado) %>%  
      filter(mes %in% input$meses_selecionados) %>%  
      filter(labelbool %in% input$label_selecionado) %>%  
      group_by(mes, labelbool) %>%  # Agrupa pelos meses e label
      summarise(media_metric = mean(get(input$metrica_selecionada), na.rm = TRUE), .groups = "drop")
    
    return(dados_filtrados)
  })
  
  output$barplot <- renderPlot({
    metric_name <- ifelse(input$metrica_selecionada == "upvotes", "Upvotes", "Comentários")
    
    dados_string <- dados_processados() %>%
      mutate(labelbool = sapply(labelbool, BoolToString))
    
    
    ggplot(dados_string, aes(x = mes, y = media_metric, fill = labelbool)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c('TRUE' = "#1f77b4", 'FALSE' = "#56b4e9")) +
      labs(
        fill = "Label",
        title = paste("Média de", metric_name, "por Mês - Ano:", input$ano_selecionado)
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),  
        axis.title.x = element_blank(),  
        axis.title.y = element_blank()   
      )
  })
}









shinyApp(ui = ui, server = server)

