
#instalar pacote shinydashboard
#install.packages(c("shinydashboard","GGally"))
#install.packages(c("googleVis","dplyr"))

# load the required packages
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(googleVis)
library(knitr)
library(GGally)
library(e1071)

# Cabeçalho do painel carregando o título do dashboard


AbrirArquivo <- function(path, separador) {
  
  dfArq <- read.csv(path, header = T, sep = separador)
  return(dfArq)
  
}

frmSelArq <- function(pathArq,Separdor){
  
  box(title = "Fonte de dados", width = 6
       ,status = "primary" 
       ,solidHeader = TRUE 
       ,collapsible = TRUE 
       
       ,fileInput(pathArq, 'Selecione um arquivo de dados', accept=c('.csv'))
       
       ,radioButtons(Separdor, "Separador dos campos", inline = TRUE, c(Virgula = ",", ponto_e_virgula = ";"))
       
  )
  
  
}

dbHeader <- dashboardHeader(title = "Exploratory data analysis ",
                            tags$li(a(href = '',
                                      "Usuário", target = "_blank"),
                                    class = "dropdown",
                                    tags$style(".main-header {max-height: 58px}"),
                                    tags$style(".main-header .logo {height: 60px}")
                            ))

dSiderBar <- dashboardSidebar(
  sidebarMenu( 
    id = "tabs",
    menuItem("Página Inicial", tabName = "fb", icon = icon("home")),
    menuItem("Análise Exploratória", tabName = "ae", icon = icon("line-chart")),

     menuItem("Algorítmos de Regressão", tabName = "al", icon = icon("line-chart"),
              menuSubItem("Regressão Linear", icon = icon("check-circle"),tabName = "RegLinear")
     ),
     
     
     menuItem("Algoritmos de Classificação", tabName = "cla", icon = icon("line-chart"),
              menuSubItem("Regressão Logística", icon = icon("check-circle"), tabName = "RegLog"),
              menuSubItem("SVM", icon = icon("check-circle"), tabName = "SVM"),
              menuSubItem("K-Means Clustering", icon = icon("check-circle"), tabName = "KMC"),
              menuSubItem("KNN", icon = icon("check-circle"), tabName = "KNN"),
              menuSubItem("Árvore de Decisão", icon = icon("check-circle"), tabName = "Arvore"),
              menuSubItem("Naive Bayes", icon = icon("check-circle"), tabName = "NaiveB")),
    
  
    menuItem("Scenario Builder", tabName = "sb", icon = icon("refresh")),
    menuItem("Table Builder", tabName = "tb", icon = icon("table")),
    br()
    
   # box(title ="Project upload", background ="black", status = "danger",solidHeader = TRUE, 
  #      fileInput('file1', 'Selecione um arquivo de dados', accept=c('.RData')),  
  #      width = 12),
  ###  br(),
  #  
  #  box(title ="Scenarios Run", background ="black",  status = "danger",solidHeader = TRUE, 
  #      uiOutput("selectSB"), width = 12)
    
))
    


# combine as duas linhas de fluido para fazer o corpo
body <- dashboardBody(
  
  
  
  tabItems(
   
      tabItem(tabName = "RegLinear"
              ,box(title = "Fonte de dados", width = 6
                  ,status = "primary" 
                  ,solidHeader = TRUE 
                  ,collapsible = TRUE 
                  
                  ,fileInput('file1', 'Selecione um arquivo de dados', accept=c('.csv'))
                  
                  ,radioButtons("sepArqRegLin", "Separador dos campos", inline = TRUE, c(Virgula = ",", ponto_e_virgula = ";"))
                  
                  
                  ,actionButton("Processar",'Processar')
                  
                  ,br()
                  ,br()
                  
              )
              ,tableOutput("frow1")
      )
      ,tabItem(tabName = "RegLog"
               ,box(title = "Fonte de dados", width = 6
                    ,status = "primary" 
                    ,solidHeader = TRUE 
                    ,collapsible = TRUE 
                    
                    ,fileInput('fileRegLog', 'Selecione um arquivo de dados', accept=c('.csv'))
                    
                    ,radioButtons("sepArqRegLog", "Separador dos campos", inline = TRUE, c(Virgula = ",", ponto_e_virgula = ";"))
                    
                    
                    ,actionButton("ProcessarRegLog",'Processar')
                    
                    ,br()
                    ,br()
                    
               )
              ,tableOutput("frow2")
      )
      ,tabItem(tabName = "SVM"
               ,box(title = "Fonte de dados", width = 6
                    ,status = "primary" 
                    ,solidHeader = TRUE 
                    ,collapsible = TRUE 
                    
                    ,fileInput('file1SVM', 'Selecione um arquivo de dados', accept=c('.csv'))
                    
                    ,radioButtons("sepArqSVM", "Separador dos campos", inline = TRUE, c(Virgula = ",", ponto_e_virgula = ";"))
                    
                    
                    ,actionButton("ProcessarSVM",'Processar SVM')
                    
                    ,br()
                    ,br()
                    
               )
                    
               
               ,tableOutput("frow3")
      )
      ,tabItem(tabName = "KMC"
               ,box(title = "Fonte de dados", width = 6
                    ,status = "primary" 
                    ,solidHeader = TRUE 
                    ,collapsible = TRUE 
                    
                    ,fileInput('file1KMC', 'Selecione um arquivo de dados', accept=c('.csv'))
                    
                    ,radioButtons("sepArqKMC", "Separador dos campos", inline = TRUE, c(Virgula = ",", ponto_e_virgula = ";"))
                    
                    
                    ,actionButton("ProcessarKMC",'Processar KMC ')
                    
                    ,br()
                    ,br()
                    
               )
               
               
               ,tableOutput("fKMC")
      )
      ,tabItem(tabName = "NaiveB"
              ,box(title = "Fonte de dados", width = 6
                   ,status = "primary" 
                   ,solidHeader = TRUE 
                   ,collapsible = TRUE 
                   
                   ,fileInput('file1NB', 'Selecione um arquivo de dados', accept=c('.csv'))
                   
                   ,radioButtons("sepArqNB", "Separador dos campos", inline = TRUE, c(Virgula = ",", ponto_e_virgula = ";"))
                   
                   
                   ,actionButton("ProcessarNB",'Processar Naives Bayes')
                   
                   ,br()
                   ,br()
                   
              )
              ,tableOutput("fNB")
      )
  )
)


## completando a parte ui com o dashboardPage
ui <- dashboardPage(
    title = 'GreatBurger',
    dbHeader, 
    dSiderBar, 
    body,
    skin='blue'

    )

## crie as funções do servidor para o dashboard 
server <- function( input, output) { 
  

  
  #Regressão Linear
  observeEvent(input$Processar, {
    
    #df <- read.csv(input$file1$datapath, header = T, sep = input$sepArqRegLin)
    df <- AbrirArquivo(input$file1$datapath, input$sepArqRegLin)
    
    df2 = df[sample(nrow(df), 20), ]
    
    output$mydf<- DT::renderDataTable({
      DT::datatable(df2[, input$show_vars, drop = FALSE])
    })
    
    mydf = renderTable(df2[, input$show_vars, drop = FALSE])
 
    #correlação
   
       output$plot_correlacao <- renderText({ p <- knitr::kable(cor(df[, input$show_vars, drop = FALSE])) })
    
     #facet_wrap
   # output$plot_Analise <- renderPlot({ p <- pairs(df[, input$show_vars, drop = FALSE]) 
    #output$plot_Analise <- renderPlot({ ggpairs(df[, input$show_vars], columns = 1:ncol(df[, input$show_vars]), ggplot2::aes(colour=Species)) 
     output$plot_Analise <- renderPlot({ ggpairs(df[, input$show_vars], columns = 1:ncol(df[, input$show_vars]),cardinality_threshold = 50) 
    
  #  output$plot_Analise <- renderPlot({ p <- ggpairs(df[, input$show_vars, drop = FALSE], 
  #                                                   columns = 1:ncol(df[, input$show_vars]), 
  #                                                   ggplot2::aes(colour=Species))
      
      #GGally::ggpairs(df) 
    

      #ggpairs(df, mapping = NULL, columns = 1:ncol(df), title = "Analise de Dados",
      #        upper = list(continuous = "cor", combo = "box_no_facet", discrete ="facetbar", na = "na"), 
      #        lower = list(continuous = "points", combo = "facethist", discrete = "facetbar", na = "na"), 
      #        diag = list(continuous ="densityDiag", discrete = "barDiag", na = "naDiag"),
      #        xlab = NULL, ylab = NULL, axisLabels = c("show", "internal", "none"),
      #        columnLabels = names(df), labeller = "label_value",
      #        switch = NULL, showStrips = NULL, legend = NULL,
      #        cardinality_threshold = NULL, progress = NULL
      #        )
  })
    output$summario_dataset = renderDataTable(summary(df[, input$show_vars, drop = FALSE],digits = 6))
    
    
    output$frow1 <- renderUI({ fluidRow(

        box(title = "Dados", width = 12
            ,status = "primary" 
            ,solidHeader = TRUE 
            ,collapsible = TRUE 
            
            
            ,mainPanel(
              tabsetPanel(type = "tabs", 
                          tabPanel("Dados",
                                   column(3,
                                          h5("Colunas:"),
                                          checkboxGroupInput("show_vars", "", names(df), selected = names(df))),
                                   column(9,
                                          h5("Dados"), 
                                          DT::dataTableOutput("mydf"))
                                   ),
                          
                          tabPanel("Sumário Estatístico",
                                   dataTableOutput("summario_dataset")
                                   ),
                          tabPanel("Correlação dos Dados",
                                   verbatimTextOutput("plot_correlacao")
                          ),
                          tabPanel ("Matrix de Gráficos",
                                    plotOutput("plot_Analise")
                                  )
                          )                         
            )),
            
              box(title = "Regressão Linear", width = 12
                ,status = "primary" 
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                , box(title = "Análise", width = 6
                    ,status = "info" 
                    ,solidHeader = FALSE 
                    ,collapsible = TRUE 
                    # dependent variable
                    ,selectInput('var_dep', h5('Variável Dependente'), choices = names(df))
    
                    # independent variable
                    ,selectInput('var_indep', h5('Variável Independente'), choices = names(df))
                    
                    #Analisra Dataset
                    
                    ,actionButton("Analisar_Dataset",'Analisar Dataset')
                  )
                , box(title = "Definição", width = 6
                      ,status = "info" 
                      ,solidHeader = FALSE 
                      ,collapsible = TRUE 
                      ,HTML("</br>Em estatística ou econometria, regressão linear é uma equação para se estimar a condicional (valor esperado) de uma variável y, dados os valores de algumas outras variáveis x.</br> </br>")
                     
                )
                
                ,mainPanel(
                       tabsetPanel(type = "tabs", 
                              tabPanel("Histograma",                   
                                            plotOutput("Hist_dv"),
                                            plotOutput("Hist_iv")
                                   ),
                              tabPanel("BarPlot",                   
                                       plotOutput("distPlot_dv"),
                                       #sliderInput("bins_dv", "Numero de barras:", min = 1, max = 50, value = 7),  
                                       plotOutput("distPlot_iv")
                                       #sliderInput("bins_iv", "Numero de barras:", min = 1, max = 50, value = 7)
                                       ),

                              tabPanel("Dispersão",                   
                                       plotOutput("scatter")),

                              tabPanel("Correlations",                   
                                       htmlOutput("corr")),

                              tabPanel("Modelo",                   
                                       verbatimTextOutput("model")),

                              tabPanel("Residuals",                   
                                       plotOutput("residuals_hist"),
                                       plotOutput("residuals_scatter"),
                                       plotOutput("residuals_qqline")
                                  )
                              
                                 )                         
                            )
 
            
                        )
                )

          })
  })
  observeEvent(input$Analisar_Dataset, {
    
    df <- read.csv(input$file1$datapath, header = T, sep = input$sepArqRegLin)
    
    # regression formula
   regFormula <- reactive({
      as.formula(paste(input$var_dep, '~', input$var_indep))
    })
    
    # bivariate model
    model <- reactive({
      lm(regFormula(), data = df[, input$show_vars, drop = FALSE])
    })
    
    
    
    
    # create graphics 
    
    # data view 
    #output$view <- renderTable({head(df) })
    
    
    # summary statistics
    output$summary <- renderPrint({
      summary(cbind(df[input$var_dep], df[input$var_indep]))
    })
    
    #barplot
    # Create data
    output$distPlot_dv <- renderPlot({
    ggplot(df, aes(x=as.factor(df[,input$var_dep]), sepArqRegLin=as.factor(df[,input$var_dep]) )) + geom_bar() 

    })
    
    output$distPlot_iv <- renderPlot({
      ggplot(df, aes(x=as.factor(df[,input$var_indep]), sepArqRegLin=as.factor(df[,input$var_indep]) )) + geom_bar() 
    })
    
    # histograms   

    output$Hist_dv <- renderPlot({qplot(x <-df[,input$var_dep], geom="histogram") })
    #  x    <- df[,input$var_dep]  
    #  bins <- seq(min(x), max(x), length.out = input$bins_dv + 1)
    #  hist(x, breaks = bins, col = 'darkgray', border = 'white', main = input$var_dep, xlab = input$var_dep)
    #})
    output$Hist_iv <- renderPlot({qplot(x <-df[,input$var_indep], geom="histogram") })
    #output$Hist_iv <- renderPlot({
    #  x    <- df[,input$var_indep]  
    #  bins <- seq(min(x), max(x), length.out = input$bins_iv + 1)
    #  hist(x, breaks = bins, col = 'darkgray', border = 'white', main = input$var_indep, xlab = input$var_indep)
    #})
    

    
    
    # scatter plot 
    output$scatter <- renderPlot({
      plot(df[,input$var_indep], df[,input$var_dep],
           xlab = input$var_indep, ylab = input$var_dep,  main = "Scatter Plot of Independent and Dependent Variables", pch = 16, 
           col = "black", cex = 1) 
      
      abline(lm(df[,input$var_dep]~df[,input$var_indep]), col="grey", lwd = 2) 
    })
    
    # correlation matrix
    output$corr <- renderGvis({
      d <- df[,sapply(df,is.integer)|sapply(df,is.numeric)] 
      cor <- as.data.frame(round(cor(d), 2))
      cor <- cbind(Variables = rownames(cor), cor)
      gvisTable(cor) 
    })
    
    # bivariate model
    output$model <- renderPrint({
      summary(model())
    })
    
    # residuals
    output$residuals_hist <- renderPlot({
      hist(model()$residuals, main = paste(input$dv, '~', input$iv), xlab = 'Residuals') 
    })
    
    output$residuals_scatter <- renderPlot({
      plot(model()$residuals ~ df()[,input$iv], xlab = input$iv, ylab = 'Residuals')
      abline(h = 0, lty = 3) 
    })
    
    output$residuals_qqline <- renderPlot({
      qqnorm(model()$residuals)
      qqline(model()$residuals) 
    })
    
    
    
  })
  

  #K-Means Clustering
  
  observeEvent(input$ProcessarKMC, {
    
    #https://shiny.rstudio.com/gallery/kmeans-example.html
    df <- AbrirArquivo(input$file1KMC$datapath, input$sepArqKMC)
    
    df2 = df[sample(nrow(df), 20), ]
    
    output$mydfKMC<- DT::renderDataTable({
      DT::datatable(df2[, input$show_varsKMC, drop = FALSE])
    })
    
    mydf = renderTable(df2[, input$show_varsKMC, drop = FALSE])
    
    #correlação
    
    output$plot_correlacaoKMC <- renderText({ p <- knitr::kable(cor(df[, input$show_varsKMC, drop = FALSE])) })
    
    output$plot_AnaliseKMC <- renderPlot({ ggpairs(df[, input$show_varsKMC], columns = 1:ncol(df[, input$show_varsKMC]),cardinality_threshold = 50) 
      
      
    })
    output$summario_datasetKMC = renderDataTable(summary(df[, input$show_varsKMC, drop = FALSE],digits = 6))
    
    
    output$fKMC <- renderUI({ fluidRow(
      
      box(title = "Dados", width = 12
          ,status = "primary" 
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          
          
          ,mainPanel(
            tabsetPanel(type = "tabs", 
                        tabPanel("Dados",
                                 column(3,
                                        h5("Colunas:"),
                                        checkboxGroupInput("show_varsKMC", "", names(df), selected = names(df))),
                                 column(9,
                                        h5("Dados"), 
                                        DT::dataTableOutput("mydfKMC"))
                        ),
                        
                        tabPanel("Sumário Estatístico",
                                 dataTableOutput("summario_datasetKMC")
                        ),
                        tabPanel("Correlação dos Dados",
                                 verbatimTextOutput("plot_correlacaoKMC")
                        ),
                        tabPanel ("Matrix de Gráficos",
                                  plotOutput("plot_AnaliseKMC")
                        )
            )                         
          )),
      
      box(title = "k-Means Clustering.", width = 12
          ,status = "primary" 
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          , box(title = "Análise", width = 6
                ,status = "info" 
                ,solidHeader = FALSE 
                ,collapsible = TRUE 
                # dependent variable
                ,selectInput('xcol', h5('X Variable'), choices = names(df))
                ,selectInput('ycol', h5('Y Variable'), choices = names(df), selected=names(df)[[2]])
                ,numericInput('clusters', h5('Número de Clusters'), 3, min = 1, max = 9)
                
                #Analisra Dataset
                
                ,actionButton("Analisar_DatasetKMC",'Analisar Dataset')
          )
          , box(title = "Definição", width = 6
                ,status = "info" 
                ,solidHeader = FALSE 
                ,collapsible = TRUE 
                ,HTML("</br>SVM</br> </br>")
                
          )
          
          ,mainPanel(
            tabsetPanel(type = "tabs", 
                        
                        tabPanel("Plot",                   
                                 plotOutput("plotKMC")
                                 
                        ),
                        tabPanel("Histograma",                   
                                 plotOutput("Xcol"),
                                 plotOutput("ycol")
                        ),
                        tabPanel("Modelo",                   
                                 verbatimTextOutput("modelSVM")),
                        
                        tabPanel("Residuals",                   
                                 plotOutput("residuals_hist"),
                                 plotOutput("residuals_scatter"),
                                 plotOutput("residuals_qqline")
                        )
                        
            )                         
          )
          
          
      )
    )
      
    })
  })
  observeEvent(input$Analisar_DatasetKMC, {
    
    df <- read.csv(input$file1KMC$datapath, header = T, sep = input$sepArqKMC)
    
    df2 <-df[,input$show_varsKMC]
    
    
    selectedData <- reactive({
      df[, c(input$xcol, input$ycol)]
    })
    
    clusters <- reactive({
      kmeans(selectedData(), input$clusters)
    })
    
    output$plotKMC <- renderPlot({
      palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
      
      par(mar = c(5.1, 4.1, 0, 1))
      plot(selectedData(),
           col = clusters()$cluster,
           pch = 20, cex = 3)
      points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
    
    # bivariate model
   # modelKMC<- reactive({ svm(input$var_alvo ~ ., data = df2) })
    
   # output$modelSVM <- renderPrint({
   #   summary(modelSVM())
   # })
    
    #barplot
    # Create data
   # output$distPlot_alvo <- renderPlot({
   #   ggplot(df, aes(x=as.factor(df[,input$var_alvo]), sepArqRegLin=as.factor(df[,input$var_alvo]) )) + geom_bar() 
      
   # })
    
    
    # histograms   
    output$Xcol <- renderPlot({qplot(x <-df[,input$xcol], geom="histogram") })
    output$ycol <- renderPlot({qplot(x <-df[,input$ycol], geom="histogram") })
    
    
    # correlation matrix
    output$corr <- renderGvis({
      d <- df[,sapply(df,is.integer)|sapply(df,is.numeric)] 
      cor <- as.data.frame(round(cor(d), 2))
      cor <- cbind(Variables = rownames(cor), cor)
      gvisTable(cor) 
    })
    
    
    # residuals
   # output$residuals_hist <- renderPlot({
    #  hist(modelSVM()$residuals, main = paste(input$var_alvo, '~', '.'), xlab = 'Residuals') 
    #})
    
   # output$residuals_scatter <- renderPlot({
   #   plot(modelSVM()$residuals ~ df()[,input$var_alvo], xlab = input$var_alvo, ylab = 'Residuals')
   #   abline(h = 0, lty = 3) 
   # })
    
   # output$residuals_qqline <- renderPlot({
    #  qqnorm(modelSVM()$residuals)
   #   qqline(modelSVM()$residuals) 
   # })
    
    
    
  })
  
  #Naives Bayes
  
  observeEvent(input$ProcessarNB, {
    
  
  if (!is.null(input$file1NB$datapath)) 
  {
    
    print(input$file1NB$datapath)
    #https://shiny.rstudio.com/gallery/kmeans-example.html
    df <- AbrirArquivo(input$file1NB$datapath, input$sepArqNB)
    
    df2 = df[sample(nrow(df), 20), ]
    
    output$mydfNB<- DT::renderDataTable({
      DT::datatable(df2[, input$show_varsNB, drop = FALSE])
    })
    
    mydf = renderTable(df2[, input$show_varsNB, drop = FALSE])
    
    #correlação
    
    output$plot_correlacaoNB <- renderText({ p <- knitr::kable(cor(df[, input$show_varsNB, drop = FALSE])) })
    
    output$plot_AnaliseNB <- renderPlot({ ggpairs(df[, input$show_varsNB], columns = 1:ncol(df[, input$show_varsNB]),cardinality_threshold = 50) 
      
      
    })
    output$summario_datasetNB = renderDataTable(summary(df[, input$show_varsNB, drop = FALSE],digits = 6))
    
    
    output$fNB <- renderUI({ fluidRow(
      
      box(title = "Dados", width = 12
          ,status = "primary" 
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          
          
          ,mainPanel(
            tabsetPanel(type = "tabs", 
                        tabPanel("Dados",
                                 column(3,
                                        h5("Colunas:"),
                                        checkboxGroupInput("show_varsNB", "", names(df), selected = names(df))),
                                 column(9,
                                        h5("Dados"), 
                                        DT::dataTableOutput("mydfNB"))
                        ),
                        
                        tabPanel("Sumário Estatístico",
                                 dataTableOutput("summario_datasetNB")
                        ),
                        tabPanel("Correlação dos Dados",
                                 verbatimTextOutput("plot_correlacaoNB")
                        ),
                        tabPanel ("Matrix de Gráficos",
                                  plotOutput("plot_AnaliseNB")
                        )
            )                         
          )),
      
      box(title = "k-Means Clustering.", width = 12
          ,status = "primary" 
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          , box(title = "Análise", width = 6
                ,status = "info" 
                ,solidHeader = FALSE 
                ,collapsible = TRUE 
                # dependent variable
                ,selectInput('xcol', h5('X Variable'), choices = names(df))
                ,selectInput('ycol', h5('Y Variable'), choices = names(df), selected=names(df)[[2]])
                ,numericInput('clusters', h5('Número de Clusters'), 3, min = 1, max = 9)
                
                #Analisra Dataset
                
                ,actionButton("Analisar_DatasetNB",'Analisar Dataset')
          )
          , box(title = "Definição", width = 6
                ,status = "info" 
                ,solidHeader = FALSE 
                ,collapsible = TRUE 
                ,HTML("</br>Naives Bayes</br> </br>")
                
          )
          
          ,mainPanel(
            tabsetPanel(type = "tabs", 
                        
                        tabPanel("Plot",                   
                                 plotOutput("plotNB")
                                 
                        ),
                        tabPanel("Histograma",                   
                                 plotOutput("Xcol"),
                                 plotOutput("ycol")
                        ),
                        tabPanel("Modelo",                   
                                 verbatimTextOutput("modelSVM")),
                        
                        tabPanel("Residuals",                   
                                 plotOutput("residuals_hist"),
                                 plotOutput("residuals_scatter"),
                                 plotOutput("residuals_qqline")
                        )
                        
            )                         
          )
          
          
      )
    )
      
    })
  }})
  observeEvent(input$Analisar_DatasetNB, {
    
    df <- read.csv(input$file1NB$datapath, header = T, sep = input$sepArqNB)
    
    df2 <-df[,input$show_varsNB]
    
    
    selectedData <- reactive({
      df[, c(input$xcol, input$ycol)]
    })
    
    clusters <- reactive({
      kmeans(selectedData(), input$clusters)
    })
    
    output$plotNB <- renderPlot({
      palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
      
      par(mar = c(5.1, 4.1, 0, 1))
      plot(selectedData(),
           col = clusters()$cluster,
           pch = 20, cex = 3)
      points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
    
    # bivariate model
    # modelNB<- reactive({ svm(input$var_alvo ~ ., data = df2) })
    
    # output$modelSVM <- renderPrint({
    #   summary(modelSVM())
    # })
    
    #barplot
    # Create data
    # output$distPlot_alvo <- renderPlot({
    #   ggplot(df, aes(x=as.factor(df[,input$var_alvo]), sepArqRegLin=as.factor(df[,input$var_alvo]) )) + geom_bar() 
    
    # })
    
    
    # histograms   
    output$Xcol <- renderPlot({qplot(x <-df[,input$xcol], geom="histogram") })
    output$ycol <- renderPlot({qplot(x <-df[,input$ycol], geom="histogram") })
    
    
    # correlation matrix
    output$corr <- renderGvis({
      d <- df[,sapply(df,is.integer)|sapply(df,is.numeric)] 
      cor <- as.data.frame(round(cor(d), 2))
      cor <- cbind(Variables = rownames(cor), cor)
      gvisTable(cor) 
    })
    
    
    # residuals
    # output$residuals_hist <- renderPlot({
    #  hist(modelSVM()$residuals, main = paste(input$var_alvo, '~', '.'), xlab = 'Residuals') 
    #})
    
    # output$residuals_scatter <- renderPlot({
    #   plot(modelSVM()$residuals ~ df()[,input$var_alvo], xlab = input$var_alvo, ylab = 'Residuals')
    #   abline(h = 0, lty = 3) 
    # })
    
    # output$residuals_qqline <- renderPlot({
    #  qqnorm(modelSVM()$residuals)
    #   qqline(modelSVM()$residuals) 
    # })
    
    
    
  })
  
  #SVM -Support Vector Machine
  
  observeEvent(input$ProcessarSVM, {
    
  if (!is.null(input$file1SVM$datapath)) {
    df <- AbrirArquivo(input$file1SVM$datapath, input$sepArqSVM)
    
    df2 = df[sample(nrow(df), 20), ]
    
    #output$DataArq <- df
    
    output$mydfSVM<- DT::renderDataTable({
      DT::datatable(df[, input$show_varsSVM, drop = FALSE])
    })
    
    #mydf = renderTable(df2[, input$show_varsSVM, drop = FALSE])
    
    #correlação
    
    output$plot_correlacaoSVM <- renderText({ p <- knitr::kable(cor(df[, input$show_varsSVM, drop = FALSE])) })
    
    output$plot_AnaliseSVM <- renderPlot({ ggpairs(df[, input$show_varsSVM], columns = 1:ncol(df[, input$show_varsSVM]),cardinality_threshold = 50) 
      
     
    })
    output$summario_datasetSVM = renderDataTable(summary(df[, input$show_varsSVM, drop = FALSE],digits = 6))
    
    
    output$frow3 <- renderUI({ fluidRow(
      
      box(title = "Dados", width = 12
          ,status = "primary" 
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          
          
          ,mainPanel(
            tabsetPanel(type = "tabs", 
                        tabPanel("Dados",
                                 column(3,
                                        h5("Colunas:"),
                                        checkboxGroupInput("show_varsSVM", "", names(df), selected = names(df))),
                                 column(9,
                                        h5("Dados"), 
                                        DT::dataTableOutput("mydfSVM"))
                        ),
                        
                        tabPanel("Sumário Estatístico",
                                 dataTableOutput("summario_datasetSVM")
                        ),
                        tabPanel("Correlação dos Dados",
                                 verbatimTextOutput("plot_correlacaoSVM")
                        ),
                        tabPanel ("Matrix de Gráficos",
                                  plotOutput("plot_AnaliseSVM")
                        )
            )                         
          )),
      
      box(title = "SVM - Support Vector Machine.", width = 12
          ,status = "primary" 
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          , box(title = "Análise", width = 6
                ,status = "info" 
                ,solidHeader = FALSE 
                ,collapsible = TRUE 
                # dependent variable
                ,selectInput('var_alvo', h5('Alvo'), choices = names(df))
                ,actionButton("Analisar_DatasetSVM",'Analisar Dataset')
          )
          , box(title = "Definição", width = 6
                ,status = "info" 
                ,solidHeader = FALSE 
                ,collapsible = TRUE 
                ,HTML("</br>SVM</br> </br>")
                
          )
          
          ,mainPanel(
            tabsetPanel(type = "tabs", 
                        tabPanel("Histograma",                   
                                 plotOutput("Hist_alvo")
                        ),
                        tabPanel("BarPlot",                   
                                 plotOutput("distPlot_alvo")
                                
                        ),
                        
                        tabPanel("Modelo",                   
                                 verbatimTextOutput("modelSVM")),
                        
                        tabPanel("Residuals",                   
                                 plotOutput("residuals_hist"),
                                 plotOutput("residuals_scatter"),
                                 plotOutput("residuals_qqline")
                        )
                        
            )                         
          )
          
          
      )
    )
      
    })
  }})  
  observeEvent(input$Analisar_DatasetSVM, {
    
    df <- AbrirArquivo(input$file1SVM$datapath, input$sepArqSVM)
   # df=input$DataArq
    
    #df2 <-df[,input$show_varsSVM]
    

    select_dados <- paste('-',input$var_alvo)
    
    # Separando dados levantados das classificações
    dd_brutos <- subset(iris, select = select_dados)
    resultados <- df$input$var_alvo
    
    # Criando modelo SVM a partir do conjunto de dados iris
    # bivariate model
    modelSVM <- reactive({ svm(input$var_alvo ~ ., data = df) })   
    
    teste001 <- predict(modelo_svm, dd_brutos)
    table(teste001, resultados)
    
   
    output$modelSVM <- renderPrint({
     summary(modelSVM())
     })
    
    #barplot
    # Create data
    output$distPlot_alvo <- renderPlot({
      ggplot(df, aes(x=as.factor(df[,input$var_alvo]) )) + geom_bar() 
      
    })
    
    
    # histograms   
    
    output$Hist_alvo <- renderPlot({qplot(x <-df[,input$var_alvo], geom="histogram") })
    
    # correlation matrix
    output$corr <- renderGvis({
      d <- df[,sapply(df,is.integer)|sapply(df,is.numeric)] 
      cor <- as.data.frame(round(cor(d), 2))
      cor <- cbind(Variables = rownames(cor), cor)
      gvisTable(cor) 
    })
    

    # residuals
    output$residuals_hist <- renderPlot({
      hist(modelSVM()$residuals, main = paste(input$var_alvo, '~', '.'), xlab = 'Residuals') 
    })
    
    output$residuals_scatter <- renderPlot({
      plot(modelSVM()$residuals ~ df()[,input$var_alvo], xlab = input$var_alvo, ylab = 'Residuals')
      abline(h = 0, lty = 3) 
    })
    
    output$residuals_qqline <- renderPlot({
      qqnorm(modelSVM()$residuals)
      qqline(modelSVM()$residuals) 
    })
    
    
    
  })
  
  
  observeEvent(input$ProcessarRegLog, {
    
    #dfRegLog <- read.csv(input$fileRegLog$datapath, header = T, sep = input$sepArqRegLin)
    
    dfRegLog <- AbrirArquivo(input$fileRegLog$datapath, input$sepArqRegLin)
    
    df2 = dfRegLog[sample(nrow(dfRegLog), 20), ]
    
    output$mydfRegLog<- DT::renderDataTable({
      DT::datatable(df2[, input$show_vars, drop = FALSE])
    })
    
    myddfRegLog = renderTable(df2[, input$show_vars, drop = FALSE])
    
    #correlação
    
    output$plot_correlacao <- renderText({ p <- knitr::kable(cor(dfRegLog[, input$show_vars, drop = FALSE])) })
    
    output$plot_Analise <- renderPlot({ ggpairs(dfRegLog[, input$show_vars], columns = 1:ncol(dfRegLog[, input$show_vars])) 
    })
    output$summario_dataset = renderDataTable(summary(dfRegLog[, input$show_vars, drop = FALSE],digits = 6))
    
    
    output$frow1 <- renderUI({ fluidRow(
      
      box(title = "Dados", width = 12
          ,status = "primary" 
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          
          
          ,mainPanel(
            tabsetPanel(type = "tabs", 
                        tabPanel("Dados",
                                 column(3,
                                        h5("Colunas:"),
                                        checkboxGroupInput("show_vars", "", names(df), selected = names(dfRegLog))),
                                 column(9,
                                        h5("Dados"), 
                                        DT::dataTableOutput("mydf"))
                        ),
                        
                        tabPanel("Sumário Estatístico",
                                 dataTableOutput("summario_dataset")
                        ),
                        tabPanel("Correlação dos Dados",
                                 verbatimTextOutput("plot_correlacao")
                        ),
                        tabPanel ("Matriz de Gráficos",
                                  plotOutput("plot_Analise")
                        )
            )                         
          )),
      
      box(title = "Regressão Logarítmica", width = 12
          ,status = "primary" 
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          , box(title = "Análise", width = 6
                ,status = "info" 
                ,solidHeader = FALSE 
                ,collapsible = TRUE 
                # dependent variable
                ,selectInput('var_dep', h5('Variável Dependente'), choices = names(dfRegLog))
                
                # independent variable
                ,selectInput('var_indep', h5('Variável Independente'), choices = names(dfRegLog))
                
                #Analisra Dataset
                
                ,actionButton("Analisar_Dataset",'Analisar Dataset')
          )
          , box(title = "Definição", width = 6
                ,status = "info" 
                ,solidHeader = FALSE 
                ,collapsible = TRUE 
                ,HTML("</br>Em estatística ou econometria, regressão linear é uma equação para se estimar a condicional (valor esperado) de uma variável y, dados os valores de algumas outras variáveis x.</br> </br>")
                
          )
          
          ,mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Histograma",                   
                                 plotOutput("Hist_dv"),
                                 plotOutput("Hist_iv")
                        ),
                        
                        tabPanel("BarPlot",                   
                                 plotOutput("distPlot_dv"),
                                 plotOutput("distPlot_iv")
                        ),
                        
                        tabPanel("Dispersão",                   
                                 plotOutput("scatter")),

                        tabPanel("Modelo",                   
                                 verbatimTextOutput("model"))

                        
            )                         
          )
          
          
      )
    )
      
    })
  })
}

shinyApp(ui, server)