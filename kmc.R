library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(googleVis)
library(knitr)
library(GGally)
library(e1071)

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