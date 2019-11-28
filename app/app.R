#App.R


# Pakker ------------------------------------------------------------------


library(shinydashboard)
library(RMariaDB)
library(shiny)
library(pdftools)
library(stringr)
library(stringi)
library(tm)
library(ggplot2)
library(dplyr)
library(wordcloud)
library(plotly)
library(DT)


ui <- dashboardPage(

#1  Header ------------------------------------------------------------------

  dashboardHeader( disable = FALSE,
    
    title = "Pdf extractor",
    titleWidth = 450


  )# Header slutt
,


# Sidebar -----------------------------------------------------------------

  dashboardSidebar(  disable = FALSE,
                     width = 250,

    
# Sidebarmenu -------------------------------------------------------------

        
    sidebarMenu(

      
      menuItem(tags$em("Last opp PDFs",style="font-size:150%"),icon=icon("upload"),tabName="data"),
      menuItem(tags$em("Summaries",style="font-size:150%"),icon=icon("bar-chart-o"),tabName="summary"),
      menuItem(tags$em("Search and Filter",style="font-size:150%"),icon=icon("search"),tabName="search")

    ) #Menu slutt

    
  ) # Sidebar slutt
,


# 2 Body --------------------------------------------------------------------

  dashboardBody(
    
    
  ###  
    tabItems(
      tabItem(tabName="data",
              
              
              
              column(width = 4,
                     fileInput('file1', em('Velg pdf fil',style="text-align:center;color:blue;font-size:120%"),multiple = TRUE,
                               accept=c('.pdf')),
                     
                     br(),
                     br()
              ),
              br()
              
              ),
      
      
      tabItem(tabName="summary",
              fluidRow(
                tabBox(width=12,
                       tabPanel(tags$em("Word Cloud",style="font-size:150%"),
                                
                                column(width = 8,
                                       plotOutput("wordcloud")),
                                
                                column(width = 4,
                                       br(),
                                       uiOutput("minfreq"),
                                       br(),
                                       uiOutput("maxwords"),
                                       br(),
                                       uiOutput("forEach"))
                                
                       ),
                       
                       
                       tabPanel(tags$em("Plotly Bar graph",style="font-size:150%"),
                                
                                plotlyOutput("myplot",height = "700px"),
                                br(),
                                
                                uiOutput("numwords")
                                
                       )
                       
                       
                       
                       
                ))),
      
      
      
      
      
      tabItem(tabName="search",
              
              
              DT::dataTableOutput("DataTable"),
              br(),
              uiOutput("text"),
              uiOutput('forsearch'),
              uiOutput('searchbutton'),
              plotlyOutput("searched",height = '600px'),
              
              
              
              #Google analytics
              
              tags$head(HTML(
                "<script>
            (function(i,s,o,g,r,a,m){
            i['GoogleAnalyticsObject']=r;i[r]=i[r]||
            function(){
            (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();
            a=s.createElement(o), m=s.getElementsByTagName(o)[0];
            a.async=1;
            a.src=g;m.parentNode.insertBefore(a,m)
            })
            (window, document, 'script',
            '//www.google-analytics.com/analytics.js','ga');
            
            ga('create', 'UA-110957197-3', 'auto');
            ga('send', 'pageview');
            
            </script>"
              ))  # Google analytics slutt
              
              
              
              
              
      )
              )




# Neste side kommer her (lim inn over. -----------------------------------------------


    ) #  dashboardBody slutt
    
    
  ) #Body slutt





# 3 Server ------------------------------------------------------------------


server <- function(input, output) { 
  
options(shiny.maxRequestSize=1600*1024^2) 
  

  mypdf2_list<-reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      
      withProgress({
        setProgress(message = "Extracting Text...")
        
        lst=list()
        for(i in 1:length(inFile[,1])){
          lst[[i]] <- pdf_text(inFile[[i, 'datapath']])
        }
        
        lst
        
      })
    }
    
  })
  

  
  
  
  documents<-reactive({
    
    
    inFile <- input$file1
    
    inFile$name
    
  })
  
  
  
  
  
  
  mymatrix<-reactive({
    
    
    withProgress({
      setProgress(message = "Processing corpus...")
    
      txt=c(unlist(mypdf2_list()))
      
      
      if(is.null(txt))
        
        return(NULL)
      
      # Create corpus
      corpus=Corpus(VectorSource(txt))
      # Convert to lower-case
      corpus=tm_map(corpus,tolower)
  #    corpus = tm_map(corpus, PlainTextDocument)
      corpus = tm_map(corpus, removePunctuation)
      
      # Remove stopwords
      corpus=tm_map(corpus,function(x) removeWords(x,stopwords("english")))
      corpus=tm_map(corpus,function(x) removeWords(x,stopwords("norwegian")))
      
     

      frequencies = DocumentTermMatrix(corpus)
      
      sparse =as.matrix(frequencies)
      sparse=apply(sparse,2,sum)
      sparse=sparse[order(sparse,decreasing = T)]
      
      Term=names(sparse)
      Frequency=as.vector(sparse)
      
      sparse=as.data.frame(list(Term=Term,Frequency=Frequency))
      sparse$Term = stri_trans_totitle(sparse$Term) 
      sparse
      
    })
  })
  
  
  
  
  output$DataTable <-DT::renderDataTable(
    
    withProgress({
      setProgress(message = "Preparing Table...")
      datatable(
        mydataTable(), filter = 'top',  
        options = list(pageLength = 5, autoWidth = TRUE),
        rownames= FALSE
        
      )
    })
  )
  
  
  mydataTable<-reactive({
    
    withProgress({
      setProgress(message = "Computing...")
      if(is.null(mymatrix()))
        return(NULL)
      
      
      pdfs=mypdf2_list()
      
      if(length(pdfs)>0){
        
        documents=documents()
        
        mydata=data.frame()
        
        for(i in 1:length(pdfs)){
          
          txt = pdfs[[i]]
          
          # Create corpus
          corpus=Corpus(VectorSource(txt))
          # Convert to lower-case
          corpus=tm_map(corpus,tolower)
#          corpus = tm_map(corpus, PlainTextDocument)
          corpus = tm_map(corpus, removePunctuation)
          
          # Remove stopwords
          corpus=tm_map(corpus,function(x) removeWords(x,stopwords("english")))
          
          frequencies = DocumentTermMatrix(corpus)
          sparse =as.matrix(frequencies)
          sparse=apply(sparse,2,sum)
          sparse=sparse[order(sparse,decreasing = T)]
          
          Term=names(sparse)
          Frequency=as.vector(sparse)
          
          sparse=as.data.frame(list(Term=Term,Frequency=Frequency))
          sparse$Term = stri_trans_totitle(sparse$Term) 
          sparse$Document=documents[i]
          mydata=rbind(mydata,sparse)
          
        }
        
        mydata = mydata%>%mutate(Term = factor(Term,levels = Term[order(Frequency,decreasing =T)]))
        
        if(nrow(mydata)>7000){
          head(mydata,7000)
        }else{mydata}
        
      }
    })
  })
  
  
  
  output$wordcloud <- renderPlot({
    
    if(is.null(mymatrix()))
      return(NULL)
    
    sparse=mymatrix()
    
    pal2 <- brewer.pal(8,"Dark2")
    
    wordcloud(sparse$Term,sparse$Frequency, min.freq=input$freq, max.words=input$max,
              random.order=FALSE,scale=c(4,0.5),
              rot.per=0.35, use.r.layout=FALSE, colors=pal2)
    
    
    
    
    
    if(input$for_each==TRUE){
      
      
      if(is.null(mymatrix()))
        return(NULL)
      
      
      pdfs=mypdf2_list()
      
      
      
      if(length(pdfs)>0){
        
        
        
        
        for(i in 1:length(pdfs)){
          
          txt = pdfs[[i]]
          
          # Create corpus
          corpus=Corpus(VectorSource(txt))
          # Convert to lower-case
          corpus=tm_map(corpus,tolower)
#          corpus = tm_map(corpus, PlainTextDocument)
          corpus = tm_map(corpus, removePunctuation)
          
          # Remove stopwords
          corpus=tm_map(corpus,function(x) removeWords(x,stopwords("english")))
          
          
          frequencies = DocumentTermMatrix(corpus)
          #sparse = removeSparseTerms(frequencies,1)
          sparse =as.matrix(frequencies)
          sparse=apply(sparse,2,sum)
          sparse=sparse[order(sparse,decreasing = T)]
          
          Term=names(sparse)
          Frequency=as.vector(sparse)
          
          sparse=as.data.frame(list(Term=Term,Frequency=Frequency))
          sparse$Term = stri_trans_totitle(sparse$Term) 
          
          
          pal2 <- brewer.pal(8,"Dark2")
          
          documents=documents()
          
          x11(title = documents[i])
          
          
          wordcloud(sparse$Term,sparse$Frequency, min.freq=input$freq, max.words=input$max,
                    random.order=FALSE,scale=c(4,0.5),
                    rot.per=0.35, use.r.layout=FALSE, colors=pal2)
          
        }
      }
    }
    
    
  })
  
  
  toshow<-reactive({
    input$show
  })
  
  
  output$myplot <- renderPlotly({
    
    if(is.null(mymatrix()))
      return(NULL)
    
    maximum =toshow()
    
    sparse=mymatrix()
    sparse=sparse[1:maximum,]
    
    q=sparse%>%mutate(Term = factor(Term,levels = Term[order(Frequency,decreasing =F)]))%>%
      ggplot(aes(x=Term,y=Frequency))+geom_bar(stat='identity',color='#33ccff',fill='#33ccff')+
      xlab("")+ggtitle('Mest brukt ord')+theme(plot.title = element_text(size = 16,colour="blue"))+
      coord_flip()# +theme(axis.text.x = element_text(angle=-90))
    
    
    
    p <- ggplotly(q + ylab(" ") + xlab(" "))
    x <- list(
      title = "Frequency"
    )
    y <- list(
      title = ""
    )
    p %>% layout(xaxis = x, yaxis = y)
    
    
    
    
  })
  
  
  
  mysearch<-reactive({
    
    input$search
    isolate(
      stri_trans_totitle(input$searchText)
    )
    
  })
  
  
  
  SearchMatrix<-reactive({
    
    if(is.null(mysearch()))
      return(NULL)
    
    if(is.null(mymatrix()))
      return(NULL)
    
    pdfs=mypdf2_list()
    
    
    if(length(pdfs)>0){
      
      
      
      mm=data.frame()
      
      for(i in 1:length(pdfs)){
        
        txt = pdfs[[i]]
        
        # Create corpus
        corpus=Corpus(VectorSource(txt))
        # Convert to lower-case
        corpus=tm_map(corpus,tolower)
  #      corpus = tm_map(corpus, PlainTextDocument)
        corpus = tm_map(corpus, removePunctuation)
        
        # Remove stopwords
        corpus=tm_map(corpus,function(x) removeWords(x,stopwords("english")))
        
      
        frequencies = DocumentTermMatrix(corpus)
        #sparse = removeSparseTerms(frequencies,1)
        sparse =as.matrix(frequencies)
        sparse=apply(sparse,2,sum)
        sparse=sparse[order(sparse,decreasing = T)]
        
        Term=names(sparse)
        Frequency=as.vector(sparse)
        
        sparse=as.data.frame(list(Term=Term,Frequency=Frequency))
        sparse$Term = stri_trans_totitle(sparse$Term) 
        
        if(stri_trans_totitle(mysearch())%in%sparse$Term==TRUE){
          total= filter(sparse,Term==stri_trans_totitle(mysearch()))$Frequency
        }else{total=0}
        
        mm=rbind(mm,total)
        
      }
      
      Documents=documents()
      names(mm)=c('Frequency')
      
      mm=cbind(Documents,mm)
      mm
    }
    
  })
  
  
  
  output$searched<-renderPlotly({
    
    if(is.null(SearchMatrix()))
      return(NULL)
    
    if(nchar(mysearch())==0)
      return(NULL)
    
    
    mm=SearchMatrix()
    
    q=mm%>%mutate(Documents = factor(Documents,levels = Documents[order(Frequency,decreasing =T)]))%>%
      ggplot(aes(x=Documents,y=Frequency))+geom_bar(stat='identity',color='#c2c2a3',fill='#999966',width=.5)+
      xlab("")+ggtitle('Search Term Frequency by Document')+ylab('')+
      theme(axis.text.x = element_text(angle=-20))+
      theme(plot.title = element_text(size = 16,colour="blue"))+
      theme(axis.text.x = element_text(colour="#4d0000",size=12))
    
    
    p = ggplotly(q)
    
    
    p
    
    
  })
  
  
  output$minfreq = renderUI({
    
    if(is.null(mymatrix()))
      return(NULL)
    sliderInput("freq",
                em("Minimum Frequency:",style="color:black;font-size:100%"),
                min = 1,  max = 50, value = 15)
  })
  
  
  output$numwords = renderUI({
    
    if(is.null(mymatrix()))
      return(NULL)
    
    sliderInput("show",
                em("Number of Words to Display:",style="color:Blue;font-size:100%"),
                min = 5,  max = 50, value = 25)
  })
  
  
  output$maxwords = renderUI({
    
    if(is.null(mymatrix()))
      return(NULL)
    sliderInput("max",
                em("Maximum Number of Words:",style="color:black;font-size:100%"),
                min = 1,  max = 300,  value = 200)
  })
  
  
  output$forEach = renderUI({
    
    if(is.null(mymatrix()))
      return(NULL)
    checkboxInput("for_each", label = p("Create Word Cloud for Each Document",style="color:#ff0000;font-size:120%" ))
  })
  
  
  
  
  output$forsearch = renderUI({
    
    if(is.null(mymatrix()))
      return(NULL)
    sidebarSearchForm(label = "Search...", textId="searchText", "searchButton")
  })
  
  
  output$text= renderUI({
    
    if(!is.null(mymatrix())){
      
      
      p("Search a word and you will see a bar graph of its frequency across all documents",
        style="text-align:center;color:#990099;font-size:110%")
    }
  })
  
  
  
  output$searchbutton = renderUI({
    
    if(is.null(mymatrix()))
      return(NULL)
    actionButton("search", "Search")
  })
  
  
  
  
  
  

  
  } # Server slutt


# App ---------------------------------------------------------------------


shinyApp(ui, server)