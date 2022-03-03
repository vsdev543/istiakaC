dash_UI <- function(id,title) {
  ns <- NS(id)
  fluidPage(style="padding:0px 5vw",
    h1(title,class='brand'),

    div(style="width:90vw; background-color:#fff;padding:10px; border-radius:10px;color:#333;",
        fluidRow(
         column(3,
                fileInput(ns('dfIn'),label = "Raw Data",accept = c('.xlsx','.xls'))
                ),
         column(5,
                uiOutput(ns('sub'))
                ),
         column(2,
                br(),
                actionBttn(inputId = ns('lock'),label = "Lock data",icon = icon("cogs"),style = "stretch",color = 'primary')
                ),
         column(2,
                br(),
                actionBttn(inputId = ns('reset'),label = "Reset",icon = icon("sync"),style = "stretch",color = 'success')
         )
        )
    ),
    # div(style="width:90vw; background-color:#fff;padding:10px; border-radius:10px;margin-top:10px; color:#333;",
        uiOutput(ns('out'))
    # )
  )
}

dash <- function(input, output, session) {
  ns<-session$ns
  r<-reactiveValues(sw=0)
  
  observeEvent(input$reset,{
    r$sw<-0
    
    enable(id = "dfIn")
    enable(id = "sheet")
  })
  
  observeEvent(input$dfIn,{
    r$sw<-0
  })
  
  observeEvent(input$lock,{
    if(!is.null(input$sheet)){
    r$sw<-1
    disable(id = "dfIn")
    disable(id = "sheet")
    }else{
      shinyalert(title = "Error! No sheet selected",text = "Do select sheets to generate visualizations")
    }
  })
  
  ns<-session$ns
  
  
  df<-reactive({
    df<-read.xlsx("data/oPestTL_V104.xlsx",sheet = input$equip)
    })
  
  output$sub<-renderUI({
    if(!is.null(input$dfIn)){
      selectInput(ns("sheet"),"Excel sheet with IDR data",choices = getSheetNames(input$dfIn$datapath),selected = getSheetNames(input$dfIn$datapath)[1],width = "600px",multiple = T)
    }
  })
  
  row<-reactive({
    req(input$pest)
    
    df()[df()[,1]==input$pest,][1,]
  })
  
  
  output$out<-renderUI({
    if(is.null(input$sheet)){
      div(class="ozz",
      h1("Upload Raw data file and select sheets to proceed")
      )
    }else{
      if(r$sw==0){
        lapply(input$sheet,function(sheet){
          div(class="ozz",
            h3(sheet),
          renderDT({
            df<-read.xlsx(input$dfIn$datapath,sheet = sheet)
          },server = F,options = list(scrollX = TRUE)),
          hr(),
          br()
          )
        })
      }else{ 
        div(class="ozz",
        tabsetPanel(id = 'tabs',selected = input$sheet[1],type = "pills",
                    .list = lapply(input$sheet,function(sheet){
                      df<-read.xlsx(input$dfIn$datapath,sheet = sheet)
                      # plotNames<-names(df)[grepl(pattern = "time",names(df),ignore.case = T) & grepl(pattern = "sec",names(df),ignore.case = T)]
                      plotNames<-names(df)[grepl("rep",names(df),ignore.case = T)]
                      
                      tabPanel(title = sheet,
                               tagList(br(),
                               lapply(plotNames,function(param){
                                 div(
                                   fluidRow(column(6,h3(paste(sheet,"|",param))),column(6,
                                   sliderInput(inputId = ns(paste0(sheet,"-",param,"slide")),
                                               min = floor(min(as.numeric(df[,param]),na.rm = T)),
                                               max =  ceiling(max(as.numeric(df[,param]),na.rm = T)),
                                               value = c(min(as.numeric(df[,param]),na.rm = T),max(as.numeric(df[,param]),na.rm = T)),label = paste(param,"range"),
                                               step = 0.5,width = "100%"
                                               ),br(),
                                   )),
                                  plotlyOutput(ns(paste0(sheet,"-",param))),hr()
                                 )
                               }),
                               renderDT({
                                 df
                               },server = F,options = list(scrollX = TRUE)),
                               )
                               )
                    })
                    )
        )
      }
    }
  })
  
  observe({
    lapply(input$sheet,function(sheet){
      df<-read.xlsx(input$dfIn$datapath,sheet = sheet)
      plotNames<-names(df)[grepl("rep",names(df),ignore.case = T)]
      
      lapply(plotNames,function(param){
        
        df<-df[as.numeric(df[,param])<=input[[paste0(sheet,"-",param,"slide")]][2] & as.numeric(df[,param])>=input[[paste0(sheet,"-",param,"slide")]][1],]
        
        output[[paste0(sheet,"-",param)]]<-renderPlotly({
          plot_ly(x=df[,names(df)[grepl(pattern = "time",names(df),ignore.case = T) & grepl(pattern = "sec",names(df),ignore.case = T)]],
                  y=df[,param],
                  type = 'scatter',
                  mode='lines+markers'
                  )%>%
            layout(
              yaxis=list(title=param),
              xaxis=list(title=names(df)[grepl(pattern = "time",names(df),ignore.case = T) & grepl(pattern = "sec",names(df),ignore.case = T)]),
              title=paste(sheet,"|",param)
            )
        })
        
      })
    })
  })
  
  
  
}
