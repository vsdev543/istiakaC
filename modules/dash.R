dash_UI <- function(id,title) {
  ns <- NS(id)
  div(style="padding:0px 5vw",
    h1(title),
    div(style="width:90vw; background-color:#fff;padding:10px; border-radius:10px;color:#333;display:flex;",
    uiOutput(ns('main')),
    div(style="margin-left:10px;",
    uiOutput(ns('sub'))
    )
    ),
    div(style="width:90vw; background-color:#fff;padding:10px; border-radius:10px;margin-top:10px;",
        uiOutput(ns('out'))
    )
  )
}

dash <- function(input, output, session) {
  ns<-session$ns
  
  sheets<-getSheetNames("data/oPestTL_V104.xlsx")
  
  output$main<-renderUI({
    selectInput(ns("equip"),"Instrument",choices = sheets,width = '300px')
  })
  
  df<-reactive({
    df<-read.xlsx("data/oPestTL_V104.xlsx",sheet = input$equip)
    })
  
  output$sub<-renderUI({
    selectInput(ns("pest"),"Pesticide",choices = df()[,1],width = "500px")
  })
  
  row<-reactive({
    req(input$pest)
    
    df()[df()[,1]==input$pest,][1,]
  })
  
  
  output$out<-renderUI({
    param<-names(row())[-1]
    
    div(
      div(style="display:flex; color:#333; align-items:end;",
      span(input$equip,style="font-size:1.3em; font-weight:900; "),
      span(input$pest,style="font-size:1em; font-weight:600;margin-left:10px;")
      ),
      hr(),
      br(),
    div(class="grid-5",
    lapply(param,function(para){
      div(style="background-color:#1E91D6; color:#fff;text-align:center;padding:10px; border-radius:10px;",
          span(para,style="font-weight:900; font-size:1.1em"),br(),
          span(row()[1,para],style="font-weight:600; font-size:1em")
          )
    })
    ),
    br()
    )
  })
  
}
