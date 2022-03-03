welcome_UI <- function(id,appName,appDesc) {
  ns <- NS(id)
  div(style="height:100vh; width:100vw; position:relative;overflow-x:hidden;overflow-y:hidden;background-image: url('Vector 1.svg');  background-repeat: no-repeat;
  background-size: cover;",
      img(src="vector.png",width='500px',height="auto",class='moveItx',style="position:absolute; left:2vw; bottom:20vh;"),
    
    div(style="position:absolute; right:2vw; top:20vh; text-align:right; width: 50vw;",
        span("WELCOME TO",style="font-weight:900; font-size:3vw;color:#1E91D6;"),br(),
        span(class="brand",appName,style="font-weight:900; font-size:5vw;"),br(),
        span(appDesc,
             style="font-weight:800; font-size:1vw;"
             ),hr(),br(),
        actionBttn(inputId = ns('goIn'),"Proceed",style = "jelly",size = 'lg')
        )
  )
}

welcome <- function(input, output, session) {

}