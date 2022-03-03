# Load Packages -----------------------------------------------------------
library(plotly)
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinycssloaders)
library(shinybusy)
library(DT)
library(shinythemes)
library(shinydashboard)

# Load UI -----------------------------------------------------------------
fluidPage(
  
  shinyjs::useShinyjs(),
  theme = shinytheme("simplex"),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "main.css")
  ),
  
  # App title ----
  titlePanel(title = "Intrinsic Dissolution Rate - Automatic Data Processing", windowTitle = "Intrinsic Dissolution Rate"),
  
  navlistPanel(
        
    "Automatic (High Sampling Rate)",
    tabPanel(
      "Load Components",
      tabsetPanel(id="groups_tabs",
        tabPanel(
          "Data",
          fileInput(
            "inptHSRRawData", 
            "Select file with raw data",
            accept = c(".xlsx")
          ),
          uiOutput("inptXlsxSheetsDropDownHSRData"),
          uiOutput("btnShowHSRGroupsInTabPanels"),
          verbatimTextOutput("renderprint")
          , icon = icon("upload")
        )
        
      )
      , icon = icon("angle-double-right")
    )
    
    , selected = ""
    , widths = c(3, 9)
  ),
  
  # add pools for other dynamically generated UI components
  uiOutput("linRegPool", style = "display: none;")
  
)
