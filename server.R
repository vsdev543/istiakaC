# Options -----------------------------------------------------------------
options(shiny.maxRequestSize=30*1024^2)
# options(error=expression(dump.frames()))
options(scipen=999)


# Load Packages -----------------------------------------------------------
library(shiny)
library(tidyverse)
library(plotly)
library(shinyBS)
library(shinyjs)
library(shinycssloaders)
library(shinybusy)
library(DT)
library(shinythemes)
library(shinydashboard)
library(openxlsx)


# Globals -----------------------------------------------------------------
tabPanelsPrev <- c()


# Run Server module -------------------------------------------------------
function(input, output, session) {

  HSRPath <- reactive ({
    # browser()
    inFile <- input$inptHSRRawData
    
    if (is.null(inFile))
      return(NULL)
    
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    
    file_name = paste0(inFile$datapath, ".xlsx")
    
    return(file_name)
  })
  
  output$inptXlsxSheetsDropDownHSRData <- renderUI({
    # browser()
    req(HSRPath())
    
    selectInput(inputId = "xlsSheetHSRData", 
                label = "Select Excel sheet with IDR data", 
                choices = openxlsx::getSheetNames(HSRPath()),
                multiple = TRUE)
  })
  
  # keep adding/removing xlsx sheet names into reactiveVal()
  multichoice <- reactiveVal()
  observeEvent(input$xlsSheetHSRData, {
    multichoice(input$xlsSheetHSRData)
  })
  
  HSRData <- reactive({
    # browser()
    req(HSRPath())
    req(multichoice())
    
    out <- tryCatch({
      # browser()
      message(multichoice())
      
      lapply(multichoice(), openxlsx::read.xlsx, xlsxFile = HSRPath())
      
      
    }, error = function(msg) {
      message(paste0("HSRData reactive failed."))
      message(msg)
      return(NA)
    })
    
    names(out) = trimws(input$xlsSheetHSRData)
    out <- lapply(out, function(x) {
      x %>%
        `colnames<-`(gsub("\\.\\(","_", as.character(colnames(.)))) %>%
        `colnames<-`(gsub("[[:space:]]","_", as.character(colnames(.)))) %>%
        `colnames<-`(gsub("[.,\\(]", "_", as.character(colnames(.)))) %>%
        `colnames<-`(gsub("\\)", "", as.character(colnames(.)))) %>%
        `colnames<-`(make.unique(colnames(.))) %>%
        dplyr::select(-contains("*"))
    }) %>%
      
    return(out)
    
  })
  
  output$btnShowHSRGroupsInTabPanels <- renderUI({
    req(HSRData())
    actionButton(
      "btnShowHSRGroupsInTabPanelsSeparated",
      label = "Confirm Groups"
    )
  })
  
  observeEvent(input$btnShowHSRGroupsInTabPanelsSeparated, {
    # browser()
    
    # regPlotNames <- c()
    # for (grp in names(HSRData())) {
    #   groupData <- HSRData()[[grp]]
    #   noReplicates <- groupData %>% dplyr::select(matches("Rep|rep")) %>% ncol(.)
    #   for (rep in 1:noReplicates) {
    #     regPlotNames <- c(regPlotNames, paste0("linRegPlot_", grp, "_", rep))
    #   }
    # }
    
    ids <- names(HSRData())
    
    if (!is_empty(tabPanelsPrev)) {
      for (id in tabPanelsPrev) {
        removeTab(inputId = "groups_tabs", target = paste0("tab_", id))
      }
    }
    
    
    plot_output_list <- list()
    i <- 1
    
    # add tabPabels into groups_tabs tabsetPanel
    for (id in ids) {
      
      groupData <- HSRData()[[id]]
      noReplicates <- groupData %>% dplyr::select(matches("Rep|rep")) %>% ncol(.)
      message(paste0("#replicates = ", noReplicates))
      
      appendTab(
        inputId = "groups_tabs",
        tabPanel(
          paste0("Data processing for ", id, " group ..."),
          
          local({
            
            id_local <- id
            # rep_local <- rep
            
            plotname <- paste0("linRegPlot_", id_local)
            message(paste0("Plot name in tabPanel: ", plotname))
            
            # output$linRegPool <- renderUI({
            #   regPlotNames = c("linRegPlot_A20ED1311","linRegPlot_A20ED1343","linRegPlot_A20FD1490")
            #   plot_output_list <- lapply(regPlotNames, function(pltNames) {
            #     plotOutput(pltNames, height = 280, width = 250)
            #   })
            # 
            #   # Convert the list to a tagList - this is necessary for the list of items
            #   # to display properly.
            #   # message(plot_output_list)
            #   do.call(tagList, plot_output_list)
            # })
            
            # plotOutput(plotname, height = 280, width = 250)
            
            plot_output_list[[i]] <<- list(
              plotOutput(plotname, height = 280, width = 250),
              actionButton(paste0("Button_", id_local), "Some new button!")
            )
            
            output[[plotname]] <- renderPlot({
              plot(
                x = 1:100, 
                y = rnorm(1:100),
                main = paste0("Plot for ", plotname),
                xlab = "Time [min]",
                ylab = "Y"
              )
            })
          }),
          
          value = paste0("tab_", id),
          title = paste0(id),
          icon = icon("analytics")
        )
      )
      i = i + 1
    }
    
    output$linRegPool <- renderUI({
      message("renderUI ...")
      do.call(shiny::tabsetPanel, plot_output_list)
    })
    
    # remember last state
    tabPanelsPrev <<- ids
  })
  
  
  output$renderprint <- renderPrint({
    # browser()
    req(HSRData())
    message(names(HSRData()))
    lapply(HSRData(), head)
  })
  
}