library(shinydashboard)
library(tidyverse)
library(ggdendro)
library(readxl)
library(plotly)
library(shiny)

path <- "DADOS_CALCIO.xlsx"
semanas <- excel_sheets(path = path)

ui <- dashboardPage(
  header = dashboardHeader(title = "Projeto Ressonância"),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Visualização",tabName = "vasualizacao",
               icon = icon("eye")),
      menuItem("Regressão",tabName = "regressao",
               icon = icon("chart-line"))
    )
  ),
  body = dashboardBody(
    tabItems(
      # UI - Visualização -------------------------------------------------------
      tabItem(
        tabName = "vasualizacao",
        fluidRow(
          column(
            width = 12,
            h1("Visualização Planilha Cálcio")
          )
        ),
        hr(style = "border-top: 1px solid black;"),
        br(),
        fluidRow(
          column(
            width = 12,
            fluidRow(
              box(
                width = 3,
                title = "Seleção da Semana",
                solidHeader = TRUE,
                status = "primary",
                selectInput(
                  "semana",
                  "",
                  choices = semanas,
                  selected = "Semana 01"
                )
              ),
              box(
                width = 3,
                title = "Seleção de Tratamento",
                solidHeader = TRUE,
                status = "primary",
                selectInput(
                  "trat",
                  "",
                  choices = ""
                )
              ),
              box(
                width = 3,
                title = "Seleção de repetição",
                solidHeader = TRUE,
                status = "primary",
                selectInput(
                  "rep",
                  "",
                  choices = ""
                )
              ),
              box(
                width = 3,
                title = "Seleção de Variável",
                solidHeader = TRUE,
                status = "primary",
                selectInput(
                  "coluna",
                  "",
                  choices = "",
                  selected = ""
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              tableOutput("dados")
            )
          )
        )
      ),
      tabItem(
        tabName = "regressao"
      )
    )
  ),
  title = "Projeto"
)



server <- function(input, output, session){
  # Server - Visualização ---------------------------------------------------
  dados<-reactive({
    req(input$semana)
    "DADOS_CALCIO.xlsx" |>
      read_excel(sheet = input$semana, na = "-")
  })

  output$dados <- renderTable({
    dados()
  })

  observe({
    req(input$semana)
    colunas <- dados() |> names()
    updateSelectInput(
      session,
      "trat",
      choices = colunas,
      selected = "Tratamento"
    )
  })

  observe({
   req(input$trat)
   colunas_rep <- dados() |>
     select(-input$trat) |>
     names()
   updateSelectInput(
       session,
       "rep",
       choices = colunas_rep,
       selected = "Repetição"
     )
  })

   observe({
     req(input$semana,input$trat,input$rep)
     # browser()
     colunas <- dados() |>
       select(-input$trat, -input$rep) |>
       select(is.numeric | is.logical) |>
       names()
     updateSelectInput(
       session,
       "coluna",
       choices = colunas,
       selected = "NívelCálcio"
     )
   })
}

shinyApp(ui, server)
