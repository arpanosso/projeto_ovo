library(shinydashboard)
library(tidyverse)
library(ggdendro)
library(readxl)
library(shiny)
library(tidyr)
library(ggridges)
library(ggpubr)


path <- "DADOS_CALCIO.xlsx"
semanas <- excel_sheets(path = path)

ui <- dashboardPage(
  header = dashboardHeader(title = "Projeto Ressonância"),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Visualização",tabName = "vasualizacao",
               icon = icon("eye")),
      menuItem("Regressão",tabName = "regressao",
               icon = icon("chart-line")),
      menuItem("Multivariada",tabName = "multivariada",
               icon = icon("pagelines"))
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
          )
        ),
        fluidRow(
          column(
            width = 12,
            fluidRow(
              box(
                width = 6,
                title = "Densidade",
                solidHeader = TRUE,
                status = "primary",
                plotOutput("ridges")
              ),
              box(
                width = 6,
                title = "Boxplot",
                solidHeader = TRUE,
                status = "primary",
                plotOutput("boxplot")
              )
            ),
            tableOutput("dados")
          )
        )
      ),
# UI - regressao linear -----------------------------------------------------
      tabItem(
        tabName = "regressao",
        fluidRow(
          column(
            width = 12,
            h1("Regressão linear")
          )
        ),
        hr(style = "border-top: 1px solid black;"),
        br(),
        fluidRow(
          column(
            width = 12,
            fluidRow(
              box(
                width = 6,
                title = "Seleção as variáveis",
                solidHeader = TRUE,
                status = "primary",
                fluidRow(
                  column(
                    width = 6,
                    selectInput(
                      "var_x",
                      "Selecione a variável X",
                      choices = "",
                      selected = ""
                    )
                  ),
                  column(
                    width = 6,
                    selectInput(
                      "var_y",
                      "Selecione a variável Y",
                      choices = "",
                      selected = ""
                    )
                  ),
                  column(
                    width = 12,
                    checkboxGroupInput(inputId = "filtro_semana",
                                       label = "Selecione a semana",
                                       choices = 1:9,
                                       inline = TRUE,
                                       selected = 1:9
                                       )
                  ),
                  column(
                    width=12,
                    plotOutput("dispersao")
                  )
                )
              )
            )
          )
        )
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

  ANOVA <- reactive({
    dados <- dados()
    req(input$coluna)
    trat <- dados |> pull(input$trat) |> forcats::as_factor()
    bloco <- dados |> pull(input$rep) |> forcats::as_factor()
    y <- dados |> pull(input$coluna)
    mod<-aov(y~trat+bloco)
    xij<-predict(mod, newdata = dados$bloco<-as.factor(bloco))
    y[is.na(y)] <- xij[is.na(y)]
    mod<-aov(y~trat+bloco)
    anova(mod)
    # ANOVA=ExpDes.pt::dbc(trat, bloco, y, quali = TRUE, mcomp = "tukey")
  })

  output$dados <- renderTable({
    as_tibble(ANOVA())
  })

  observe({
    req(input$semana)
    colunas <- dados() |> names()
    updateSelectInput(
      session,
      "trat",
      choices = colunas,
      selected = "NívelCálcio"
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
       selected = colunas[5]
         )
   })

   output$ridges <- renderPlot({
     req(input$coluna)
     dados() |>
       mutate(Trat = forcats::as_factor(.data[[input$trat]]),
              Rep = forcats::as_factor(.data[[input$rep]])) |>
       group_by(Trat,Rep) |>
       summarise(media_var = mean(.data[[input$coluna]], na.rm=TRUE)) |>
       ungroup() |>
       mutate(
          Trat = fct_reorder(Trat,
                                     media_var, .fun = median, na.rm = TRUE)
        ) |>
        ggplot(aes(x = media_var, y = Trat, fill = Trat)) +
        ggridges::geom_density_ridges(color = 'transparent', alpha = .6) +
        scale_fill_viridis_d() +
        theme_minimal() +
        labs(
          x = input$coluna,
          y = input$trat
        ) +
        theme(
          legend.position = 'none') +
        geom_vline(xintercept = 4)
   })

   output$boxplot <- renderPlot({
     req(input$coluna)
     dados() |>
       mutate(Trat = forcats::as_factor(.data[[input$trat]])) |>
       mutate(
         Trat = fct_reorder(Trat,
                            .data[[input$coluna]], .fun = median, na.rm = TRUE)
       ) |>
       ggplot(aes(x = Trat,
                  y = .data[[input$coluna]],fill=Trat)) +
       geom_boxplot() +
       scale_fill_viridis_d(alpha = .5) +
       theme_minimal()
   })

# Server - Regressao ------------------------------------------------------

   dados_temporais<-reactive({
     semanas <- excel_sheets(path = "DADOS_CALCIO.xlsx")
     obj<-purrr::map_df(semanas,
                        ~lendo("DADOS_CALCIO.xlsx",.x),
                        .id = "semanas")
     obj
   })

     observe({
       vars_x <- dados_temporais() |> select(-semanas) |>  names()
       updateSelectInput(
         session,
         "var_x",
         choices = vars_x,
         selected = "FREQ_MED"
       )

       updateSelectInput(
         session,
         "var_y",
         choices = vars_x,
         selected = "FFT_R"
       )
     })



   output$dispersao <- renderPlot({
     req(input$var_x,input$var_y)

     x<-dados_temporais() |>
       filter(semanas %in% input$filtro_semana) |>
       pull(input$var_x)

     y<-dados_temporais() |>
       filter(semanas %in% input$filtro_semana) |>
       pull(input$var_y)
     mod <- lm(y~x)
     ANOVA<-summary.lm(mod)
     R2 <- round(ANOVA$r.squared,4)

     dados_temporais() |>
       filter(semanas %in% input$filtro_semana) |>
       select(.data[[input$var_x]],.data[[input$var_y]],semanas) |>
       drop_na() |>
       ggplot(aes_string(x=input$var_x,y=input$var_y))+
       geom_point(aes(color=semanas),size=4)+
       geom_smooth(method = "lm") +
       # labs(title = paste0("R² = ",R2))+
       theme_minimal()+
       stat_regline_equation(aes(
        label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~")))

   })

}

shinyApp(ui, server)


