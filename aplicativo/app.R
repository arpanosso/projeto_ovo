library(shinydashboard)
library(tidyverse)
library(ggdendro)
library(readxl)
library(shiny)
library(tidyr)
library(ggridges)
library(ggpubr)
library(vegan)
library(forcats)

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
                    width = 6,
                    checkboxGroupInput(
                      inputId = "filtro_semana",
                      label = "Selecione a semana",
                      choices = 1:9,
                      inline = TRUE,
                      selected = 1:9
                    )
                  ),
                  column(
                    width = 6,
                    checkboxGroupInput(
                      inputId = "filtro_calcio",
                      label = "Tratamento",
                      choices = 1:7,
                      selected = 1:7,
                      inline=TRUE
                    )
                  ),
                  column(
                    width=12,
                    plotOutput("dispersao")
                  )
                )
              ),
              box(
                width = 6,
                title = "Agrupamento por Semana",
                solidHeader = TRUE,
                status = "primary",
                height = "600px",
                fluidRow(
                  column(
                    width = 12,
                    plotOutput("dispersao_wrap")
                  )
                )
              )
            ),
            fluidRow(
              box(
                width = 12,
                title = "Agrupamento por Tratamento",
                solidHeader = TRUE,
                status = "primary",
                fluidRow(
                  column(
                    width = 12,
                    plotOutput("dispersao_wrap_trat")
                  )
                )
              )
            )
          )
        )
      ),

# UI - multivariada -------------------------------------------------------
      tabItem(
        tabName = "multivariada",
        fluidRow(
          column(
            width = 12,
            h1("Análise Multivariada")
          )
        ),
        hr(style = "border-top: 1px solid black;"),
        br(),
        fluidRow(
          box(
            width = 12,
            title = "Seleção da variáveis",
            solidHeader = TRUE,
            status = "primary",
            fluidRow(
              column(
                width = 6,
                selectizeInput(
                  inputId = "lista_multivaria",
                  label = "Selecione as variáveis",
                  choices = "",
                  selected = "",
                  multiple = TRUE,
                  options = NULL
                )
              ),
              column(
                width = 6,
                selectizeInput(
                  inputId = "semana_multivaria",
                  label = "Selecione as semanas",
                  choices = c("TODAS",semanas),
                  selected = semanas[1:2],
                  multiple = TRUE,
                  options = NULL
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                tableOutput("tabela_multivariada")
              )
            ),
            fluidRow(
              column(
                width = 6,
                plotOutput("biplot")
              ),
              column(
                width = 6,
                plotOutput("dendrograma")
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

    dados_temporais() |>
      filter(semanas %in% input$filtro_semana) |>
      filter(Tratamento %in% as.numeric(input$filtro_calcio)) |>
       select(.data[[input$var_x]],.data[[input$var_y]],semanas) |>
       drop_na() |>
       ggplot(aes_string(x=input$var_x,y=input$var_y))+
       geom_point(aes(color=semanas),size=4)+
       geom_smooth(method = "lm") +
       theme_minimal()+
       stat_regline_equation(aes(
        label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~")))
   })

   output$dispersao_wrap <- renderPlot({
     req(input$var_x)
     dados_temporais() |>
       select(.data[[input$var_x]],.data[[input$var_y]],semanas) |>
       drop_na() |>
       ggplot(aes_string(x=input$var_x,y=input$var_y))+
       geom_point(aes(color=semanas),size=4)+
       facet_wrap(~semanas) +
       geom_smooth(method = "lm") +
       theme_minimal()+
       stat_regline_equation(aes(
         label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~")))
   },height = 550)


   output$dispersao_wrap_trat <- renderPlot({
     req(input$var_x)
     #browser()
     dados_temporais() |>
       select(.data[[input$var_x]],.data[[input$var_y]],Tratamento) |>
       drop_na() |>
       mutate(Tratamento = as_factor(Tratamento)) |>
       ggplot(aes_string(x=input$var_x,y=input$var_y))+
       geom_point(aes(color=Tratamento),size=4)+
       facet_wrap(~Tratamento) +
       geom_smooth(method = "lm") +
       theme_minimal()+
       stat_regline_equation(aes(
         label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~")))
   },height = 540)

   observe({
     variaveis <- dados_temporais() |> names()
     updateSelectizeInput(
       session,
       "lista_multivaria",
       choices = variaveis[-c(1:4,10)],
       selected = variaveis[6:10]
     )
   })

# Server-multivariada ----------------------------------------------------

   base_multivariada <- reactive({
     req(input$lista_multivaria)

     if(sum(input$semana_multivaria == "TODAS") == 1){
       dados_temporais() |>
         select(input$lista_multivaria, Tratamento)
     } else {
       dados_temporais() |>
         mutate(semanas = paste("Semana 0", semanas, sep="")) |>
         filter(semanas %in% input$semana_multivaria) |>
         select(input$lista_multivaria, Tratamento)
     }
   })

   output$tabela_multivariada <- renderTable({

     da <- base_multivariada() |>
       drop_na()

     pca <- prcomp(da, scale.=T)
     mcor <- cor(da, pca$x)
     nomes_pcs <- colnames(mcor)
     mcor <- t(mcor)
     mcor <- as_tibble(mcor)
     mcor$PC <- nomes_pcs
     tab <- tibble(
       Autovalor=pca$sdev^2,
       Var_exp=Autovalor/sum(Autovalor),
       Var_exp_acum=cumsum(Var_exp)*100,
       mcor
     ) |> relocate(PC)
     tab
   })

   output$biplot<- renderPlot({
     rotulos <- (base_multivariada() |>
                         drop_na() |>
                         pull(Tratamento))
     my_biplot(base_multivariada() |>
                 drop_na() |>
                 select(-Tratamento),
               rotulos,"")
   })

   output$dendrograma<- renderPlot({
     rotulos <- (base_multivariada() |>
                   drop_na() |>
                   pull(Tratamento))
     da <- base_multivariada() |>
       drop_na() |>
       select(-Tratamento)
     rownames(da) <- paste0("T",rotulos,sep="_",1:length(rotulos))
     da_pad<- decostand(da,
                        method = "standardize",na.rm=TRUE)
     da_pad_euc<-vegdist(da_pad,"euclidean")
     da_pad_euc_ward<-hclust(da_pad_euc, method="ward.D")
     ggdendrogram(da_pad_euc_ward)
   })
}

shinyApp(ui, server)


