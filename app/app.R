library(shiny)
library(shinythemes)
library(here)
library(tidyverse)
library(readxl)
library(reactable)
library(heatmaply)
library(corrplot)

# ========== DATA =========
fpath1 <- "data/data1.xlsx"
fpath2 <- "data/data2.xlsx"

# ------------- UI ---------------
sideP <- sidebarPanel(
  uiOutput("data1_sheets"),
  uiOutput("ds1_col1"),
  uiOutput("ds1_col4"),
  width = 3
)
# ---------- main UI --------
mainP <- mainPanel(
  tabsetPanel(
    tabPanel("Data1", reactableOutput("tbl_data1")),
    tabPanel("Data2", reactableOutput("tbl_data2")),
    tabPanel("heatmap", plotlyOutput("heatmapPlot", height="1100px")),
    tabPanel("plots",
             plotlyOutput("df1_replot"),
             plotlyOutput("df1_hist"),
             fluidRow(column(width = 6, plotOutput("df1_clustermap")), column(width=6, plotlyOutput("df1_replot2")))),
  ),
  width = 9
)

navP_Tables <- tabPanel("Tables", sidebarLayout(sideP, mainP))
navP_Docs <- tabPanel("Documentation", mainPanel())

ui <- navbarPage("DeepsmirUD", navP_Tables, navP_Docs, theme = shinytheme("cerulean"))

# ------------ Server --------
server <- function(input, output) {
  # ========= DYNAMIC UI ==============
  output$data1_sheets <- renderUI({
    selectInput("data1_sheets", "Select sheets in data 1", excel_sheets(fpath1))
  })
  output$ds1_col1 <- renderUI({
    selectInput("ds1_col1", "cid",
                sort(unique(df_data1()$cid)),
                multiple = TRUE,
                selectize = T)
  })
  output$ds1_col4 <- renderUI({
    selectInput("ds1_col4", "mirBase",
                sort(unique(df_data1()$Mirbase)),
                multiple = TRUE,
                selectize = T)
  })

  # =========== REACTIVE ============
  df_data1 <- reactive({
    df <- read_excel(fpath1, sheet = input$data1_sheets)
    })

  df_data2 <- reactive({df <- read_excel(fpath2)})

  # ========== OUTPUT =============
  output$tbl_data1 <- renderReactable({
    df <- df_data1()
    if (!is.null(input$ds1_col1) & !is.null(input$ds1_col4)) {
      print("cond1")
      df <- df %>% dplyr::filter(cid %in% input$ds1_col1, Mirbase %in% input$ds1_col4)
    } else if (is.null(input$ds1_col1) & !is.null(input$ds1_col4)) {
      print("cond2")
      df <- df %>%dplyr::filter(Mirbase %in% input$ds1_col4)
    } else if (!is.null(input$ds1_col1) & is.null(input$ds1_col4)) {
      print("cond3")
      df <- df %>%dplyr::filter(cid %in% input$ds1_col1)
    }
    if (input$data1_sheets == "Sheet1") {
      reactable(df)
    } else {
      reactable(df,
                searchable = TRUE,
                sortable = TRUE,
                filterable = TRUE,
                highlight = TRUE,
                columns = list(
                  predicted_type = colDef(
                    style = function(value) {
                      if (value == "Upregulation") {
                        color <- "#008000"
                      } else if (value == "Downregulation") {
                        color <- "#e00000"
                      }
                      list(color = color, fontWeight = "bold")
                    }
                  )))
    }
  })

  output$tbl_data2 <- renderReactable({
    df <- df_data2()
    reactable(df,
              searchable = TRUE,
              sortable = TRUE,
              filterable = TRUE,
              highlight = TRUE)
  })

  df4plot <- reactive({
    df_data1() %>%
      dplyr::select(c(AlexNet, BiRNN, RNN, Seq2Seq, CNN, ConvMixer64, DSConv, LSTMCNN, MobileNetV2, ResNet18, ResNet50, SCAResNet18))
    # save(df, file = "~/df.Rdata")
  })

  df1 <- reactive({
    df <- df4plot()
    df %>%
      pivot_longer(names_to = "variable",
                   values_to = "value",
                   cols = colnames(.)) %>%
      mutate(a = sort(rep(1:nrow(df), ncol(df))) - 1)
  })

  # heatmap
  output$heatmapPlot <- renderPlotly({
    df4plot() %>% heatmaply()
  })

  # df1: replot
  output$df1_replot <- renderPlotly({
    p <- df1() %>%
      ggplot(aes(x=a, y=value, color=variable)) +
      geom_line() +
      facet_wrap("variable")
    ggplotly(p)
  })

  # df1: hist
  output$df1_hist <- renderPlotly({
    p <- df1() %>%
      mutate(value = log10(value)) %>%
      ggplot(aes(x=value, fill=variable)) +
      geom_histogram()
    ggplotly(p)
  })

  # df1: clustermap
  output$df1_clustermap <- renderPlot({
    corrplot(cor(df4plot()))
  })

  # df1: replot2
  output$df1_replot2 <- renderPlotly({
    p <- df1() %>%
      ggplot(aes(x=a, y=value, size = value, color = variable)) +
      geom_point()
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)
