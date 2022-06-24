library(shiny)
library(shinythemes)
library(bslib)
library(here)
library(tidyverse)
library(readxl)
library(reactable)
library(heatmaply)
library(corrplot)
library(knitr)
library(markdown)

# ========== DATA =========
fpath1 <- "data/data1.xlsx"
fpath2 <- "data/data2.xlsx"
fp_disease <- "data/disease.xlsx"
fp_cmap <- "data/main/gseaweight1score.rds"
fp_d2sm_heatmap <- "data/data4heatmap.rds"
fp_profile <- "data/profiles_deepsmirud.rdata"
df_disease <- read_excel(fp_disease, sheet = "mircancer")
df_dsu <- read_excel(fp_disease, sheet = "deepsmirud")
disease_names <- sort(unique(df_disease$disease_name))
df_cmap <- readRDS(fp_cmap)
d2sm_heatmap <- readRDS(fp_d2sm_heatmap)
load(fp_profile)

# ============ HOME ========
main_Home <- fixedPage(
  withMathJax(includeMarkdown("www/homepage.md")),
  hr(),
  div(
    class = "footer",
    includeHTML("www/footer.html")
  )
)
navP_Home <- tabPanel("Home", main_Home)


# =========== TUTORIAL ===========
mainP_Doc <- fixedPage(
  withMathJax(includeMarkdown("www/tutorial.md")),
  hr(),
  div(
    class = "footer",
    includeHTML("www/footer.html")
  )
)

navP_Doc <- tabPanel("Tutorial", mainP_Doc)


# =========== TABLE ===========
sideP_TB <- sidebarPanel(
  uiOutput("data1_sheets"),
  uiOutput("ds1_col1"),
  uiOutput("ds1_col4"),
  width = 3
)

mainP_TB <- mainPanel(
  tabsetPanel(
    tabPanel("Search all",
             reactableOutput("tbl_data1"),
             plotlyOutput("df1_replot"),
             fluidRow(
               column(width = 6, plotlyOutput("df1_hist")),
               column(width=6, plotlyOutput("df1_replot2"))
               ),
             fluidRow(
               column(width = 8, plotlyOutput("heatmapPlot", height="1100px")),
               column(width = 4, plotOutput("df1_clustermap"))
               ),
             ),
    tabPanel("Search Verse", reactableOutput("tbl_data2")),
  ),
  width = 9
)

navP_TB <- tabPanel("Regulatory effect", sidebarLayout(sideP_TB, mainP_TB))


# =========== DISEASE ============
sideP_disease <- sidebarPanel(
  uiOutput("disease"),
  width = 3
)

mainP_disease <- mainPanel(
  tabsetPanel(
    tabPanel("Connectivity score", reactableOutput("cmap_ks"), reactableOutput("cmap_zh"),reactableOutput("cmap_g0"),reactableOutput("cmap_g1"),reactableOutput("cmap_g2"),reactableOutput("cmap_xs")),
    tabPanel("Drug-cancer association heatmap", fluidRow(plotlyOutput("heatmap_d2sm", height="800px"))),
    tabPanel("Disease", reactableOutput("tbl_disease_cancer"), reactableOutput("tbl_disease_dsu")),
  ),
  width = 9
)

navP_Disease <- tabPanel("Drug-cancer association", sidebarLayout(sideP_disease, mainP_disease))


# ============ MAIN UI =============
# ui <- navbarPage("DeepsmirUD_web", navP_Home, navP_TB, navP_Disease, theme = bs_theme(bootswatch="cerulean"))
ui <- navbarPage("DeepsmirUD-web", navP_Home, navP_Doc, navP_TB, navP_Disease, theme = shinytheme("cerulean"))


# ------------ Server --------
server <- function(input, output) {
  # ========== Dynamic UI:  TABLE side ==========
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

  # ============ Dynamic UI: DISEASE side =========
  output$disease <- renderUI({
    selectInput("disease", "Select disease", disease_names)
  })

  # -------------- Data: search all -----------
  df_data1 <- reactive({
    df <- read_excel(fpath1, sheet = input$data1_sheets)
  })

  # -------------- Data: search Verse ---------
  df_data2 <- reactive({df <- read_excel(fpath2)})

  # ========== OUTPUT =============
  # ----------- Render: search all ---------------
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
      df %>%
        mutate(across(where(is.numeric), round, 4)) %>%
        reactable(searchable = TRUE,
                  sortable = TRUE,
                  filterable = TRUE,
                  highlight = TRUE,
                  wrap = FALSE,
                  resizable = TRUE,
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

  # -------------- Render: search Verse --------------
  output$tbl_data2 <- renderReactable({
    df <- df_data2() %>%
      mutate(across(where(is.numeric), round, 4))
    reactable(df,
              searchable = TRUE,
              sortable = TRUE,
              wrap = FALSE,
              resizable = TRUE,
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
  })

  # ----------- Data: disease -----------
  subset_disease <- reactive({
    df_disease %>%
      dplyr::filter(disease_name == input$disease)
  })

  subset_dsu <- reactive({
    df_dsu %>%
      dplyr::filter(mirna_baseid %in% unique(subset_disease()$mirna_baseid))
  })

  output$tbl_disease_cancer <- renderReactable({
    subset_disease() %>%
      dplyr::select(-pubmed_article) %>%
      distinct() %>%
      reactable(searchable = TRUE,
                sortable = TRUE,
                filterable = TRUE,
                highlight = TRUE)
  })


  output$tbl_disease_dsu <- renderReactable({
    subset_dsu() %>%
      reactable(searchable = TRUE,
                sortable = TRUE,
                filterable = TRUE,
                highlight = TRUE)
  })


  # ================ navP_Disease ============
  # ----------- Tab: Connectivity score ----------
  output$cmap_ks <- renderReactable({
    df_cmap[[input$disease]] %>%
      rownames_to_column("colnames_new") %>%
      left_join(df_smname_mapping, by = "colnames_new") %>%
      dplyr::select(-c(colnames_new, pValue)) %>%
      column_to_rownames("colnames_old") %>%
      mutate(Score = Score / 2) %>%  # normalize score to [-1,1]
      mutate(across(where(is.numeric), round, 4)) %>%
      dplyr::filter(pAdjValue <= 0.05) %>%
      arrange(Score) %>%
      rownames_to_column("compound_name") %>%
      # mutate(baidu=compound_name) %>%
      reactable(searchable = TRUE,
                sortable = TRUE,
                filterable = TRUE,
                highlight = TRUE,
                columns = list(
                  compound_name = colDef(cell = function(value, index) {
                    url <- sprintf('https://www.google.com/search?q="%s" +%s', input$disease, value)
                    htmltools::tags$a(href = url, target = "_blank", as.character(value))
                  })
                  # baidu = colDef(cell = function(value, index) {
                  #   url <- sprintf('https://www.baidu.com/s?wd="%s" +%s', input$disease, value)
                  #   htmltools::tags$a(href = url, target = "_blank", as.character(value))
                  # })
                  ))
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

  output$heatmap_d2sm <- renderPlotly({
    d2sm_heatmap
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
