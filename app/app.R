library(shiny)
library(shinythemes)
library(bslib)
library(here)
library(tidyverse)
library(reactable)
library(corrplot)
library(knitr)
library(markdown)
library(plotly)
library(shinycssloaders)

# ========== DATA =========
fp_d2sm_heatmap <- "data/d4heatmap.rds"
dsud <- read_rds("data/dsud_simple.rds")

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

mainP_web1 <- fixedPage(
  withMathJax(includeMarkdown("www/tutorial1.md")),
  hr(),
  div(
    class = "footer",
    includeHTML("www/footer.html")
  )
)

mainP_web2 <- fixedPage(
  withMathJax(includeMarkdown("www/tutorial2.md")),
  hr(),
  div(
    class = "footer",
    includeHTML("www/footer.html")
  )
)

mainP_web3 <- fixedPage(
  withMathJax(includeMarkdown("www/tutorial3.md")),
  hr(),
  div(
    class = "footer",
    includeHTML("www/footer.html")
  )
)

navP_Doc <- navbarMenu("Tutorial",
                       tabPanel("Novel regulatory effect", mainP_web1),
                       tabPanel("Drug-cancer association", mainP_web2),
                       tabPanel("Heatmap of drug-cancer associations", mainP_web3),
                       tabPanel("Video", mainP_Doc))
# navP_Doc <- tabPanel("Tutorial", mainP_Doc)


# =========== TABLE ===========
sideP_TB <- sidebarPanel(
  radioButtons("db", "Select database", dsud$dbs, selected = "unappv_0.01_non_op_down"),
  width = 3
)

mainP_TB <- mainPanel(
  tabsetPanel(
    tabPanel("Database overview", withSpinner(reactableOutput("overview"), type=7)),
    tabPanel("Search",
             withSpinner(reactableOutput("tbl_data1"), type=7),
    ),
    tabPanel("Plots",
             conditionalPanel(condition="input.db != 'Sheet1'",
                              h4("Score distributions of different models"),
                              withSpinner(plotlyOutput("df1_replot"), type=7),
                              hr(),
                              fluidRow(
                                column(width = 4, h4("Correlation of models"), withSpinner(plotOutput("df1_clustermap"), type=7)),
                                column(width = 8, h4("Histogram of model scores"), withSpinner(plotlyOutput("df1_hist"), type=7))
                              ),
                              hr(),
                              h4("Score distributions of different models"),
                              withSpinner(plotlyOutput("df1_replot2"), type=7),
                              # ns = NS(NULL)
             ),
    ),
    tabPanel("Heatmap",
             conditionalPanel(condition="input.db != 'Sheet1'",
                              withSpinner(plotlyOutput("heatmapPlot", height="1200px"), type=7),
                              # ns = NS(NULL)
             ),
    ),
  ),
  width = 9
)

mainP_verse <- fluidPage(withSpinner(reactableOutput("tbl_data2"), type=7))

navP_TB <- navbarMenu("Novel regularoty effect",
                      tabPanel("Psmir", sidebarLayout(sideP_TB, mainP_TB)),
                      tabPanel("Verse", mainP_verse))

# navP_TB <- tabPanel("Novel regulatory effect",
#                     sidebarLayout(sideP_TB, mainP_TB))


# =========== DISEASE ============
sideP_disease <- sidebarPanel(
  selectizeInput("disease", "Select a disease", dsud$disease_names, selected = "acute myeloid leukemia"),
  p("Select a disease to show it's potential drugs."),
  width = 3
)

mainP_disease <- mainPanel(
  tabsetPanel(
    tabPanel("Table", h3("Connectivity score"), withSpinner(reactableOutput("cmap_score"), type=7), hr(), h3("miRNA profile of DeepsmirUD predictions"), withSpinner(reactableOutput("tbl_disease_dsu"), type=7), hr(), h3("miRNA profile of the disease"), withSpinner(reactableOutput("tbl_disease_cancer"), type=7)),
    tabPanel("Drug-cancer association heatmap", fluidRow(withSpinner(plotlyOutput("heatmap_d2sm", height="800px"), type=7)))
  ),
  width = 9
)

navP_Disease <- tabPanel("Drug-cancer association",
                         sidebarLayout(sideP_disease, mainP_disease))

# ============ MAIN UI =============
navP_DeepsmirUD <- tabPanel("Software", fixedPage(
  h3("DeepsmirUD software can be obtained from ", tags$a(href="https://github.com/2003100127/deepsmirud", target="_blank", "Github"), "."),
  br(),
  hr(),
  div(
    class = "footer",
    includeHTML("www/footer.html")
    )
  )
)

# ui <- navbarPage("DeepsmirUD-web", navP_Home, navP_Doc, navP_TB, navP_Disease, navP_DeepsmirUD,
ui <- navbarPage("DeepsmirUD-web", navP_Home, navP_Doc, navP_TB, navP_Disease, navP_DeepsmirUD,
                 theme = shinytheme("flatly"))
                 # position = "fixed-top",
                 # tags$style("body {padding-top: 70px;}"))


server <- function(input, output, session) {
  # ========== SEARCH ALL =============
  # -------------- search all: data -----------
  output$overview <- renderReactable({
    reactable(dsud[["db_overview"]])
  })

  df_data1 <- reactive({
    dsud[[input$db]][["df"]]
  })

  # ----------- search all: table ----------
  output$tbl_data1 <- renderReactable({
    df <- df_data1()
    if (input$db == "Sheet1") {
      reactable(df)
    } else {
      df_show <- df %>%
        mutate(across(where(is.numeric), round, 4)) %>%
        reactable(searchable = TRUE,
                  defaultPageSize = 10,
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
  }) %>%
    bindCache(input$db)

  # ------------ search all: figures ------------
  df4plot <- reactive({
    dsud[[input$db]][["df4plot"]]
  }) %>%
    bindCache(input$db)


  df1 <- reactive({
    dsud[[input$db]][["df1"]]
  }) %>%
    bindCache(input$db)

  # heatmap
  output$heatmapPlot <- renderPlotly({
    dsud[[input$db]][["heatmapPlot"]]
  }) %>%
    bindCache(input$db)

  # df1: replot
  output$df1_replot <- renderPlotly({
    dsud[[input$db]][["df1_replot"]]
  }) %>%
    bindCache(input$db)

  # df1: hist
  output$df1_hist <- renderPlotly({
    dsud[[input$db]][["df1_hist"]]
  }) %>%
    bindCache(input$db)

  # df1: clustermap
  output$df1_clustermap <- renderPlot({
    corrplot(cor(dsud[[input$db]][["df4plot"]]))
  }) %>%
    bindCache(input$db)

  # df1: replot2
  output$df1_replot2 <- renderPlotly({
    dsud[[input$db]][["df1_replot2"]]
  }) %>%
    bindCache(input$db)

  # =========== SEARCH VERSE ==============
  output$tbl_data2 <- renderReactable({
    df <- dsud[["df_verse"]] %>%
      mutate(across(where(is.numeric), round, 4))
    reactable(df,
              searchable = TRUE,
              defaultPageSize = 15,
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


  # ================ DISEASE: navP ============
  # ----------- disease: data -----------
  subset_disease <- reactive({
    dsud[["df_disease"]] %>%
      dplyr::filter(disease_name == input$disease)
  }) %>%
    bindCache(input$disease)

  subset_dsu <- reactive({
    dsud[["df_deepsmirud"]] %>%
      dplyr::filter(mirna_baseid %in% unique(subset_disease()$mirna_baseid)) %>%
      mutate(across(where(is.numeric), round, 4))
  }) %>%
    bindCache(input$disease)

  data_cmap_score <- reactive({
    dsud[["df_cmap"]][[input$disease]] %>%
      rownames_to_column("colnames_new") %>%
      left_join(dsud[["df_smname_mapping"]], by = "colnames_new") %>%
      dplyr::select(-c(colnames_new, pValue)) %>%
      column_to_rownames("colnames_old") %>%
      mutate(Score = Score / 2) %>%  # normalize score to [-1,1]
      mutate(across(where(is.numeric), round, 4)) %>%
      dplyr::filter(pAdjValue <= 0.05) %>%
      arrange(Score) %>%
      rownames_to_column("compound_name")
  }) %>%
    bindCache(input$disease)

  # ----------- disease vis: Connectivity score ----------
  output$cmap_score <- renderReactable({
    data_cmap_score() %>%
      # mutate(baidu=compound_name) %>%
      reactable(searchable = FALSE,
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
  }) %>%
    bindCache(input$disease)

  output$tbl_disease_dsu <- renderReactable({
    tbl_sub <- subset_dsu()
    tbl_parent <- tbl_sub %>%
      dplyr::select(sm_name, cid) %>%
      dplyr::filter(sm_name %in% unique(data_cmap_score()$compound_name)) %>%
      dplyr::distinct()

    reactable(tbl_parent, filterable = T, details = function(index) {
      cid_miRNA_profile <- tbl_sub[tbl_sub$sm_name == tbl_parent$sm_name[index],]
      htmltools::div(style = "padding: 1rem",
                     reactable(cid_miRNA_profile,
                               outlined = TRUE,
                               wrap = F,
                               resizable = T,
                               highlight = T,
                               filterable = T,
                               sortable = T,
                               searchable = F)
      )
    })
  }) %>%
    bindCache(input$disease)

  output$tbl_disease_cancer <- renderReactable({
    subset_disease() %>%
      # dplyr::select(-pubmed_article) %>%
      # distinct() %>%
      reactable(searchable = FALSE,
                sortable = TRUE,
                filterable = TRUE,
                highlight = TRUE,
                resizable = TRUE,
                wrap = FALSE)
  }) %>%
    bindCache(input$disease)

  # ------------ disease: heatmap --------------
  output$heatmap_d2sm <- renderPlotly({
    readRDS(fp_d2sm_heatmap)
  }) %>%
    bindCache(fp_d2sm_heatmap)
}

shinyApp(ui = ui, server = server)
