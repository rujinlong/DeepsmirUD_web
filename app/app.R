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
library(shinycssloaders)

# ========== DATA =========
fpath1 <- "data/data1.xlsx"
fpath2 <- "data/data2.xlsx"
fp_disease <- "data/disease.xlsx"
fp_cmap <- "data/main/GSEAweight1Score.rds"
fp_d2sm_heatmap <- "data/data4heatmap.rds"
fp_profile <- "data/profiles_deepsmirud.rdata"
df_disease <- read_excel(fp_disease, sheet = "mircancer")
disease_names <- sort(unique(df_disease$disease_name))
df_cmap <- readRDS(fp_cmap)
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
  selectizeInput('ds1_col1', label= "Filter by small molecule (compound)", choices = NULL, selected = NULL),
  selectizeInput('ds1_col4', label= "Filter by miRNA", choices = NULL, selected = NULL),
  width = 3
)

mainP_TB <- mainPanel(
  tabsetPanel(
    tabPanel("Search Psmir",
             withSpinner(reactableOutput("tbl_data1")),
             hr(),
             conditionalPanel(condition="input.data1_sheets != 'Sheet1'",
                              h4("Score distributions of different models"),
                              withSpinner(plotlyOutput("df1_replot")),
                              hr(),
                              fluidRow(
                                column(width = 4, h4("Correlation of models"), withSpinner(plotOutput("df1_clustermap"))),
                                column(width = 8, h4("Histogram of model scores"), withSpinner(plotlyOutput("df1_hist")))
                                ),
                              hr(),
                              h4("Score distributions of different models"),
                              withSpinner(plotlyOutput("df1_replot2")),
                              hr(),
                              h4("Heatmap"),
                              withSpinner(plotlyOutput("heatmapPlot", height="900px")),
                              # ns = NS(NULL)
               ),
             ),
    tabPanel("Search Verse", withSpinner(reactableOutput("tbl_data2")))
  ),
  width = 9
)

navP_TB <- tabPanel("Novel regulatory effect",
                    sidebarLayout(sideP_TB, mainP_TB))


# =========== DISEASE ============
sideP_disease <- sidebarPanel(
  withSpinner(uiOutput("disease")),
  p("Select a disease to show it's potential drugs."),
  width = 3
)

mainP_disease <- mainPanel(
  tabsetPanel(
    tabPanel("Table", h3("Connectivity score"), withSpinner(reactableOutput("cmap_score")), hr(), h3("miRNA profile of DeepsmirUD predictions"), withSpinner(reactableOutput("tbl_disease_dsu")), hr(), h3("miRNA profile of the disease"), withSpinner(reactableOutput("tbl_disease_cancer"))),
    tabPanel("Drug-cancer association heatmap", fluidRow(withSpinner(plotlyOutput("heatmap_d2sm", height="800px"))))
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
  # ========== Dynamic UI:  TABLE side ==========
  output$data1_sheets <- renderUI({
    selectInput("data1_sheets", "Select database", excel_sheets(fpath1))
  })

  observeEvent(df_data1(), {
    updateSelectizeInput(session, "ds1_col1",
                         choices=sort(unique(df_data1()[["Small molecule"]])),
                         server=TRUE,
                         selected=F)
  })

  observeEvent(df_data1(), {
    updateSelectizeInput(session, "ds1_col4",
                         choices=sort(unique(df_data1()[["MiRNA"]])),
                         server=TRUE,
                         selected=F)
  })

  # ============ Dynamic UI: DISEASE side =========
  output$disease <- renderUI({
    selectInput("disease", "Select disease", disease_names)
  })

  # ========== SEARCH ALL =============
  # -------------- search all: data -----------
  df_data1 <- reactive({
    df <- read_excel(fpath1, sheet = input$data1_sheets)
  })

  # ----------- search all: table ----------
  output$tbl_data1 <- renderReactable({
    df <- df_data1()
    if (input$data1_sheets == "Sheet1") {
      reactable(df)
    } else {
      df_show <- df %>%
        mutate(across(where(is.numeric), round, 4))
      if (input$ds1_col1 != "" & input$ds1_col4 != "") {
        print("cond1")
        df_show <- df_show %>% dplyr::filter(`Small molecule` %in% input$ds1_col1, MiRNA %in% input$ds1_col4)
      } else if (input$ds1_col1 == "" & input$ds1_col4 != "") {
        print("cond2")
        df_show <- df_show %>%dplyr::filter(MiRNA %in% input$ds1_col4)
      } else if (input$ds1_col1 != "" & input$ds1_col4 == "") {
        print("cond3")
        df_show <- df_show %>%dplyr::filter(`Small molecule` %in% input$ds1_col1)
      }
      df_show %>%
        # reactable()
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

  # ------------ search all: figures ------------
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
      geom_histogram(binwidth = 1)
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
      geom_point(alpha=0.3)
    ggplotly(p)
  })

  # =========== SEARCH VERSE ==============
  # ----------- search verse: data --------------
  df_data2 <- reactive({df <- read_excel(fpath2)})

  # ----------- search verse: table -------------
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


  # ================ DISEASE: navP ============
  # ----------- disease: data -----------
  subset_disease <- reactive({
    df_disease %>%
      dplyr::filter(disease_name == input$disease)
  })

  subset_dsu <- reactive({
    read_excel(fp_disease, sheet = "deepsmirud") %>%
      dplyr::filter(mirna_baseid %in% unique(subset_disease()$mirna_baseid)) %>%
      mutate(across(where(is.numeric), round, 4))
  })

  data_cmap_score <- reactive({
    df_cmap[[input$disease]] %>%
      rownames_to_column("colnames_new") %>%
      left_join(df_smname_mapping, by = "colnames_new") %>%
      dplyr::select(-c(colnames_new, pValue)) %>%
      column_to_rownames("colnames_old") %>%
      mutate(Score = Score / 2) %>%  # normalize score to [-1,1]
      mutate(across(where(is.numeric), round, 4)) %>%
      dplyr::filter(pAdjValue <= 0.05) %>%
      arrange(Score) %>%
      rownames_to_column("compound_name")
  })

  # ----------- disease vis: Connectivity score ----------
  output$cmap_score <- renderReactable({
    data_cmap_score() %>%
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

  output$tbl_disease_dsu <- renderReactable({
    tbl_sub <- subset_dsu()
    tbl_parent <- tbl_sub %>%
      dplyr::select(sm_name, cid) %>%
      filter(sm_name %in% unique(data_cmap_score()$compound_name)) %>%
      distinct()

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
                               searchable = T)
      )
    })
  })

  output$tbl_disease_cancer <- renderReactable({
    subset_disease() %>%
      # dplyr::select(-pubmed_article) %>%
      # distinct() %>%
      reactable(searchable = TRUE,
                sortable = TRUE,
                filterable = TRUE,
                highlight = TRUE,
                resizable = TRUE,
                wrap = FALSE)
  })

  # ------------ disease: heatmap --------------
  output$heatmap_d2sm <- renderPlotly({
    readRDS(fp_d2sm_heatmap)
  })
}

shinyApp(ui = ui, server = server)
