library(shiny)
library(shinythemes)
library(bslib)
library(here)
library(tidyverse)
library(readxl)
library(reactable)
library(heatmaply)
library(corrplot)

# ========== DATA =========
fpath1 <- "data/data1.xlsx"
fpath2 <- "data/data2.xlsx"
fp_disease <- "data/disease.xlsx"
fp_cmap <- "data/main/GSEAweight1Score.RDS"
fp_d2sm_heatmap <- "data/data4heatmap.rds"
fp_profile <- "data/profiles_deepsmirud.Rdata"
df_disease <- read_excel(fp_disease, sheet = "mircancer")
df_dsu <- read_excel(fp_disease, sheet = "deepsmirud")
disease_names <- sort(unique(df_disease$disease_name))
df_cmap <- readRDS(fp_cmap)
d2sm_heatmap <- readRDS(fp_d2sm_heatmap)
load(fp_profile)

# ============ HOME ========
sideP_Home <- navlistPanel(fluid = F)
main_Home <- fixedPage(
  h2("Background"),
  p("DeepsmirUD-web catalogues predictions of SM-mediated regulatory effects upon miRNAs from all relations used in the DeepsmirUD paper. By trawling through miRNA-disease data from miRCancer, we also established a database of drug–cancer associations to provide potential therapeutics based on the SM-miR upregulation and downregulation profiles predicted by DeepsmirUD."),
  h2("Introduction"),
  p("MicroRNAs (miRNAs) are a class of non-coding RNAs, which are vital to biological activities by primarily virtue of their post-transcriptionally regulatory mechanisms. miRNAs are often known to downregulate the expression of genes by inhibiting translation or promoting the degradation of target mRNAs, thereby exerting an impact on the gene regulatory pathways to either remodel homeostasis or give rise to malignancy such as leukemia and osteoarthritis. It has been reported that up to thousands of mRNAs can be targeted by a single miRNA and vice versa. Therefore, there are a myriad of gateways for miRNAs to influence the regulated gene pathways. Growing evidence has particularly suggested that miRNAs play important roles in cancer and their expression abnormalities can lead to a variety of diseases or supress tumor progression. In this regard, therapeutics can be achieved by targeting oncogenic miRNAs with potential drug molecules to alter their expression. Existing experiments have revealed that many small molecules (SMs) have held great promise as pharmaceuticals to target the miRNAs to modulate their expression thereafter."),
  p("According to the SM2miR database built based on more than 2000 publications, only 1.14% of all possible SM-miR pairs interwoven with 1492 unique miRNAs and 212 unique small molecules (after pre-processing) are catalogued to be experimentally verified (Figure 1). It is therefore in addition to the experimental profiling that much attention has been paid for designing computational techniques to speed up the inference of binding. A precise understanding of the types of SM-mediated regulation on miRNAs may provide direct or indirect evidence for cancer pathogenesis and therapeutics in the sense that the oncogenic signalling pathways regulated by the miRNAs are affected. Therefore, the efficient identification and the follow-up analysis of disease-associated miRNAs targeted by small molecule drugs with a measurable influence on the miRNA expression are becoming increasingly important and have emerged as a new therapeutic treatment in miRNA pharmacogenomics."),
  HTML('<center><img src="fig1.svg" width=95%></center>'),
  p("Figure 1. Data wrangling of upregulated and downregulated relations. a. Inference of potentially upregulated and downregulated relations using guilt-by-association. b. Flowchart of generating the Train, Test, TestSim, TestRptMIR, TestRptSM, TestUniqMIR, and TestUniqSM datasets."),
  p("Advances in deep learning have spawned ample opportunity to promote biological applications and discoveries such as protein structural and functional prediction. In order for maximization of method performance specific to the regulatory effect prediction, we attempted to seek prime solutions from the computer-vision and speech-recognition fields by utilizing both well-established models including AlexNet, MobileNetV2, Transformer-based ConvMixer, ResNet18, ResNet50, attention-based SCAResNet18, and our self-assembly architectures, including convolutional neural networks (CNNs), recurrent neural networks (RNNs), bidirectional recurrent neural networks (BiRNNs), depth-wise and separable neural networks, the fusion of long short-term memory (LSTM) neural networks and CNNs, and sequence-to-sequence (Seq2Seq) neural networks. These models, the vast majority of which can be trained fast due to the residual connection in design or required parameter numbers, allow relatively full-scale examination and comparison of performance from shallow to ultradeep layers visually and semantically."),
  HTML('<center><img src="fig2.svg" width=95%></center>'),
  p("Figure 2. Workflow of predicting SM-mediated regulatory effects on miRNA expression by deep learning algorithms. In box Feature preparation, the integers stand for the length of features. The feature vector stands for a concatenation of miRNA and small-molecule features."),
  p("A comprehensive analysis was made to opt for deep learning models optimized sufficiently and properly based on well-curated SM-miR relations and biophysical and biochemical features (Figure 2). In the context of achieving competing results (at least AUC 0.80-0.92) by individual models, the final ensemble model, DeepsmirUD, can tap into their variance-reduced prediction values to obtain an even further boosted performance gain up to ~2% in AUC and ~1-2% in AUCPR."),
  hr(),
  div(
    class = "footer",
    includeHTML("www/footer.html")
  )
)
# navP_Home <- tabPanel("Home", sidebarLayout(sideP_Home, main_Home))
navP_Home <- tabPanel("Home", main_Home)



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
    hr(),
    div(
      class = "footer",
      includeHTML("www/footer.html")
    )

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
    hr(),
    div(
      class = "footer",
      includeHTML("www/footer.html")
    )
  ),
  width = 9
)

navP_Disease <- tabPanel("Drug-cancer association", sidebarLayout(sideP_disease, mainP_disease))



# =========== SUPPLEMENTARY ===========
sideP_SP <- sidebarPanel(
  # uiOutput("data1_sheets"),
  # uiOutput("ds1_col1"),
  # uiOutput("ds1_col4"),
  # width = 2
)

mainP_SP <- mainPanel(
  # tabsetPanel(
  #   tabPanel("Data1", reactableOutput("tbl_data1")),
  #   tabPanel("Data2", reactableOutput("tbl_data2")),
  #   tabPanel("heatmap", plotlyOutput("heatmapPlot", height="1100px")),
  #   tabPanel("heatmap_d2sm", fluidRow(plotlyOutput("heatmap_d2sm", height="500px")))),
  # width = 10
)

navP_SP <- tabPanel("Instruction", sidebarLayout(sideP_SP, mainP_SP))


# ============ MAIN UI =============
# ui <- navbarPage("DeepsmirUD_web", navP_Home, navP_TB, navP_Disease, theme = bs_theme(bootswatch="cerulean"))
ui <- navbarPage("DeepsmirUD_web", navP_Home, navP_SP, navP_TB, navP_Disease, theme = shinytheme("cerulean"))


# ------------ Server --------
server <- function(input, output) {
  # ========= DYNAMIC UI ==============
  output$sup_left <- renderUI({
    selectInput("data1_sheets", "Select sheets in data 1", excel_sheets(fpath1))
  })

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
  output$disease <- renderUI({
    selectInput("disease", "Select disease", disease_names)
  })

  # =========== REACTIVE ============
  df_data1 <- reactive({
    df <- read_excel(fpath1, sheet = input$data1_sheets)
  })

  df_data2 <- reactive({df <- read_excel(fpath2)})



  # ========== OUTPUT =============
  # ----------- Data1 ---------------
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

  # -------------- Data2 --------------
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
