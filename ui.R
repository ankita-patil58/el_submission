library(shiny)
library(shinydashboard)
library(mapGCT)
library(DT)
library(ggplot2)
library(shinyjs)
library(shinyhelper)
library(edgeR)
library(reactable)
library(plotly)
library(shinyalert)


ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Elucidata",
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Sales Dept",
                                 message = "Sales are steady this month."
                               ))),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("File Upload", tabName = "file_upload", icon = icon("upload")),
      menuItem("Normalization", tabName = "normalization", icon = icon("magnifying-glass-chart")),
      menuItem("PCA", tabName = "pca_analysis", icon = icon("chart-simple"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                column(12,
                       box(status = 'primary', width = 12, title = 'Application Overview', solidHeader = TRUE,
                           div(style = "font-size: 16px; line-height: 1.6;",
                               h3(style = "color: #007bff; font-weight: bold;", "Overview of the Application"),
                               p("This application is a bioinformatics tool designed for gene expression analysis. It takes a GCT (Gene Cluster Text) file as input and performs essential data preprocessing and visualization steps to help you understand your gene expression data better."),
                               p("The key functionalities of the application are as follows:"),
                               tags$ul(
                                 tags$li("Input a GCT file that contains the gene expression matrix (mat) and sample description (cdesc)."),
                                 tags$li("Perform Principal Component Analysis (PCA) using Coefficient of Variation-based approach to analyze variance in the data."),
                                 tags$li("Apply Quantile Normalization to adjust gene expression distributions across samples for comparability."),
                                 tags$li("Generate visualizations such as PCA plots and Scree plots to explore the variance and structure of the data.")
                               ),
                               h4(style = "color: #28a745; font-weight: bold;", "Key Uses of This Application"),
                               p("The application serves several important use cases in bioinformatics research and gene expression analysis:"),
                               tags$ul(
                                 tags$li("Data Normalization: Apply Quantile Normalization to make sure that expression values are comparable across samples."),
                                 tags$li("Dimensionality Reduction: Perform PCA to reduce the dimensionality of your data while retaining most of the variance."),
                                 tags$li("Exploratory Data Analysis: PCA and Scree plots help to visualize the underlying structure of data and uncover potential outliers or patterns."),
                                 tags$li("Gene Expression Analysis: Ideal for researchers who want to preprocess and visualize gene expression data for further biological interpretation.")
                               ),
                               p("By using this tool, you can efficiently analyze gene expression datasets and uncover meaningful insights that could be crucial for your research.")
                           )
                       )
                )
              )
    ),
      tabItem(tabName = "file_upload",
              fluidRow(
                column(12,
                 box(status='info',width=12, title = 'Upload Section', footer = 'Make sure you have GCT file with cdesc & mat in it',solidHeader = TRUE, 
                fileInput("gct_file", "To run program upload your GCT file", accept = ".gct"),
                br()
                )
                )
              ),
              fluidRow(
                column(12,
                       box(width=12,
                valueBoxOutput("genes"),
                valueBoxOutput("sample"),
                valueBoxOutput("tissue")
              ))),
              fluidRow(
                column(12,
                       box(
                         title = "Uploaded GCT data", width = 12,
                         reactableOutput("data_table"),
                         downloadButton("download_gene_data", "Download Matrix Data")
                       )
                )
              )
      ),
      tabItem(tabName = "normalization",
              fluidRow(
                column(12,
                  box(status='primary',width=12,solidHeader = TRUE, 
                         title = "Normalisation",
                         selectInput("normalization_method", "Normalization Method",
                                     choices = c("CPM", "Quantile")),
                         checkboxInput("log_transform", "Apply Log Transformation", value = FALSE),
                         br(),
                         actionButton("apply_normalization", "Apply Normalization"),
                         br()
                )
                )
              ),
              fluidRow(
                column(12,
                       box(
                         title = "Normalized Data", width = 12,
                         hr(),
                         reactableOutput("normalized_data"),
                         downloadButton("download_normalized_data", "Download Normalized Data")
                       )
                )
          )
      ),
      tabItem(tabName = "pca_analysis",
              fluidRow(
                column(12,
                       box(status='warning',solidHeader = TRUE, 
                         title = "Select PCA Parameters", width = 6,
                         numericInput("top_counts", "Top Count", value = 500, min = 500),
                         selectInput("pc_x", "PC1", choices = NULL),
                         selectInput("pc_y", "PC2", choices = NULL),
                         selectInput("pc_col", "Color by", choices = NULL),
                         actionButton("pca_plot_btn", "Plot PCA"),
                         br(),
                         br(),
                         downloadButton("download_pca_plot", "Download PCA Plot")
                         
                       ),
                       box(title = "PCA Plot", width = 6,
                           plotlyOutput("pca_plot"),
                           br()
                           
                           )
                )
              ),
              fluidRow(
                column(12,
                       box(
                         title = "Scree Plot", width = 12,
                         hr(),
                         plotlyOutput("scree_plot")
                       )
                )
              )
      )
    )
  )
)

