# library(shiny)
# library(mapGCT)
# library(edgeR)
# library(ggplot2)
# library(DT)
# library(plotly)
# library(reactable)
options(shiny.maxRequestSize=30*1024^2)

quantile_normalize <- function(counts, log = FALSE) {
  if (log) {
    counts <- log2(counts + 1) 
  }
  counts <- as.matrix(counts)
  sorted_counts <- apply(counts, 2, sort)
  mean_ranks <- rowMeans(sorted_counts)
  ranked_data <- apply(counts, 2, rank, ties.method = "average")
  normalized_matrix <- matrix(NA, nrow = nrow(counts), ncol = ncol(counts))
  for (i in 1:ncol(counts)) {
    for (j in 1:nrow(counts)) {
      rank_value <- ranked_data[j, i] 
      normalized_matrix[j, i] <- mean_ranks[rank_value]
    }
  }
  return(normalized_matrix)
}

pca_calc <- function(input_matrix, metadata, top_genes = 500) {
  mad_values <- apply(input_matrix, 1, mad)
  top_genes <- order(mad_values, decreasing = TRUE)[1:top_genes]
  input_matrix <- input_matrix[top_genes, , drop = FALSE]
  transpose_input_matrix <- t(input_matrix)
  transpose_input_matrix <- transpose_input_matrix[, apply(transpose_input_matrix, 2, var) != 0, drop = FALSE]
  pca <- prcomp(transpose_input_matrix, scale = TRUE)
  score <- data.frame(pca$x, metadata)
  return(list("scores" = score, "summary" = summary(pca)))
}

normalizeCPM <- function(counts, log = FALSE) {
  cpm(counts, log = log)
}

pca_plot <- function(pca, column, pc1 = "PC1", pc2 = "PC2") {
  fig <- plotly::plot_ly(
    data = pca$scores, x = as.formula(paste("~", pc1)), y = as.formula(paste("~", pc2)), type = "scatter", text = as.character(row.names(pca$scores)),
    mode = "markers", color = as.formula(paste("~", column))
  ) %>% plotly::layout(
    xaxis = list(title = paste(pc1, " ", pca$summary$importance[2, pc1], " ")),
    yaxis = list(title = paste(pc2, " ", pca$summary$importance[2, pc2], " ")),
    title = "PCA Plot",
    legend = list(title = list(text = paste("<b>", column, "</b>")))
  )
  fig
}

server <- function(input, output, session) {
  rv <- reactiveValues(gct_data = NULL, normalized_data = NULL, pca_result = NULL, normalized_counts=NULL, pca_plot_dw = NULL)
  
  shinyjs::disable("download_gene_data")
  shinyjs::disable("download_normalized_data")
  shinyjs::disable("download_pca_plot")
  observeEvent(input$gct_file, {
    showNotification("File being uploaded...",duration = 3,
                     action = a(href = "javascript:location.reload();", "Reload page")
    )
    req(input$gct_file)
   
    tryCatch({
      gct_obj <- mapGCT::parse_gct(input$gct_file$datapath)
      
      rv$gct_data <- gct_obj@mat
      rv$cdesc <- gct_obj@cdesc
    }, error = function(e) {
      showNotification(paste("Error: ", e$message), type = "error")
      rv$gct_data <- NULL
      rv$cdesc <- NULL
      return()
    })
    
    
    colnames_gct <- colnames(rv$gct_data)
    updateSelectInput(session, "length_column", choices = colnames_gct)
    
    output$genes <- renderValueBox({valueBox(value = nrow(rv$gct_data),subtitle = "Total Genes", color = "aqua", icon = icon("dna"))})
    output$sample <- renderValueBox({valueBox(value = ncol(rv$gct_data),subtitle = "Total Samples", color = 'teal', icon = icon("vials"))})
    output$tissue <- renderValueBox({valueBox(value = length(rv$cdesc$kw_curated_tissue[rv$cdesc$kw_curated_tissue != "none"]),subtitle = "Tissues",color = 'olive', icon = icon("eye"))})
    
    # Conditionally enable the download button when data is available
    observe({
      if (is.null(rv$gct_data)) {
        shinyjs::disable("download_gene_data")
      } else {
        shinyjs::enable("download_gene_data")
      }
    })
  })
  
  output$download_gene_data <- downloadHandler(
    filename = function() {
      paste("gene_data", ".csv", sep = "")
    },
    content = function(file) {
      data <- rv$gct_data
      req(data)  # Ensure that data exists before attempting download
      write.csv(data, file)
    }
  )
  
  
  observeEvent(input$apply_normalization, {
    req(rv$gct_data)
    showModal(modalDialog("Normalization PCA...", footer = NULL))
    
    counts <- rv$gct_data
    rownames(counts) <- rownames(rv$gct_data)
    
    normalized_counts <- NULL
    log_transform <- input$log_transform 
    
    if (input$normalization_method == "CPM") {
      normalized_counts <- normalizeCPM(counts, log = log_transform)
    } else if (input$normalization_method == "Quantile") {
      normalized_counts <- quantile_normalize(counts, log = log_transform)
      rownames(normalized_counts) <- rownames(rv$gct_data)
      colnames(normalized_counts) <- colnames(rv$gct_data)
    }
    rv$normalized_counts <- normalized_counts
    removeModal()
    
    observe({
      if (is.null(rv$normalized_counts)) {
        shinyjs::disable("download_normalized_data")
      } else {
        shinyjs::enable("download_normalized_data")
      }
    })
  })
  
  observeEvent(rv$normalized_counts, {
    showModal(modalDialog("Computing PCA...", footer = NULL))
    
    updateNumericInput(session, "top_counts", value = min(500, nrow(rv$normalized_counts)))
    tryCatch({
      rv$pca <- pca_calc(rv$normalized_counts, rv$cdesc, nrow(rv$normalized_counts))
      
      pca_axis_values <- setdiff(names(rv$pca$scores), names(rv$cdesc))
      color_by_values <- setdiff(intersect(names(rv$pca$scores), names(rv$cdesc)), c("Sample", "id"))
      
      updateSelectInput(session, "pc_x", choices = c(pca_axis_values))
      updateSelectInput(session, "pc_y", choices = c(pca_axis_values), selected = c(pca_axis_values)[2])
      updateSelectInput(session, "pc_col", choices = c(color_by_values))
      
      removeModal()
      shinyalert("Done", "Normalization Completed", type = "success")
    }, error = function(e) {
      # Handle the error
      removeModal()  # Ensure modal is removed in case of failure
      shinyalert("Failed", paste("Error: ", e$message), type = "error")
    })
  })
  
  
  observeEvent(input$pca_plot_btn, {
    if (is.null(input$top_counts)) {
      shinyalert("Invalid Input", "Please provide a valid top count value.", type = "error")
      return(NULL)
    }
    
    if (input$pc_x == "" || input$pc_y == "" || input$pc_col == ""){
      shinyalert("Missing Input", "Please select all PCA options (PC1, PC2, and Color by).", type = "warning")
      return(NULL)
    }
    
    req(input$top_counts, input$pc_x, input$pc_y, input$pc_col)
    
    # Ensure the top N feature value is not less than 500
    if (input$top_counts < 500) {
      showNotification(tags$h4("Error"), "Top N feature value can't be less than 500", type = "error", duration = 10)
      return()
    }
    
    showModal(modalDialog("Plotting PCA...", footer = NULL))
    
    
    rv$pca_scores_plot <- pca_plot(
      pca = rv$pca,
      column = input$pc_col,
      pc1 = input$pc_x,
      pc2 = input$pc_y
    )
    
    output$pca_plot <- plotly::renderPlotly({
      rv$pca_scores_plot
    })
    
    observe({
      if (is.null(rv$pca_scores_plot)) {
        shinyjs::disable("download_pca_plot")
      } else {
        shinyjs::enable("download_pca_plot")
      }
    })
    
    removeModal()
  })
  
  output$scree_plot <- renderPlotly({
    req(rv$pca) 
    
    variance_explained <- (rv$pca$summary$importance[2, ]) * 100 
    
    pca_variance_df <- data.frame(
      PC = paste0("PC", 1:length(variance_explained)),
      VarianceExplained = variance_explained
    )
    
    fig <- plot_ly(
      data = pca_variance_df, 
      x = ~PC, 
      y = ~VarianceExplained, 
      type = "scatter", 
      mode = "lines+markers",  # Line plot with markers for each point
      line = list(color = "steelblue"),
      marker = list(size = 8, color = "steelblue")
    ) %>% 
      layout(
        title = "Scree Plot: Variance Explained by Each Principal Component",
        xaxis = list(title = "Principal Component"),
        yaxis = list(title = "Variance Explained (%)"),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
    
    fig
  })
  
  output$download_pca_plot <- downloadHandler(
    filename = function() { "PCA_plot.png" },
    content = function(file) {
      plotly::export(p = rv$pca_scores_plot, file = file, width = 800, height = 600)
    }
  )
  
  output$download_normalized_data <- downloadHandler(
    filename = function() { "normalized_data.csv" },
    content = function(file) {
      write.csv(rv$normalized_counts, file)
    }
  )
  
  output$data_table <- renderReactable({
    req(rv$gct_data)
    reactable(as.data.frame(rv$gct_data), sortable = FALSE,filterable = TRUE,searchable = TRUE, minRows = 10)
  })
  
  output$normalized_data <- renderReactable({
    req(rv$normalized_counts)
    reactable(as.data.frame(rv$normalized_counts))
  })
  
}

