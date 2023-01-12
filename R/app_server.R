#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny readxl ggplot2 FactoMineR factoextra dplyr reshape2 plotly kableExtra knitr
#' @importFrom dplyr filter
#' @noRd
app_server <- function(input, output, session) {

  #Boolean
  shinyjs::hide(id="warning_outlier")
  shinyjs::show(id="message_outlier")
  shinyjs::hide(id="warning_na")
  shinyjs::hide(id="body_outlier")

  #DATA
  dta <-  read_xlsx("inst/app/www/Data Test Technique V2.xlsx")
  dta <- as.data.frame(dta)
  listY <- colnames(dta %>% select(-Product, - Judge))


  vals <- reactiveValues()
  vals$df <- dta
  p_dta <- reactive({
    vals$df
  })

  observe({

    #Data Table
    output$tbl_dta <- renderTable({p_dta()})

    #Data Graph
    meltdata=melt(p_dta(), id=c("Product", "Judge"))
    meltdata$value <- as.numeric(meltdata$value)
    output$graph_dta <- renderPlotly(
      ggplotly(ggplot(meltdata, aes(factor(variable), value, fill =variable)) +
        geom_boxplot(width=0.1, fill="white", outlier.shape=NA) +
        geom_jitter(shape=16, position=position_jitter(0.1)) +
        geom_segment(y = 0, yend = 0, x = -5, xend = 5, colour = "red") +
          geom_segment(y = 10, yend = 10, x = -5, xend = 5, colour = "red") +
        facet_wrap(~variable, scale="free", ncol=5) +
        theme_classic() +
        theme(legend.position = "none"))
    )
  })


  #TRANSFORMATION DONNEES
  observe({
    if((p_dta() %>%
        filter_at(vars(starts_with("Sensory Variable")), any_vars((. > 10) | (. < 0))) %>%
        nrow()) > 0){
      shinyjs::show(id="warning_outlier")
      shinyjs::show(id="body_outlier")
      shinyjs::hide(id="message_outlier")
    }else{
      shinyjs::hide(id="warning_outlier")
      shinyjs::hide(id="body_outlier")
      shinyjs::show(id="message_outlier")
    }

    if((p_dta() %>%
        filter_at(vars(starts_with("Sensory Variable")), any_vars(is.na(.))) %>%
        nrow()) > 0){
      shinyjs::show(id="warning_na")
    }
  })

  na_col <- colSums(is.na(dta)) / nrow(dta) * 100
  names(na_col) <- c("Product", "Judges", 1:(ncol(dta) - 2))
  output$graph_TRANSFO_NA_var <- renderPlot(
    barplot(na_col, main = "Pourcentage de données manquantes par variable", ylim = c(0,100))
  )

  output$tbl_TRANSFO_NA <- function(){
    dta %>%
      filter_at(vars(starts_with("Sensory Variable")), any_vars((. > 10) | (. < 0))) %>%
      knitr::kable("html") %>%
      kable_material(c("striped", "hover"))
  }

  observeEvent(
    input$submit_transfo,
    {
      dta_temp_transfo <- dta
      #1: supprimer toute la ligne
      if(input$radio_transfo == 1){
        dta_temp_transfo <- read_xlsx("inst/app/www/dta_NO.xlsx") %>% as.data.frame()
      }
      #2: Remplacer par NA
      else if(input$radio_transfo == 2){
        dta_temp_transfo <- read_xlsx("inst/app/www/dta_NA.xlsx") %>% as.data.frame()
      }
      vals$df <- dta_temp_transfo
    }
  )

  #MODEL ANOVA
  shinyjs::hide(id="Modele_graph")
  isProduitmodel <- reactive({FALSE})

  output$y_selection <-
    renderUI(
      selectInput(inputId = "y_ANOVA",
                  label = "Variable à ajuster",
                  choices = listY,
                  multiple = FALSE)
    )

  observeEvent(
    input$submitButton,
    {
      shinyjs::show(id="Modele_graph")
      dta_temp <- p_dta()

      #input$radio_model == 1 : Modele Complet
      #input$radio_model == 2 : Modele Produit
      if(input$radio_model == 2){
        isProduitmodel <- TRUE
        dta_temp$Product <- factor(dta_temp$Product)
        dta_temp$Judge <- factor(dta_temp$Judge)
        formule <- formula(paste0("`", input$y_ANOVA , "`", " ~ Product"))
      }else{
        isProduitmodel <- FALSE
        dta_temp$Product <- as.character(dta_temp$Product)
        dta_temp$Judge <- as.character(dta_temp$Judge)
        formule <- formula(paste0("`", input$y_ANOVA , "`", " ~ Product + Judge"))
      }


      resAov <- FactoMineR::AovSum(
        formula = formule,
        data = dta_temp)

      resAov_f <- resAov$Ftest %>% as.data.frame
      resAov_f$`Pr(>F)` <- as.character(signif(resAov_f$`Pr(>F)`, digits = 3))

      output$tbl_ANOVA_f <- renderTable(
        resAov_f, rownames = TRUE
      )

      resAov_t <- resAov$Ttest %>% as.data.frame
      output$tbl_ANOVA_t <- renderTable(
        resAov_t, rownames = TRUE
      )
      output$tbl_ANOVA_t_ui <- renderUI({
        if(!isProduitmodel){
          return("")
        }
        tableOutput("tbl_ANOVA_t")
      })

      reslm <- lm(
        formule,
        data = p_dta()
      )

      output$graph_ANOVA_1 <- renderPlot(
        acf(residuals(reslm),
            main="Indépendance des résidus")
      )
      output$graph_ANOVA_2 <- renderPlot(
        plot(reslm, 2,
             main = "QQ Plot - Distribution normale des résidus",
             sub = formule)
      )

      output$graph_ANOVA_3 <- renderPlot(
        plot(reslm, 3,
             main = "Homoscédasticité des résidus",
             sub = formule)
      )

    })

}
