#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny readxl ggplot2 FactoMineR factoextra dplyr reshape2 plotly
#' @importFrom dplyr filter
#' @noRd
app_server <- function(input, output, session) {

  #Boolean
  shinyjs::hide(id="warning_outlier")
  hasNA <- FALSE

  #DATA
  dta <-  read_xlsx("inst/app/www/Data Test Technique V2.xlsx")
  dta$Product <- as.character(dta$Product)
  dta$Judge <- as.character(dta$Judge)
  dta <- as.data.frame(dta)

  listY <- colnames(dta %>% select(where(is.numeric)))


  p_dta <- reactive({
    dta
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
    if((p_dta()[which(as.numeric(p_dta()$`Sensory Variable 1`)>=10 | as.numeric(p_dta()$`Sensory Variable 1`)<=0),] %>% nrow()) > 0){
      shinyjs::show(id="warning_outlier")
    }
  })

  #MODEL ANOVA
  shinyjs::hide(id="Modele_graph")

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

      resAov <- FactoMineR::AovSum(
        formula = formula(paste0("`", input$y_ANOVA , "`", " ~ Product + Judge")),
        data = p_dta())$Ftest %>% as.data.frame
      resAov$`Pr(>F)` <- as.character(signif(resAov$`Pr(>F)`, digits = 3))

      reslm <- lm(
        formula(paste0("`", input$y_ANOVA , "`", " ~ Product + Judge")),
        data = p_dta()
      )

      output$graph_ANOVA_1 <- renderPlot(
        acf(residuals(reslm),
            main="Indépendance des résidus")
      )
      output$graph_ANOVA_2 <- renderPlot(
        plot(reslm, 2,
             main = "QQ Plot - Distribution normale des résidus",
             sub = paste0("`", input$y_ANOVA , "`", " ~ Product + Judge"))
      )

      output$graph_ANOVA_3 <- renderPlot(
        plot(reslm, 3,
             main = "Homoscédasticité des résidus",
             sub = paste0("`", input$y_ANOVA , "`", " ~ Product + Judge"))
      )

      output$tbl_ANOVA <- renderTable(
        resAov
      )

    })

}
