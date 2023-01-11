#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny readxl ggplot2 FactoMineR factoextra dplyr reshape2
#' @importFrom dplyr filter
#' @noRd
app_server <- function(input, output, session) {

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
    output$graph_dta <- renderPlot(
      ggplot(meltdata, aes(factor(variable), value, fill =variable)) +
        geom_violin() +
        geom_boxplot(width=0.1, fill="white", outlier.shape=NA) +
        geom_jitter(shape=16, position=position_jitter(0.1)) +
        facet_wrap(~variable, scale="free") +
        theme_classic() +
        theme(legend.position = "none")
    )
  })



  #MODEL ANOVA
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
      resAov <- FactoMineR::AovSum(
        formula = formula(paste0("`", input$y_ANOVA , "`", " ~ Product + Judge")),
        data = p_dta())$Ftest %>% as.data.frame

      output$tbl_ANOVA <- renderTable(
        resAov
      )

    })





  #ACP
  # acp <- PCA(data, scale.unit = T)
  # fviz_pca_ind(acp,
  #              geom.ind = c("point","text"), #c("#FA0000", "#FFFFFF", "#FFFFFF") show points only (nbut not "text")
  #              col.ind = as.factor(couleur)
  # )
  #
  # fviz_pca_var(acp, col.var = "cos2",
  #              gradient.cols = c("#FA0000", "#E7B800", "#5FCC00"),
  #              repel = TRUE # Avoid text overlapping
  # ) #alpha.var="cos2"
}
