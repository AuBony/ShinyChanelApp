#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny readxl ggplot2 FactoMineR factoextra dplyr
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

  observe(
    output$tbl_dta <- renderTable({p_dta()})
            )

  #MODEL ANOVA
  output$formula_ANOVA <- renderText("")

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
      output$formula_ANOVA <- renderText(paste0("`", input$y_ANOVA , "`", " ~ Product + Judge"))

      resAov <- FactoMineR::AovSum(
        formula = formula(paste0("`", input$y_ANOVA , "`", " ~ Product + Judge")),
        data = p_dta())$Ftest %>% as.data.frame

      output$main_res <- renderTable(
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
