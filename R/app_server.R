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
  dta$Product <- factor(dta$Product)
  dta$Judge <- factor(dta$Judge)

  p_dta <- reactive({
    dta
    })

  observe(
    output$tbl_dta <- renderTable({p_dta()})
            )

  #MODEL ANOVA
  observe({
    #as.formula(paste0( ))
    resAov <- FactoMineR::AovSum(formula = `Sensory Variable 1` ~ Product + Judge, data = p_dta())$Ftest
    resAov$` ` <- rownames(resAov)
    resAov$`Pr(>F)` <- as.character(signif(resAov$`Pr(>F)`, digits = 3))
    output$main_res <- renderTable(
      resAov %>% select(` `, everything())
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
