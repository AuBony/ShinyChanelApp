#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny readxl ggplot2 FactoMineR
#' @importFrom dplyr filter
#' @noRd
app_server <- function(input, output, session) {

  #DATA
  dta <-  read_xlsx("inst/app/www/Data Test Technique V2.xlsx")

  p_dta <- reactive({
    dta
    })

  observe(
    output$tbl_dta <- renderTable({p_dta()})
            )

  #MODEL
  observe(
    #as.formula(paste0( ))
    # resAov <- FactoMineR::AovSum(formula = `Sensory Variable 1` ~ Product + Judge + Product:Judge, data = p_dta())$Ftest,
    # tbl_Factor <- rownames(resAov$Ftest),
    # resAov <- resAov[,c(length(resAov[1,]),1:(length(resAov[1,])-1))],
    # colnames(resAov)[1] <- str_to_upper(variables[i]),

    output$main_res <- renderTable(
      FactoMineR::AovSum(formula = `Sensory Variable 1` ~ Product + Judge + Product:Judge, data = p_dta())$Ftest
      )
  )
}
