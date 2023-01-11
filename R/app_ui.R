#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    navbarPage("ShinyChanelApp",

               #Onglet Exploration des données
               tabPanel("Exploration des données",

                        fluidRow(
                          navlistPanel(widths = c(2,10),
                            tabPanel("Affichage des données brutes", tableOutput("tbl_dta")),
                            tabPanel("Graph", plotlyOutput("graph_dta", height = 800))
                          )
                        )
                        ),

               #Onglet Transformation des données
               tabPanel("Transformation des données",
                        fluidRow()),

               #Onglet Modèle
               tabPanel("Modèle",
                        sidebarLayout(
                          sidebarPanel(
                            uiOutput("y_selection"),
                            actionButton("submitButton", "Lancer")
                          ),
                          mainPanel(
                            tableOutput("tbl_ANOVA")
                          )
                        )
                        )
               )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ShinyChanelApp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
