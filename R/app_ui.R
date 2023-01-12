#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny shinyjs
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    navbarPage("ShinyChanelApp",

               useShinyjs(),

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

                        #Bandeau Erreur
                        fluidRow(
                          shinydashboard::box(
                            id="warning_outlier",
                            width=11,
                            img(src="www/avertissement.png", width = 25, height = 25),
                            HTML(text = "&nbsp;"),
                            HTML(text ="<strong>Des données aberrantes ont été détectées !</strong> Des scores supérieurs à 10 ou inférieur 0 sont présents dans le jeu de données")
                          ),
                          shinydashboard::box(
                            id="warning_na",
                            width=11,
                            img(src="www/avertissement.png", width = 25, height = 25),
                            HTML(text = "&nbsp;"),
                            HTML(text = "<strong>Des données manquantes ont été détectées !</strong> Elles ne seront pas prise en compte dans la modélisation")
                          )
                        ),

                        navlistPanel(widths = c(2,10),
                                     tabPanel("Données Aberrantes", tableOutput("tbl_TRANSFO_NA")),
                                     tabPanel("Données manquantes", plotOutput("graph_TRANSFO_NA_var"))
                        )

               ),

               #Onglet Modèle
               tabPanel("Modèle",
                        sidebarLayout(
                          sidebarPanel(width = 2,
                                       uiOutput("y_selection"),
                                       actionButton("submitButton", "Lancer")
                          ),
                          mainPanel(
                            fluidRow(align="center",
                                     tableOutput("tbl_ANOVA")
                                     ),

                            shinydashboard::box(id="Modele_graph",
                                                width = 12,
                                                wellPanel(
                                                  h1("Test des hypothèses"),
                                                  fluidRow(
                                                    column(width = 6,
                                                           plotOutput("graph_ANOVA_1"),
                                                           br(),
                                                           plotOutput("graph_ANOVA_2")
                                                    ),
                                                    column(width = 6,
                                                           plotOutput("graph_ANOVA_3")
                                                    )
                                                  )
                                                )

                            )
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
