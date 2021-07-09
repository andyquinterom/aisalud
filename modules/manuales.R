#
# Analítica Integrada Salud
#
# Derechos de autor 2021 por MD&CO Consulting Group (NIT 901.119.781-5)
# Copyright (C) 2021 by MD&CO Consulting Group
#
# Este programa es software libre: puede redistribuirlo o modificarlo bajo
# los términos de la licencia Affero General Public License tal cual
# publicada por la Free Software Foundation, sea la versión 3 de la licencia
# o cualquier versión posterior. Este programa se distribuye SIN GARANTÍA
# EXPERSA O IMPLÍCITA, INCLUIDAS LAS DE NO INFRACCIÓN, COMERCIABILIDAD O
# APTITUD PARA UN PROPÓSITO PARTICULAR. Referir a la
# AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) para más detalles.
#

manuales_ui <- function(id) {
  ns <- NS(id)

  archivos <- c(
    "Cargar datos" = "cargar_datos.md",
    "Filtros y opciones adicionales" = "filtros_opciones.md",
    "Agrupadores"  = "agrupadores.md",
    "Selección de datos" = "seleccion_datos.md",
    "Descriptiva" = "descriptiva.md",
    "Outliers" = "outliers.md",
    "Nota técnica" = "nota_tecnica.md",
    "Otros gráficos" = "otros_graficos.md",
    "Composición" = "composicion.md",
    "Seguimiento contratos" = "seguimiento_contratos.md"
  )

  tagList(
    fluidRow(
      box(
        width = 12,
        tags$div(
          style = "min-height: 600px",
          selectizeInput(
            inputId = ns("manual"),
            label = "Manual",
            width = "100%",
            choices = archivos
          ),
          uiOutput(ns("markdown"))
        )
      )
    )
  )

}

manuales_server <- function(id) {

  moduleServer(
    id = id,
    module = function(input, output, session) {

      output$markdown <- renderUI({
        if (input$manual != "") {
          tags$article(
            class = "markdown-body",
            includeMarkdown(file.path("man", input$manual))
          )
        }
      })

    }
  )

}
