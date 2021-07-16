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
# El backend de Analítica Integrada Salud seguira una estructura modular.
# El flow de datos empieza en el módulo carga_datos y alimentan a los otros.
# La información compartida se manifiesta en la variable opciones.
# ┌────────────┐   ┌─────┐
# │            │◄──┤ pSQL│
# │            │   ├─────┤
# │cargar_datos│◄──┤ UI  │
# │            │   ├─────┤
# │            │◄──┤ WEB │
# └─────┬──────┘   └─────┘
#       │
#       │    ┌───────────┐
#       │◄──►│  Filtros  │
#       │    └───────────┘
#       ▼
# ┌──────────────────────┐
# │        Módulos       │
# └──────────────────────┘
#
# Los módulos deben ser self containing. No deben afectar el comportamiento
# de otros módulos. Solamente el módulo de filtros podrá cambiar datos.

shinyServer(function(input, output, session) {

  opciones <- reactiveValues(
    "valor_costo" = "valor",
    "tabla_nombre" = "Ninguno",
    "datos_cargados" = FALSE,
    "perfil_enable" = FALSE,
    "fecha_rango" = rep(Sys.Date(), 2),
    "cantidad" = FALSE
  )

  cache <- reactiveValues()

  # Modulo prepara ------------------------------------------------------------

  cargar_datos_server(
    id = "cargar_datos",
    opciones = opciones,
    cache = cache,
    conn = conn
  )

  # Modulo de opciones adicionales --------------------------------------------

  opciones_adicionales_server(
    id = "opciones_adicionales",
    opciones = opciones
  )

  # Modulo filtros ------------------------------------------------------------

  filtros_server(
    id = "filtros_sideBar",
    opciones = opciones,
    cache = cache
  )

  # Modulo descriptiva y episodios --------------------------------------------

  descriptiva_server(
    id = "episodios_modulo",
    opciones = opciones,
    cache = cache,
    conn = conn
  )

  # Modulo outliers -----------------------------------------------------------

  outliers_server(
    id = "outliers_modulo",
    opciones = opciones,
    cache = cache
  )

  # Modulo generar nota técnica -----------------------------------------------

  nota_tecnica_server(
    id = "nota_tecnica_modulo",
    opciones = opciones,
    cache = cache
  )

  # # Modulo otros gráficos ---------------------------------------------------

  otros_graficos_server(
    id = "otros_graficos_modulo",
    opciones = opciones
  )

  # Modulo de composicion

  composicion_server(
    id = "composicion_modulo",
    opciones = opciones,
    cache = cache,
    conn = conn
  )

  # Modulo de dashboard de notas técnicas

  nt_dashboard_server(
    id = "nt_dashboard",
    opciones = opciones
  )

  # Modulo de seguimiento

  seguimiento_server(
    id = "seguimiento",
    opciones = opciones,
    cache = cache
  )

  # Modulo de manuales

  manuales_server("manuales")

  manuales_server("manuales_sideBar")

})
