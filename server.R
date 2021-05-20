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

  # Modulo prepara ------------------------------------------------------------

  cargar_datos_server(
    id = "cargar_datos",
    opciones = opciones,
    conn = conn
  )

  # Modulo filtros ------------------------------------------------------------

  filtros_server(
    id = "filtros_sideBar",
    opciones = opciones
  )

  # Modulo descriptiva y episodios --------------------------------------------

  descriptiva_server(
    id = "episodios_modulo",
    opciones = opciones,
    conn = conn
  )

  # Modulo outliers -----------------------------------------------------------

  outliers_server(
    id = "outliers_modulo",
    opciones = opciones
  )

  # Modulo generar nota técnica -----------------------------------------------

  nota_tecnica_server(
    id = "nota_tecnica_modulo",
    opciones = opciones
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
    conn = conn
  )

  seguimiento_notas_dashboard_server(
    id = "seguimiento_notas_dash",
    opciones = opciones)

})
