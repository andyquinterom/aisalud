shinyServer(function(input, output, session) {
  
  opciones <- reactiveValues(
    "valor_costo" = "valor",
    "tabla_nombre" = "Ninguno",
    "datos_cargados" = FALSE,
    "perfil_enable" = FALSE
  )
  
  
 # Modulo prepara ---------------------------------------------
  
  base_de_datos_server(
    id = "prepara_base_de_datos",
    opciones = opciones,
    conn = conn
  )
  
  # Modulo filtros --------------------------------------------
  
  filtros_server(
    id = "filtros_sideBar",
    opciones = opciones
  )
  
  # Modulo descriptiva y episodios --------------------------------------------
  
  episodios_server(
    id = "episodios_modulo",
    opciones = opciones,
    conn = conn
  )
  
  # # Modulo outliers -----------------------------------------------------------
  # 
  outliers_server(
    id = "outliers_modulo",
    opciones = opciones
  )
  # 
  # 
  # # Modulo generar nota técnica -----------------------------------------------
  # 
  
  nota_tecnica_server(
    id = "nota_tecnica_modulo",
    opciones = opciones
  )
  
  # # Modulo otros gráficos -----------------------------------------------------

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
  
  configuracion_server(
    id = "configuracion",
    opciones = opciones
  )

})