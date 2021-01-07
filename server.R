shinyServer(function(input, output, session) {
  
  opciones <- reactiveValues(
    "valor_costo" = "valor",
    "tabla_nombre" = "Ninguno"
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
  
  # # Modulo Frecuencias --------------------------------------------
  # 
  # callModule(
  #   module = frecuencias_server, 
  #   id = "frecuencias_modulo",
  #   opciones = opciones,
  #   nombre_id = "frecuencias_modulo",
  #   datos = datos_modulos
  # )
  # 
  # 
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

  # Modulos seguimiento NT ---------------------------------------------

  if (NTS_INCLUIDO) {

    callModule(
      module = seguimiento_notas_indice_server,
      id = "seguimiento_notas_indice",
      indice = dash_nt_indice,
      nombre_id = "seguimiento_notas_indice"
    )
  
    callModule(
      module = seguimiento_notas_dashboard_server,
      id = "seguimiento_notas_dash",
      indice = dash_nt_indice,
      nota_tecnica = dash_nt_datos,
      inclusiones = dash_nt_inclusiones
    )
  
    seguimiento_notas_comparar_server(
      id = "seguimiento_notas_comparar",
      opciones = opciones,
      indice = dash_nt_indice,
      nota_tecnica = dash_nt_datos
    )

  }

 
})