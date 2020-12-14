shinyServer(function(input, output, session) {
  
  opciones <- reactiveValues(
    "valor_costo" = "valor"
  )
  
 # Modulo prepara ---------------------------------------------
  
  datos_modulos <- callModule(
    module = prepara_server,
    id = "prepara_modulo",
    nombre_id = "prepara_modulo",
    opciones = opciones
  )
  
  callModule(
    module = base_de_datos_server,
    id = "prepara_base_de_datos",
    nombre_id = "prepara_base_de_datos",
    datos = datos_modulos,
    opciones = opciones
  )
  
  # Modulo filtros --------------------------------------------
  
  callModule(
    module = filtros_server,
    id = "filtros_sideBar",
    datos = datos_modulos
  )
  
  # Modulo descriptiva y episodios --------------------------------------------
  
  callModule(
    module = episodios_server, 
    id = "episodios_modulo",
    opciones = opciones,
    nombre_id = "episodios_modulo",
    datos = datos_modulos
  )
  
  # Modulo Frecuencias --------------------------------------------
  
  callModule(
    module = frecuencias_server, 
    id = "frecuencias_modulo",
    opciones = opciones,
    nombre_id = "frecuencias_modulo",
    datos = datos_modulos
  )
  
 
  # Modulo outliers -----------------------------------------------------------
  
  callModule(
    module = outliers_server,
    id = "outliers_modulo",
    datos = datos_modulos,
    opciones = opciones,
    nombre_id = "outliers_modulo"
  )
  
 
  # Modulo generar nota técnica -----------------------------------------------
  
  callModule(
    module = nota_tecnica_server,
    id = "nota_tecnica_modulo",
    nombre_id = "nota_tecnica_modulo",
    datos = datos_modulos,
    opciones = opciones
  )
  

  # Modulo paquete -------------------------------------------------
  
  if (PAQUETES_INCLUIDO) {
    
    callModule(
      module = paquetes_dashboard_server,
      id = "paquetes_modulo_dashboard"
    )
    
    callModule(
      module = paquetes_indice_server,
      id = "paquetes_modulo_indice",
      paquete_path = paquete_path,
      nombre_id = "paquetes_modulo_indice"
    )
    
  }
  
  # Modulo otros gráficos
  
  callModule(
    module = otros_graficos_server,
    id = "otros_graficos_modulo",
    datos = datos_modulos
  )
  

  # Pricing ------------------------------------------------------------------
  
  if (PRICING_INCLUIDO) {
    callModule(
      module = pricing_server,
      id = "pricing_modulo", 
      pricing_path = pricing_path,
      nombre_id = "pricing_modulo"
    )
  }
 
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
    
    callModule(
      module = seguimiento_notas_comparar_server,
      id = "seguimiento_notas_comparar",
      datos = datos_modulos,
      indice = dash_nt_indice,
      nota_tecnica = dash_nt_datos,
      nombre_id = "seguimiento_notas_comparar",
      opciones = opciones
    )
    
  }
  
 
})