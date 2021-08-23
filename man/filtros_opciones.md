# Filtros y opciones adicionales

Para acceder al menú de filtros y opciones adicionales se debe hacer clic
en el botón en la esquina superior derecha con un icono de piñones. Al hacer
clic, se debe desplegar un menú desde la derecha con dos pestañas: filtros y
opciones adicionales.

> Los filtros y opciones adicionales afectan el comportamiento de la mayoría de
> funcionalidades de AIS. Es importante estar consiente del estado de estas
> opciones.

## Filtros

Los filtros se componen de tres secciones (ver más detalle abajo) y tres 
columnas (explicadas a continuación):

1. Variables: la selección de las variables a filtrar en el conjunto de datos.
2. Estado: Si el estado del filtro es de inclusión o exclusión.
3. Valores: Los valores seleccionados de la variable.

En las dos primeras secciones existen dos botones con un (+) y un (-), sirven 
para adicionar o eliminar filtros. 
NOTA: La aplicación permite adicionar hasta 20 filtros.

Para aplicar filtros se debe hacer clic en el botón **Aplicar** una vez se
tengan seleccionados todos los parámetros deseados.

### Filtros discretos

Para hacer un filtro se debe identificar la columna a filtrar. Una vez 
seleccionada, se podrán buscar y seleccionar los valores en la casilla de la 
misma fila en la columna valores. Por último, si el estado del filtro está en
**Incluir** solo se tomarán en cuenta registros que incluyan los valores 
seleccionados. Si el estado del filtro está en **Excluir** solo se tomarán 
en cuenta registros que no incluyan los valores seleccionados.

### Rangos numéricos

Con el fin de brindar rápidez en los filtros de variables cuantitativas, se 
debe digitar en las sub-columnas el valor **Mínimo** y **Máximo** a filtrar. 
Solo se tomarán en cuenta registros con valores dentro de este rango.

## Eventos y pacientes
### Outliers
Registros seleccionados en el modulo __outliers__, los registros serán 
visualizados cuando en el modulo outliers se presione el botón "excluir 
pacientes seleccionados". De igual forma que las anteriores secciones, los 
registros pueden ser incluidos o excluidos.

### Pacientes / Facturas
Filtro para conocer pacientes o facturas que __contengan__ la selección realizada.
Se diferencia de la primera sección ya que la primera sección filtra exactamente
la selección realizada. 

## Opciones adicionales

La pestaña de opciones adicionales incluye las siguientes opciones:

1. **Columna de valor**: permite cambiar la columna con la cual se hacen los
cálculos estadísticos y de seguimiento a través de AIS.
2. **Perfil**: se selecciona el perfil que se utilizará para los diferentes
parámetros de la aplicación.
3. **Prestaciones por cantidad**: En caso de que las frecuencias deban
calcularse sumando la variable cantidad y no contando registros, esta opción
cambiará el método de cálculo.
