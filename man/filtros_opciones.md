# Filtros y opciones adicionales

Para acceder al menú de filtros y opciones adicionales se debe hacer clic
en el botón en la esquina superior derecha con un icono de piñones. Al hacer
clic, se debe desplegar un menú desde la derecha con dos pestañas: filtros y
opciones adicionales.

> Los filtros y opciones adicionales afectan el comportamiento de la mayoría de
> funcionalidades de AIS. Es importante estar consiente del estado de estas
> opciones.

## Filtros

Los filtros se componen de tres columnas:

1. Variables: la selección de las variables a filtrar en el conjunto de datos.
2. Estado: Si el estado del filtro es de inclusión o exclusión.
3. Valores: Los valores seleccionados de la variable.

Para aplicar filtros se debe hacer clic en el botón **Aplicar** una vez se
tengan seleccionados todos los parámetros deseados.

### Variables cualitativas

Para hacer un filtro se debe identificar si es una variable cuantitativa o
cualitativa. Si es cualitativa, esta debe seleccionarse en las filas con
opción de inclusión o exclusión. Una vez seleccionada, se podrán buscar y
seleccionar los valores a excluir en la casilla de la misma fila en la columna
valores. Si el estado del filtro está en **Incluir** solo se tomarán en cuenta
registros que incluyan los valores seleccionados. Si el estado del filtro está
en **Excluir** solo se tomarán en cuenta registros que no incluyan los valores
seleccionados.

### Variables cuantitativas

Para filtrar variables cuantitativas, estas deben seleccionarse en las filas
que incluyan las sub-columnas **Mínimo** y **Máximo**. Al seleccionar una
variable, se cargarán los valores mínimos y máximos de esta variable. Solo se
tomarán en cuenta registros con valores dentro de este rango.

### Pacientes

La fila con la preselección **Pacientes** se comporta de igual manera a los
filtros de variables cualitativas, pero solo se puede alimentar de datos al
excluir pacientes con la funcionalidad de outliers.

## Opciones adicionales

La pestaña de opciones adicionales incluye las siguientes opciones:

1. **Columna de valor**: permite cambiar la columna con la cual se hacen los
cálculos estadísticos y de seguimiento a través de AIS.
2. **Perfil**: se selecciona el perfil que se utilizará para los diferentes
parámetros de la aplicación.
3. **Prestaciones por cantidad**: En caso de que las frecuencias deban
calcularse sumando la variable cantidad y no contando registros, esta opción
cambiará el método de cálculo.

