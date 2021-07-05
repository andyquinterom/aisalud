# Nota Técnica

En este modulo se puede generar una nota técnica a partir de la combinación de
diferentes escenarios. Para la generación de la Nota técnica se utilizan los
estadísticos descriptivos y el usuario puede interactuar con la aplicación para
lograr el ajuste deseado.

## Generación de escenarios

Para la generación de escenarios es requerido seleccionar el **Agrupador
principal** a utilizar (para mayor información acerca de los agrupadores se
puede revisar el manual). también se debe seleccionar la unidad de conteo a
utilizar entre prestaciones, pacientes y facturas, como también se encuentra la
opción de hacer una agrupación por episodios (para ampliación de
esta información revisar el manual de **agrupadores**). Por último se debe hacer
clic en **Generar escenarios**.

> Para cambiar la población no es necesario hacer clic en **Generar
> escenarios**, los datos se ajustarán automáticamente.

La Nota técnica generada por defecto corresponde a los escenarios considerando
el costo medio de cada agrupación cómo la mediana del conjunto de costos medios
por mes de la agrupación y la frecuencia media por mes. Este valor se puede
ajustar utilizando el gráfico y deslizador que se encuentra en la esquina
inferior derecha.

## Ajuste a los valores y frecuencias

Para ajustar los costos medios y frecuencias de los diferentes agrupadores
se deben utilizar las casillas y parámetros presentes en la sección
inferior derecha. En **Agrupador** se escoge la categoría dentro del agrupador
que se desea ajustar. A esta se le puede ajustar el **costos medio** y la
**Frecuencia**.

### Costos medios

El costo medio se puede ajustar haciendo usos de los valores de los percentiles,
estos se encuentran en la barra deslizable donde se puede escoger desde el
percentil 0 (el valor mínimo) hasta el percentil 100 (valor máximo).

El gráfico de esta sección muestra en los costos medios a través del periodo de
tiempo de los datos con el fin de poder ubicar tendencias. Las lineas
horizontales permiten ubicar la media, la mediana, el percentil 75 y el
percentil seleccionado por el usuario para esa categoría. Al mover el
deslizador, la linea **Ajuste analista** debe moverse al valor seleccionado.

### Frecuencias

En la pestaña frecuencias con la misma barra deslizante se puede escoger desde
el valor mínimo de frecuencias mensuales hasta el valor máximo de estas. El
gráfico muestra las frecuencias de cada mes con el fin de facilitar la
identificación de tendencias.

## Seguimiento en tiempo real

> El valor a ejecutar se refiere a la proyección del valor de la nota técnica
> multiplicando el valor a mes por el número de meses a comparar.

En la esquina inferior de la izquierda se encuentra un gráfico de linea de serie
de tiempo, la fecha de inicio y la fecha final se pueden modificar con la barra
deslizable. también se encuentra un checkbox sencillo que la ser escogido cambia
el método de seguimiento a uno de frecuencias. En el primero se encuentra la
comparación entre el valor a ejecutar y el valor efectivamente ejecutado, en el
segundo se muestra esta misma comparación pero multiplicando el número de
frecuencias por el costo medio de la Nota técnica.
