# Nota Tecnica

En este modulo se puede generar una nota tecnica a partir de la combinación de
diferentes escenarios, y descargarala al ordenador en un archivo excel. Para la
generación de la Nota Tecnica se utilizan los estadisticos descriptivos y el
usuario puede interactuar con la aplicación para lograr el ajuste deseado.

## Generación de escenarios

Para la generación de escenarios es requerido seleccionar el **Agrupador
principal** a utilizar (para mayor información acerca de los agrupadores se
puede revisar el manual). Tambien se debe seleccionar la unidad de conteo a
utilizar entre prestaciones, pacientes y facturas, como tambien se encuentra la
opción de hacer una agrupación por episodios (para ampliación de
esta información revisar el manual de **agrupadores**). Por último se debe
ingresar la población de manera manual y hacer clic en **Generar escenarios**.
La Nota Tecnica generada por defecto corresponde al escenario considerando el
valor de la mediana para cada una de las categorías de agrupación, este valor 
se puede ajustar utilizando el gráfico que se encuentra en la esquina inferior
de la derecha.

## Ajuste usuario

En **Agrupador** se escoje la variable categorica que se va a ajustar. Se
pueden ajustar los **costos medios** y la **Frecuencia**. Los Costos Medios se
pueden ajustar haciendo usos de los valores de los percentiles, estos se
encuentran en una barra deslizable en donde se puede escoger desde el percentil
0 o valor mínimo hasta el percentil 100 o valor máximo. Las lineas horizontales
ayudan a ubicar los valores de la media, la mediana y el percentil 75. En la
pestaña frecuencias con la misma barra deslizante se puede escoger desde el 
valor mínimo de frecuencias mensuales hasta el valor máximo de estas.

En la esquina inferior de la izquierda se encuentra in gráfico de linea de 
serie de tiempo, la fecha de inicio y la fecha final se pueden modificar con la
barra deslizable. Tambien se encuentra un checkbox sencillo que la ser escogido
cambia al gráfico de frecuencias.
