# Cargar Datos

La sección de cargar datos se divide en dos zonas:

1. La selección de conjuntos de datos y subida de archivos.
2. Modificación de perfiles y notas técnicas.

## Selección del conjunto  de datos

Para seleccionar el conjunto de datos a trabajar, primero se debe seleccionar
el rango de fechas a trabajar. Aunque no es obligatorio, se recomienda
pre-seleccionar el rango de fechas para altos volumenes de datos, dado que
cambios a esta selección pueden causar aumentos en tiempos de carga inicial.

Después de haber seleccionado un rango de fechas se debe seleccionar el
conjunto de datos a trabajar. En el menú **Seleccionar datos** se encontraran
todos los conjuntos de datos cargados.

Si los datos fueron cargados, un gráfico de barras debe aparecer en el lado
derecho de la pantalla.

## Subir datos

> **Advertencia**: Esta funcionalidad esta hecha para manejar paqueños volumenes de datos, entre 0 - 100.000 registros. Los limites de tamaño pueden ser cambiados por un administrador. Se recomienda siempre trabajar con conjuntos de datos cargados en la nube.

Para subir un conjunto de datos desde un archivo de texto delimitado, se debe
hacer click en la sección **Subir datos** en el panel izquierdo.

Primero, se debe subir el archivo delimitado codificado en **UTF-8**. La
aplicación mostrará el mensaje **Upload completed** una vez el archivo se haya
acabado de subir.

Después se debe seleccionar el rango de fechas para el conjunto de datos
subido.

Si el archivo es un archivo de tipo .feather, se debe seleccionar esta opción
en **Tipo de archivo**, de lo contrario se mantiene la opción csv.

El botón opciones, abrirá un dialogo con la selección del delimitador y el
separador decimal. La opción espacios se refiere al carácter tabulador **\t**.
Una vez seleccionadas estas opciones se hace click en el botón guardar. El
dialogo debe cerrarse.

Existen muchas maneras de codificar una fecha, por esta razón, se debe hacer
especificar el formato de fechas a utilizar.
