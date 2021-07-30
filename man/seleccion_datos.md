# Selección de parámetros

El proceso de seleccionar parámetros es muy similar entre los diferentes
módulos de AIS. Este manual explica el proceso estándar de selección de
parámetros. Cualquier diferencia específica se mencionará en el manual
específico de cada módulo.

## Agrupador principal

El agrupador principal es la variable de agrupación principal que se utilizará
para generar los indicadores del módulo. En esta sección solo se puede
seleccionar una variable. Si se van a crear episodios, la jerarquía debe estar
compuesta por valores dentro de la variable seleccionada.

## Separadores

En los módulos que permiten separadores, se pueden seleccionar variables para
fragmentar el análisis. En esta sección se pueden seleccionar varias variables
sin prioridad por orden.

## Agrupar por episodios

Si esta opción esta habilitada y el agrupador principal contiene menos de 60
valores únicos, se desplegará un menú y cuatro secciones conteniendo diferentes
unidades de conteo.

## Unidad de descriptiva o jerarquía de episodios

> Para una explicación a profundidad de las unidades de conteo, revisar el
> manual acerca de agrupadores.


### Unidad básica

Si la opción de agrupar por episodios no esta habilitada, se podrá seleccionar
la unidad de conteo prestación, paciente o factura.

### Jerarquía de episodios

Si la opción agrupar por episodios esta habilitada se generarán un menú con el
título **Relacionar episodios por**. En este se selecciona la variable que
relaciona los episodios de los pacientes. Dependiendo del sistema de
información o el fin del análisis, esta variable puede cambiar. Usualmente es
el número de factura, el número de identificación del paciente o algún otro
identificador.

La jerarquía muestra cuatro secciones, una por unidad de conteo. Para mover un
agrupador de una unidad de conteo a otra se debe hacer clic en el botón del
agrupador y arrastrar hasta la unidad de conteo deseada. En la sección de
episodios las prioridades se definen de izquierda a derecha - arriba a abajo.

## Generar

Para ejecutar la función principal del módulo se debe hacer clic el botón
generar en la parte inferior de la sección.

