# Outliers

El módulo de **Outliers** permite la identificación de pacientes que representan
casos atípicos debido a su alto valor al ser comparados con el paciente medio,
y a su vez permite la exclusión de estos. Lo que es muy útil cuando estamos
utilizando estadísticos descriptivos en nuestro análisis, ya que los outliers
generan un sesgo hacia los valores más altos. Para poder completar con éxito la
exclusión de los pacientes outliers es necesario el manejo de los **filtros**,
si no domina el manejo de estos se recomienda una revisión del manual.

## Identificación de pacientes

La identificación de pacientes outliers se puede hacer con dos diferentes
métodos de cálculo utilizados en la estadística descriptiva.

1.  Percentil
2.  Rango inter cuartil

Es importante entender como funcionan estos dos métodos y sus diferencias.

### **Extracción por percentil**

Una vez se ha seleccionado **percentil** como el **método de cálculo** a
utilizar, se tiene una barra deslizante como la siguiente para que el usuario
escoja el percentil a utilizar.

El rango de selección va desde 75 hasta 99 y se debe seleccionar un número
múltiplo de 5 (con excepción de 99). La escogencia del **percentil 90** como
método de cálculo indica que vamos a obtener un conjunto de pacientes compuesto
por el 10% de pacientes con un mayor valor acumulado, si se selecciona el
**percentil 99** vamos a obtener el 1% de pacientes con los valores más altos.

### Extracción por IQR (Rango inter cuartil)

Una vez se ha seleccionado **Rango inter cuartil** como **método de cálculo**,
se presenta la siguiente barra deslizante para que el usuario pueda escoger que
tan exigente necesita que sea el ejercicio de la identificación de los
pacientes outliers.

En la barra de selección se encuentran los valores: 1.5, 3, 6, 12, 24. Este
número indica un valor multiplicativo por el cual extrapolar el rango
intercaurtil. Entre mayor sea el número escogido menor será el número de
pacientes outliers, o incluso puede presentarse el caso en el cual no los haya
al realizar el cálculo por este método.

## **Ejecutar**

Una vez seleccionados los parámetros para la identificación de los pacientes
outliers, se puede hacer clic en **Ejecutar**. La opción **Frecuencia mínima**
permite escoger el número mínimo de prestaciones que un paciente debe tener
para ser tenido en cuenta al momento de hacer el cálculo.

La aplicación nos presentará una tabla en donde se encuentra el **número de
identificación** por paciente, la suma del **valor**, el conteo de
**prestaciones**, y el peso o la participación del paciente en el valor total
en la columna **contribución.**

## **Excluir pacientes seleccionados**

Identificados los **pacientes outliers,** estos pueden ser excluidos muy
fácilmente. Primero se deben seleccionar los pacientes que se quieren excluir,
lo cual se hace dando clic encima de ellos y luego dar clic en **Excluir
pacientes seleccionados**. Para completar con éxito el proceso, se debe dar
clic en **Aplicar** en la sección **Filtros**. Para mayor información se
recomienda leer el manual de **Filtros**.
