# Agrupadores

Los agrupadores son esenciales para el funcionamiento de Analítica Integrada.
Estos definen casi todas las funcionalidades de la herramienta y además son
de gran importancia comprender para el análisis y seguimiento de pagos
agrupados.

## ¿Qué es un agrupador?

Un agrupador es la categoría de uno o varios servicios. Un agrupador o un grupo
de agrupadores pueden variar en detalle y cobertura. Por ejemplo, el tipo de
servicio y el nombre de la prestación son dos agrupadores muy diferentes en
detalle; el nombre de la prestación generalmente agrupa solo una prestación
mientras que el tipo de servicio agrupa cientos de diferentes prestaciones. El
número de identificación también es un agrupador, dado que agrupa los servicios
prestados a un paciente.

Desde un punto de vista técnico, los agrupadores son las relaciones entre
registros de prestación. Un agrupador puede incluir varias llaves primarias.
Por ejemplo, un agrupador que tome en cuenta el centro de costos, el ámbito y
el código CUPS es válido.

## Unidades de conteo

Las unidades de conteo nos permiten observar el comportamiento de los
agrupadores de diferentes maneras. Típicamente, las unidades de conteo son:
prestaciones, pacientes, facturas o episodios. Por prestación, se suman y se
agregan registros, por pacientes y facturas se agregan pacientes y facturas
respectivamente. Por ejemplo, puede que la frecuencia para un agrupador
utilizando la unidad de conteo prestaciones sea mucho más alta que la
frecuencia del mismo por paciente; sin embargo la suma final debe ser igual. En
el caso de pacientes y facturas, todos los registros relacionados con un
agrupador y a un paciente o factura se considerarán cómo un solo registro

## Episodios

Los episodios tienen una particularidad que los diferencia de las otras
unidades de conteo. Aunque los episodios de por sí dependen de otras unidades
de conteo (sea pacientes, facturas, número de ingreso, etc.), los episodios
tienen la posibilidad de incluir registros que no estén directamente
relacionados con agrupador.

Por ejemplo, si un paciente se hospitaliza y durante su estancia recibe una
cirugía, esta sería incluida en el episodio. Aunque el agrupador de
hospitalización sea diferente al de cirugía, si hospitalización tiene mayor
prioridad en la jerarquía de episodios, este incluirá las prestaciones
relacionadas con la cirugía.

¿Entonces, cómo se determina que registros se asocian al episodio y cuáles no?
Al calcular un episodio se debe tener una columna que sume los registros. Esta
puede ser un número de episodio, una factura, un número de identificación del
paciente, etc. Por ejemplo, si se desea hacer seguimiento a episodios cortos,
la factura o el número de episodio pueden ser buenas columnas para sumar. Si se
desea hacer seguimiento a planes de largo plazo a través de múltiples meses,
puede que el número de identificación del paciente sea apropiado.

### Jerarquía

La jerarquía define que agrupadores tienen mayor prioridad al calcularse por
episodio. En el ejemplo de la estancia y la cirugía se asume que la estancia
tiene mayor prioridad que la cirugía; sin embargo, si la cirugía tuviera mayor
prioridad que la estancia, todas las prestaciones asociadas se sumarían a
cirugía.
