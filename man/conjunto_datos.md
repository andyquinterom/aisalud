# Conjunto mínimo de datos AISalud

Para poder correr la versión oficial de Analítica Integrada Salud se requiere
tener un conjunto de datos mínimo. Este conjunto de datos sería una tabla donde
cada fila representa un registro de prestación.

## Variables mínimas

Las variables mínimas son necesarias para poder correr la aplicación.

|Nombre de columna|Descripción|Tipo|
|-|-|-|
|fecha_prestacion|Columna con la fecha en que se prestó el servicio|Fecha|
|nro_identificacion|identificador único del paciente|Char|
|valor|Valor total del servicio prestado|Double|
|cantidad|Cantidad del servicio prestado o tecnología|Double|

## Variables recomendadas

|Nombre de columna|Descripción|Tipo|
|-|-|-|
|nro_factura|Identificador único de una factura|Char|
|nro_episodio|Identficador único de un episodio|Char|
|edad|Edad del paciente al momento de recibir la atención|Int / Double|

## Otras variables

### Recomendaciones

Se recomienda que las otras variables del conjunto de datos no contengan valores
NA (vacios). Para óptimo uso se recomienda que estos valores sean reemplazados
con algún identificador.
