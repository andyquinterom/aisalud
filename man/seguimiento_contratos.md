# Seguimiento de contratos

El módulo de seguimiento de contratos permite evaluar la ejecución de un
contrato durante un lapso determinado de tiempo. Se permite la comparación entre
el valor ejecutado con el valor contratado y frecuencia ejecutada con
frecuencia contratada.

## Parametros especiales

Para poder hacerle seguimiento a una nota técnica se debe seleccionar esta en
la casilla **Comparar con nota técnica**. Después, se debe seleccionar la
variable en el conjunto de datos cargado que contenga los agrupadores incluidos
en la nota técnica. Finalmente, se debe seleccionar en la casilla
**Seguimiento** cuales comparaciones hacer (por valor facturado y/o
frecuencias).

## Frecuencias

La comparación de frecuencias muestra las diferencias en ejecución de
frecuencias de cada uno de los agrupadores de la nota técnica.

### Resumen

1. **Valor a ejecutar**: La suma del costo medio multiplicado por la frecuencia
contratada de cada agrupador.
2. **Valor ejecutado con costo medio**: La suma de las frecuencias ejecutadas
multiplicadas por los costos medios de cada agrupador de la nota técnica.
3. **Diferencia de valor total**: El valor a ejecutar menos el valor ejecutado
con costo medio.
4. **Porcentaje del valor ejecutado**: El valor ejecutado con costo medio sobre
el valor a ejecutar.

### Tablas

1. **Ejecución de frecuencias**:
    1. Ejecución total: La suma de las frecuencias de cada mes.
    2. Ejecución media: La ejecución media de las frecuencias.
2. **Ejecución por costo medio**: La multiplicación de las frecuencias
ejecutadas por el costo medio de cada agrupador.
3. **Diferencias de frecuencia con costos medios**: El valor a mes contratado
menos la ejecución por costo medio.

## Valor facturado

La comparación de valor facturado muestra las diferencias entre el valor
ejecutado en el conjunto de datos con el valor contratado en la nota técnica.

### Resumen

1. **Valor a ejecutar**: La suma del costo medio multiplicado por la frecuencia
contratada de cada agrupador.
2. **Facturado**: La suma del valor facturado para los agrupadores de la nota
técnica.
3. **Diferencia de valor total**: El valor a ejecutar menos el valor facturado.
4. **Porcentaje del valor ejecutado**: El valor facturado sobre el valor a
ejecutar.

## Tablas

1. **Ejecución**: El valor facturado por mes por agrupador de la nota técnica.
2. **Diferencias de valor**: El valor a mes contratado menos la ejecución.
3. **Diferencias de valor en porcentaje**: La ejecución sobre el valor a mes
contratado.
