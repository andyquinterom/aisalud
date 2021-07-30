# Seguimiento de contratos

El módulo de seguimiento de contratos permite evaluar la ejecución de un
contrato durante un lapso determinado de tiempo. Se permite la comparación entre
el valor ejecutado con el valor contratado y frecuencia ejecutada con
frecuencia contratada.

## Parámetros especiales

Para poder hacerle seguimiento a una nota técnica se debe seleccionar esta en
la casilla **Comparar con nota técnica**. Después, se debe seleccionar la
variable en el conjunto de datos cargado que contenga los agrupadores incluidos
en la nota técnica.

## Resumen

1. **Valor a ejecutar**: La suma del costo medio multiplicado por la frecuencia
contratada de cada agrupador más o menos ajustes.
2. **Valor ejecutado con frecuencia**: La suma de las frecuencias ejecutadas
multiplicadas por los costos medios de cada agrupador de la nota técnica.
3. **Diferencia de valor con frecuencia**: La multiplicación del costo medio
   por las frecuencias de cada agrupador menos el valor a ejecutar.
4. **Porcentaje del valor ejecutado con frecuencia**: La multiplicación del
   costo medio por las frecuencias de cada agrupador sobre el valor a ejecutar.
5. **Valor ejecutado con facturación**: La suma de la facturación para los
   agrupadores de la nota técnica.
6. **Diferencia de valor con facturación**: La suma de la facturación para los
   agrupadores de la nota técnica menos el valor a ejecutar.
7. **Porcentaje del valor ejecutado con facturación**: La suma de la
   facturación para los agrupadores de la nota técnica sobre el valor a
   ejecutar.



## Tablas

1. **Frecuencia a mes contratada ajustada**: Esta tabla incluye la frecuencia
   efectiva reconocida por el pagador al prestador. Esta solo se ve afectada si
   una agrupación tiene límites de ejecución.
2. **Total pagador**: La multiplicación de la frecuencia a mes contratada
   ajustada multiplicada por el costo medio de la agrupación. Esta representa
   el valor contratado más o menos excedentes.
3. **Ajuste al valor del contrato**: Los excedentes por mes para agrupaciones
   con límites ejecución.

### Frecuencias

4. **Ejecución de frecuencias por mes**: La frecuencia de cada agrupación por
   mes.
5. **Ejecución de frecuencias por costo medio**: Multiplica la frecuencia por
   mes de agrupación con el costo medio contratado.
6. **Diferencia ejecución**: La diferencia entre la ejecución de frecuencias
   por costo medio y el total pagador.

### Valor facturado

7. **Valor facturado por mes**: El valor facturado para cada agrupación por
   mes.
8. **Diferencia ejecución**: La diferencia entre el valor facturado y el total
   pagador.
