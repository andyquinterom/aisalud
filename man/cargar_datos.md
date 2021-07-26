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

> **Advertencia**: Esta funcionalidad esta hecha para manejar paqueños
> volumenes de datos, entre 0 - 100.000 registros. Los limites de tamaño
> pueden ser cambiados por un administrador. Se recomienda siempre trabajar
> con conjuntos de datos cargados en la nube.

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
especificar el formato de fechas a utilizar. Las fechas se deben codificar
de la siguiente manera.

| Código | Descripción                            | Ejemplo            |
|--------|----------------------------------------|--------------------|
| %d     | Día del mes                            | 01, 02, 03, 14, 31 |
| %j     | Día del año                            | 148, 188, 354      |
| %m     | Número del mes                         | 05, 07, 12         |
| %Y     | Año completo                           | 2002, 2020         |
| %y     | Año (Sin siglo)                        | 02, 20             |
| %U     | Semana 0-6 (Empieza domingo)           | 22, 27             |
| %W     | Semana (Empieza lunes)                 | 21, 27             |
| %u     | Día de la semana 1-7 (Empieza domingo) | 1, 4, 7            |
| %w     | Día de la semana (domingo es 0)        | 0, 3, 6            |

**Ejemplos**:

| Ejemplo de fecha | Formato en palabras | Formato en código |
|------------------|---------------------|-------------------|
| 2020-02-28       | AÑO-MES-DÍA         | %Y-%m-%d          |
| 28-02-20         | DÍA-MES-AÑO         | %d-%m-%y          |
| 2020/148         | AÑO/DÍA             | %Y/%j             |

Finalmente, se hace click en aplicar y se espera a que los datos se carguen.
Al finalizar un gráfico de barras debe aparecer en el lado de derecho de la
pantalla.

## Perfiles

Los perfiles se pueden modificar desde la sección perfiles en al area derecha
de la pantalla. Los perfiles se almacenan en texto plano en formato JSON.
Aunque este disponible la interfaz para modificarlos dentro de AIS, no es
obligatorio y se pueden editar desde cualquier editor de texto.

Los perfiles en la versión actual de AIS siguen la siguiente estructura.

1. Nombre del perfil
    1. Jerarquia
        1. Unidad de conteo (episodio, paciente, factura, prestacion)
            1. Agrupadores

Al modificar los perfiles, se pueden guardar con el botón guardar en la parte
superior de la sección. Se abrirá un dialogo de contraseña en el cual se
ingresará la clave determinada por el administrador de la aplicación.

### Esquema JSON de los perfiles

```json
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "type": "object",
  "additionalProperties": {
    "type": "object",
    "properties": {
      "cantidad": {
        "type": "boolean"
      },
      "jerarquia": {
        "type": "object",
        "properties": {
          "episodio": {
            "type": "array",
            "items": {
              "type": "string"
            }
          },
          "paciente": {
            "type": "array",
            "items": {
              "type": "string"
            }
          },
          "factura": {
            "type": "array",
            "items": {
              "type": "string"
            }
          },
          "prestacion": {
            "type": "array",
            "items": {
              "type": "string"
            }
          }
        }
      }
    },
    "required": [
      "jerarquia"
    ]
  }
}
```

## Notas técnicas

Las notas técnicas se pueden modificar desde la sección de notas técnicas en
el area derecha de la pantalla. Las notas técnicas se archivan en texto plano
por lo cual se pueden modificar tanto en la interfaz de AIS o en cualquier
editor de texto.

Las notas técnicas llevan la siguiente estructura, sin embargo, datos
adicionales se pueden incluir solo si estos no interrumpen el esquema
obligatorio.

1. Nombre de la nota técnica (Obligatorio)
    1. poblacion (Obligatorio).
    2. departamento (Obligatorio)
    3. cod_departamento (Obligatorio)
    4. ciudades (Obligatorio)
    5. vigente (Obligatorio)
    6. agrupadores (Obligatorio)
        1. costo medio (Obligatorio)
        2. frecuencia (Obligatorio)
        3. frecuencia mínima
        4. frecuencia máxima
    7. prestador 
    8. asegurador
    9. notas
    10. inclusiones
    11. exclusiones
    12. perfil (debe ser un perfil valido)

Al modificar las notas técnicas, se pueden guardar con el botón guardar en la
parte superior de la sección. Se abrirá un dialogo de contraseña en el cual se
ingresará la clave determinada por el administrador de la aplicación.

### Esquema JSON de las notas técnicas

```json
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "type": "object",
  "additionalProperties": {
    "type": "object",
    "properties": {
      "poblacion": {
        "type": "number"
      },
      "prestador": {
        "type": "string"
      },
      "asegurador": {
        "type": "string"
      },
      "departamento": {
        "type": "string"
      },
      "cod_departamento": {
        "type": "integer"
      },
      "ciudades": {
        "type": "string"
      },
      "vigente": {
        "type": "boolean"
      },
      "perfil": {
        "type": "string"
      },
      "notas": {
        "type": "string"
      },
      "exclusiones": {
        "type": ["array", "string"]
      },
      "inclusiones": {
        "type": ["array", "string"]
      },
      "agrupadores": {
        "type": "object",
        "additionalProperties": {
          "type": "object",
          "properties": {
            "cm": {
              "type": "number"
            },
            "n": {
              "type": "number"
            },
            "n_max": {
              "type": "number"
            },
            "n_min": {
              "type": "number"
            }
          },
          "required": ["cm", "n"]
        }
      }
    },
    "required": [
      "poblacion",
      "departamento",
      "cod_departamento",
      "ciudades",
      "vigente",
      "agrupadores"
    ]
  }
}
```
