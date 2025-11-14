# Caso Final – Analítica  
## **Factores que afectan la cantidad de check-ins diarios en los hoteles Caesars Las Vegas**

Este repositorio corresponde al caso **Caesars Entertainment**, presentado como proyecto final en el curso de Analítica de Datos de la Pontificia Universidad Javeriana.

Contiene los **datos**, **scripts** y **reportes** asociados al análisis del comportamiento de los check-ins diarios en los hoteles de Caesars Las Vegas, con el objetivo de identificar los factores que influyen en la demanda y construir modelos predictivos para optimizar la asignación del personal en recepción.

---

## Resumen

El caso aborda un desafío operativo clave en la industria hotelera: **anticipar la cantidad de check-ins diarios** con el fin de equilibrar el nivel de servicio y los costos laborales.

Caesars Entertainment enfrenta variabilidad diaria en llegadas debido a factores como:

- Precio promedio de habitaciones (FIT ADR)  
- Segmento del cliente (Casino, Group, FIT)  
- Día de la semana  
- Mes del año  
- Estacionalidad  
- Eventos especiales (Super Bowl, Año Nuevo, San Valentín, etc.)  

El análisis desarrollado en R busca responder preguntas como:

- **¿Qué factores explican mejor la variación diaria en los check-ins?**  
- **¿Qué tan fuerte es la estacionalidad semanal y mensual?**  
- **¿Cómo impactan los eventos especiales en la demanda?**  
- **¿Es posible predecir cuándo habrá alta o baja demanda?**

El trabajo replica y extiende el análisis cuantitativo mediante modelos estadísticos, integrando datos operativos del hotel, variables temporales y eventos externos para evaluar patrones, correlaciones y desempeño predictivo.

---

## Estructura del Repositorio

### **Carpeta Document**

- **Caso_Caesars.pdf:** Documento del caso original, con contexto del sector hotelero, descripción del problema operativo y variables involucradas.  
- **Analisis_Caesars.pdf:** Documento con el análisis completo, visualizaciones, modelos y conclusiones del proyecto final.

---

### **Carpeta Scripts**

El análisis se realiza en **R (versión ≥ 4.0.0)**.

Incluye scripts que realizan:

- Lectura y preparación de datos  
- Limpieza y transformación  
- Construcción de tablas descriptivas  
- Visualización de tendencias y patrones  
- Modelos de regresión lineal simple  
- Modelos de regresión múltiple con interacciones y no linealidades  
- Modelo logístico para clasificación de alta/baja demanda  
- Comparación y evaluación de modelos  

---

### **Carpeta Data**

Contiene la base de datos utilizada en el proyecto:

- **Data_Caesars.xlsx:**  
  Dataset con:  
  - Check-ins diarios  
  - Tarifas (FIT ADR)  
  - Segmentos de cliente  
  - Estacionalidad  
  - Eventos especiales  
  - Días de la semana  
  - Meses  
  - Variables transformadas y rezagos  

---

## Autores

**Valeria Garcia Torres**  
**Valery Ramírez Mejía**

Pontificia Universidad Javeriana  
Facultad de Ciencias Económicas y Administrativas  
Curso: *Analítica de Datos*  
Año: **2025**

---

