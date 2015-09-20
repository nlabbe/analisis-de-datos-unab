---
title: "Desarrollo Laboratorio Nº6"
subtitle: Análisis de Datos
author1: "Profesores: Ramón H. Cornejo-Muñoz y Felipe Rojas"
author2: "Profesor Ayudante de Laboratorio: Mauricio Vargas"
author3: "Ayudantes: Franco Mansilla y Mauricio Díaz"
job: "Universidad Nacional Andrés Bello"
logo: logounab.png
license : by-nc-sa
hitheme: tomorrow
framework: io2012
highlighter: highlight.js
widgets: mathjax
url:
  assets: ../../assets
  lib: ../../libraries
mode: selfcontained # {standalone, draft}
<!-- knit : slidify::knit2slides --> 
knit : slidify::knit2slides
---

## Modelos anidados (1)

En el <a href="https://www.youtube.com/watch?v=IUvu3Jsyyfw">complemento del Laboratorio Nº4</a> y el <a href="http:"http://pachamaltese.github.io/analisis-de-datos-unab/laboratorios/laboratorio5/desarrollo-laboratorio5.html">Laboratorio Nº5</a> vimos la base de datos `appleProdFr86`. Para estos datos comparamos el ajuste de cuatro funciones de producción.

1. Lineal: $y = \beta_0 + \sum_i \beta_i x_i$
2. Cuadrática: $y = \beta_0 + \sum_i \beta_i x_i + \frac{1}{2} \sum_i \sum_j \beta_{ij} x_i x_j$
3. Cobb-Douglas: $y = A \prod_i x_i^{\beta_i}$
4. Translogarítmica: $\ln y = \beta_0 + \sum_i \alpha_i \ln x_i + \frac{1}{2} \sum_i \sum_j \beta_{ij} \ln x_i \ln x_j$

Las primeras dos se pueden comparar con las herramientas para modelos anidados y la función cuadrática tiene mejor ajuste.

Las últimas dos se pueden comparar de igual manera y la función translogarítmica tiene mejor ajuste.

---

## Modelos no anidados (1)

¿Cómo comparar el ajuste de las funciones cuadrática y translogarítmica?

* Las variables dependientes son distintas ($y$ versus $\ln(y)$)
* No se pueden usar las herramientas de modelos anidados
* No se pueden comparar los valores de $R^2$ directamente
* Para comparar se debe calcular, por ejemplo, el valor hipotético de $y$ aplicando una transformación exponencial a la función translogarítmica y se obtendría un $R^2$ comparable
* El $R^2$ comparable sería a partir de los valores observados de $y$ y la diferencia existente respecto de la estimación usando la función translogarítmica

---

## Modelos no anidados (2)

Se procede con base en el código del <a href="http:"http://pachamaltese.github.io/analisis-de-datos-unab/laboratorios/laboratorio5/desarrollo-laboratorio5.html">Laboratorio Nº5</a>



```r
library(miscTools)
summary(prodQuad)$r.squared
```

```
## [1] 0.8448983
```

```r
rSquared(data$qOut, data$qOut - data$qOutTL)
```

```
##           [,1]
## [1,] 0.7696638
```
$\Longrightarrow$ la función cuadrática tiene mejor ajuste en $y$

---

## Modelos no anidados (3)

Aplicando una transformación logarítmica a la función cuadrática

```r
summary(prodTL)$r.squared
```

```
## [1] 0.6295696
```

```r
rSquared(log(data$qOut), log(data$qOut) - log(data$qOutQuad))
```

```
##           [,1]
## [1,] 0.5481309
```
$\Longrightarrow$ la función translogarítmica tiene mejor ajuste en $\ln(y)$

---

## Modelos no anidados (4)

* En este caso la comparación de los $R^2$ no ayuda a elegir el modelo con mejor ajuste
* Se puede usar $R^2$ ya que la cantidad de coeficientes en ambos modelos es la misma
* Si los modelos tienen distinta cantidad de coeficientes se debe usar $\bar{R}^2$

---

## Modelos no anidados (5)

Como alternativa se puede ver la consistencia teórica de ambos modelos. En el complemento del Laboratorio Nº5 vimos que algunas observaciones tienen productividad marginal negativa cuando se ajusta una función translogarítmica. El problema de esto es que hace que la elasticidad producto-factor sea negativa.

![plot of chunk unnamed-chunk-4](assets/fig/unnamed-chunk-4-1.png) 

---

## Modelos no anidados (6)

Cuando se ajusta una función translogarítmica ocurre lo mismo

![plot of chunk unnamed-chunk-5](assets/fig/unnamed-chunk-5-1.png) 

---

## Modelos no anidados (7)

Desde un punto de vista teórico se esperaría es que la elasticidad de escala sea positiva. En los gráficos se observa que hay varias observaciones en las que esto no se cumple.


```r
with(data, sum(eCapQuad < 0) + sum(eLabQuad < 0) + sum(eMatQuad < 0))
```

```
## [1] 41
```

```r
with(data, sum(eCapTL < 0) + sum(eLabTL < 0) + sum(eMatTL < 0))
```

```
## [1] 54
```

---

## Modelos no anidados (8)

Al menos no hay cantidades negativas en las predicciones

```r
with(data, sum(qOutQuad <0))
```

```
## [1] 0
```

```r
with(data, sum(qOutTL < 0))
```

```
## [1] 0
```

---

## Modelos no anidados (9)

|                               | Cuadrática | Translogarítmica |
|---                            |---         |---               |
| $R^2$ en $y$                  | 0.84       | 0.77             |
| $R^2$ en $ln(y)$              | 0.55       | 0.63             |
| Obs. con elasticidad negativa | 41         | 54               |
| Obs. con cantidad negativa    | 0          | 0                |

$\Longrightarrow$ no es claro cual especificación elegir. 

En síntesis:

* Se podría decir que se prefiere la función cuadrática por tener mejor ajuste, pero ambas tienen observaciones con elasticidad negativa.
* La presencia de elasticidades negativas es un indicio de mala especificación del modelo.
* Para solucionar esto se podría usar enfoque distinto al modelo de regresión lineal que permite solucionar el problema de la elasticidad negativa. Dicho enfoque corresponde a la regresión no paramétrica.

---

## Profundizando modelos multivariados (1)

Trabajaremos con la base de datos `swiss` (viene en la instalación de R) que contiene una medición estandarizada de la fertilidad e indicadores socioeconómicos para 47 familias de Suiza en 1888.

Se tienen 47 observaciones en 6 variables y cada observación se mide en porcentaje.

* Fertilidad            medición estandarizada de fertilidad
* Agricultura           % de hombres que trabajan en la agricultura
* Examinación           % de reclutas que obtienen alta calificación en el ejército
* Educación             % de jóvenes que tienen instrucción primaria o más
* Católicos             % de católicos (distinto de protestantes)
* Mortalidad infantil   nacidos vivos que viven menos de un año

De acuerdo a la documentación las variables excepto fertilidad representan una proporción de la población. 

---

## Profundizando modelos multivariados (2)

Vamos a graficar la correlación y la distribución de los datos:

```r
require(datasets); data(swiss)
require(GGally); require(ggplot2)
```

```
## Loading required package: GGally
## Loading required package: ggplot2
```

```r
g = ggpairs(swiss, lower = list(continuous = "smooth"), params = c(method = "loess"),
            axisLabels = 'show')
```

---

## Profundizando modelos multivariados (3)

![plot of chunk unnamed-chunk-9](assets/fig/unnamed-chunk-9-1.png) 

---

## Profundizando modelos multivariados (4)

`summary(lm(Fertility ~ Agriculture + Education + Catholic + Infant.Mortality, data = swiss))`

```r
Coefficients:
                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)      62.10131    9.60489   6.466 8.49e-08 ***
Agriculture      -0.15462    0.06819  -2.267  0.02857 *  
Education        -0.98026    0.14814  -6.617 5.14e-08 ***
Catholic          0.12467    0.02889   4.315 9.50e-05 ***
Infant.Mortality  1.07844    0.38187   2.824  0.00722 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 7.168 on 42 degrees of freedom
Multiple R-squared:  0.6993,	Adjusted R-squared:  0.6707 
F-statistic: 24.42 on 4 and 42 DF,  p-value: 1.717e-10
```

---

## Profundizando modelos multivariados (4)

En la regresión anterior:

* La variable agricultura se expresa en porcentajes (0 - 100)
* El coeficiente de la variable agricultura es -0.1721.
* De acuerdo al modelo se espera una disminución de 0.17% en la fertilidad por un aumento de 1% en el porcentaje de hombres que trabajan en l agricultura (estando todas las demás variables constantes).
* El test t $H_0: \beta_{Agri} = 0$ versus $H_A: \beta_{Agri} \neq 0$ es significativo

---

## Profundizando modelos multivariados (5)

Es interesante que si se hace una regresión usando sólo agricultura como regresor el coeficiente cambia de signo




```r
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 60.30438    4.25126  14.185   <2e-16 ***
Agriculture  0.19420    0.07671   2.532   0.0149 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 11.82 on 45 degrees of freedom
Multiple R-squared:  0.1247,	Adjusted R-squared:  0.1052 
F-statistic: 6.409 on 1 and 45 DF,  p-value: 0.01492
```

---

## Profundizando modelos multivariados (6)

En síntesis:

* El signo del coeficiente de la variable agricultura cambia con la incorporación de los regresiones examinación y educación.
* El porcentaja de hombres que trabajan en agricultura está negativamente correlacionado con el nivel de educación.
* Examinación y educación miden cosas similares (y tienen una correlación fuertemente postiva).

Preguntas relevantes:

* ¿Es el coeficiente positivo en la segunda regresión una evidencia de que no se han incluido varibles relevantes como educación u otra?
* ¿La variable educación tiene un efecto más importante que agricultura?
* ¿Se podría plantear que una región dedicada fuertemente a la agricultura tiene mayor tasa de fertilidad?

---

## Profundizando modelos multivariados (7)

¿Qué ocurre si se incluye una variable no relevante?

Si se tiene una variable, digamos $z$, que es una combinación lineal de otras variables de la regresión, R descarta dicha variable.


```r
Z <- swiss$Agriculture + swiss$Education
lm(Fertility ~ Agriculture + Education + Catholic + Infant.Mortality + Z, data = swiss)
```

```r
Coefficients:
  (Intercept)   Agriculture   Education   Catholic    Infant.Mortality    Z
  62.1013       -0.1546       -0.9803     0.1247      1.0784              NA
```

---

## Profundizando modelos multivariados (7)

Considere el modelo lineal
$$
Y_i = \beta_0 + \beta_1 X_{i1} + \varepsilon_{i}
$$
donde $X_{i1}$ es binaria y toma valor 1 si el individuo $i$ pertenece a determinado grupo y 0 en otro caso (e.g. grupo control versus grupo experimental)

En este modelo:

* Para los individuos que pertenecen al grupo $E[Y_i] = \beta_0 + \beta_1$.
* Para los individuos que no pertenecen al grupo $E[Y_i] = \beta_0$.
* La estimación por OLS entrega un valor $\hat \beta_0 + \hat \beta_1$ que corresponde a la media de $Y_i$ para los individuos pertenecientes al grupo y $\hat \beta_0$ para los individuos que no pertenecen al grupo.
* $\beta_1$ se interpreta como la diferencia con respecto a la media de $Y_i$ entre los individuos que componen el grupo y los que están fuera de este.
* Incluir una variable binaria para señalar la no pertenencia al grupo es redundante.

---

## Profundizando modelos multivariados (8)

Considere el siguiente modelo lineal
$$Y_i = \beta_0 + \beta_1 X_{i1} + \beta_2 X_{i2} + \varepsilon_i$$
Suponga que las personas de la muestra viven en las ciudades de Santiago, Concepción o Valdivia. $X_{i1}$ tiene valor 1 para los habitantes de Santiago y $X_{i2}$ tiene valor 1 para los habitantes de Concepción.

En este modelo:

* Si el individuo $i$ vive en Santiago $E[Y_i] = \beta_0 +\beta_1$.
* Si el individuo $i$ vive en Concepción $E[Y_i] = \beta_0 +\beta_2$.
* Si el individuo $i$ vive en Valdivia $E[Y_i] = \beta_0$.
* $\beta_1$ compara las medias de $Y_i$ entre los habitantes de Santiago y Valdivia
* $\beta_2$ compara las medias de $Y_i$ entre los habitantes de Concepción y Valdivia.
* $\beta_1 - \beta_2$ compara las medias de $Y_i$ entre los habitantes de Santiago y Concepción.
