---
  
  ## Modelos no anidados (10)
  
  La estimación cuadrática tiene mejor $\bar{R}^2$ que la translogarítmica pero la forma funcional genera una elasticidad producto-factor negativa. Más aún, en algunos casos la elasticidad de escala será negativa.

Ajustar una función de manera que se obtenga un buen ajuste y las productividades estimadas sean razonables no es simple ya que se deberían probar varios modelos hasta encontrar el más adecuado.

$\Rightarrow$ se puede estimar una regresión no paramétrica que no depende de una forma funcional.

--- 
  
  ## Regresión no paramétrica (1)
  
  Se trabaja con un modelo de la forma 
$$Y_i = m(X_i) + \varepsilon_i$$
  
  Ventajas: 
  
  * No se define una función a priori
* Se busca la forma funcional de $m$ de mejor ajuste a partir de los datos (e.g. función lineal o polinomial por partes)

Desventajas: 
  
  * Computacionalmente es más costoso
* No hay coeficientes de regresión (el foco es la función de regresión)
* Difícil de interpretar

---
  
  ## Regresión no paramétrica (3)
  
  La idea geométrica de lo anterior es lo siguiente:
  
  <center>
  <img src="intuicion-no-parametrico.svg">
  </center>
  
  ---
  
  ## Regresión no paramétrica (4)
  
  Podemos estimar la producción considerando como único regresor el capital.
```{r results='hide'}
require(KernSmooth)

qOut <- as.vector(data$qOut)
qCap <- as.vector(data$qCap)

bw <- dpill(qCap,qOut)
fit1 <- locpoly(qCap, qOut, bandwidth = bw, degree = 1) # forma polinomial de grado 1
```

---
  
  ## Regresión no paramétrica (5)
  
  ```{r,fig.height=5}
plot(fit1$x, fit1$y, type = "l")
```

---
  
  ## Regresión no paramétrica (6)
  
  Es posible probar distintas especificaciones
```{r, results='hide'}
# forma polinomial de grado 2
fit2.1 <- locpoly(qCap, qOut, bandwidth = bw, degree = 2) 
fit2.2 <- locpoly(qCap, qOut, bandwidth = bw*2, degree = 2) #menos varianza / más sesgo
fit2.3 <- locpoly(qCap, qOut, bandwidth = bw/2, degree = 2) #más varianza / menos sesgo

# regresión lineal habitual
fit.lm <- lm(qOut ~ qCap)
```

---
  
  ## Regresión no paramétrica (7)
  
  ```{r,fig.width=14,fig.height=7.5, echo=FALSE}
plot(fit1$x, fit1$y, type = "l");par(mfrow=c(2,2))
plot(fit1$x, fit1$y, type = "l");lines(fit2.1, col = "blue")
plot(fit1$x, fit1$y, type = "l");lines(fit2.2, col = "red")
plot(fit1$x, fit1$y, type = "l");lines(fit2.3, col = "green")
plot(fit1$x, fit1$y, type = "l");abline(fit.lm, col = "purple")
```

---
  
  ## Regresión no paramétrica (8)
  
  ```{r}
# modelo 1
fit1$y # hay observaciones que se indeterminan
fit1$x # el capital se divide en 401 valores e incrementos de 1393.9

# modelos 2, 3 y 4
fit2.1$y
fit2.2$y # menos observaciones indeterminadas
fit2.3$y

# se ajusta la banda y el grado
fit.sqrt <- locpoly(qCap, qOut, bandwidth = bw*3, degree = 0.5)
fit.sqrt$y # todas las observaciones definidas y no negativas
```

---
  
  ```{r}
plot(fit.sqrt$x, fit.sqrt$y, type = "l")
```


---
  
  ```{r}
median(fit.sqrt$y)/median(fit.sqrt$x) # cada unidad de capital produce en promedio 18.19 unidades 
```


<!--- 
  
  ## Regresión no paramétrica (2)
  
  Una metodología no paramétrica (Epanechnikov) consiste en:
  
  * Estimar una regresión para cada observación
* Tomar cada observación de la muestra y asignar ponderadores a todas las demás según que tan lejos o cerca están de la referencia
* Repetir el proceso iterativamente hasta obtener el mejor ajuste
* Aplicar una validación cruzada a la estimación obtenida

--->
  
  <!---
  
  ## Regresión no paramétrica (3)
  
  Se estimará una regresión no paramétrica con todos los inputs y el output en logaritmos
```{r, eval=TRUE, results='hide'}
library(np)
prodNP <- npreg(log(qOut) ~ log(qCap) + log(qLab) + log(qMat), regtype = "ll", 
                bwmethod = "cv.aic", ckertype = "epanechnikov",  data = data, 
                gradients = TRUE)
```

--->
  
  <!---
  
  ## Regresión no paramétrica (4)
  
  `summary(prodNP)`
```
Regression Data: 140 training points, in 3 variable(s)
log(qCap) log(qLab) log(qMat)
Bandwidth(s):  1.039647    332644 0.8418465

Kernel Regression Estimator: Local-Linear
Bandwidth Type: Fixed
Residual standard error: 0.6227669
R-squared: 0.6237078

Continuous Kernel Type: Second-Order Epanechnikov
No. Continuous Explanatory Vars.: 3
```

--->
  
  <!---
  
  ## Regresión no paramétrica (5)
  
  Gráficos ceteris paribus (las variables no graficadas se mantienen fijas en un valor igual a su mediana)
```{r,echo=FALSE, fig.width=12, fig.height=7}
plot(prodNP, plot.errors.method = "bootstrap")
```

--->
  
  <!---
  
  ## Regresión no paramétrica (6)
  
  Test de significancia: `npsigtest(prodNP)`
```
log(qCap) log(qLab) log(qMat)
Bandwidth(s): 0.9898624  285519.9 0.8432296

Individual Significance Tests
P Value: 
  log(qCap) 0.13033  
log(qLab) < 2e-16 *** 
  log(qMat) < 2e-16 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```
$\Longrightarrow$ con un nivel de significancia de 10% la única variable cuyos efectos son no significativos es el capital.

--->
  
  <!---
  
  ## Regresión no paramétrica (7)
  
  Histogramas de elasticidad producto-factor y elasticidad de escala en base a regresión no paramétrica
```{r,echo=FALSE,fig.width=12, fig.height=7}
par(mfrow=c(2,2)); hist(gradients(prodNP)[ ,1], xlab = "Capital", main=NULL); hist(gradients(prodNP)[ ,2], xlab = "Labour", main=NULL); hist(gradients(prodNP)[ ,3], xlab = "Materials", main=NULL); hist(gradients(prodNP), xlab = "Scale", main=NULL)
```

--->
  
  <!---
  
  ## Regresión no paramétrica (8)
  
  Relación tamaño de planta - elasticidad de escala

```{r,echo=FALSE,fig.width=8, fig.height=6}
plot(data$qOut, rowSums(gradients(prodNP)), log = "x", xlab = "qOut", ylab = "eScaleNP")
```

--->
  