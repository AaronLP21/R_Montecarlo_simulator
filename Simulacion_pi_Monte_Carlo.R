"
@Autor   : Aaron Lopez Pedraza
@Fecha   : 04-07-2020
@Version : 1.3

Contacto : aaron21@ciencias.unam.mx


Programa que simula mediante el método de Monte Carlo la constante 'Pi'
y además visualizamos las aproximaciones a lo largo de 'n'

La funcion 'puntos_en_Unitario' justifica geometricamente el metodo, la funcion
'simulacion_PI_MC' calcula las aproximaciones y 'simulacion_PI_MC_Grafica' visualiza a la 
funcion anterior.


Para navegar en las secciones del script aprieta el comando: Shift + Alt + j

crear divisiones del script con : ctrl + shift + R 

"


# Biblioteca ---------------------------------------------------------------

install.packages("ggplot2")  #se debe instalar esta libreria para visualizar una funcion
library(ggplot2)             #la mandamos a llamar


# Funciones ---------------------------------------------------------------


"
La funcion (puntos_en_Unitario ) muestra geometricamente los puntos que caen en el circulo unitario
y los que no caen, de tal forma sucede la aproximacion Monte Carlo a Pi
mediante lanzamientos aleatorios en un circulo unitario inscrito en un cuadradado de area 4

"


puntos_en_Unitario<-function(lanzamientos){#recibe soo un parametro
  
  #primero definimos el radio del circulo  
  radio <- 1
  
  #ahora las coordenadas de donde caera el punto lanzado aleatoriamente
  x <- runif(lanzamientos, 0, radio)#numero de valores aleatorios uniformes entre 0 y 1 para x
  y <- runif(lanzamientos, 0, radio)#los mismo para 'y'
  
  norma <- sqrt(x^2 + y^2)#la norma
  
  intra_circulo<- norma < radio    #son los pares (x,y) que caen en el unitario
  
  "
  Solo vamos a graficar en el primer cuadrante para ilustrar lo que esta sucediendo.
  ggplot es una funcion que genera graficos interactivos por capas
  
  
  "
  cuadrante <- ggplot(data.frame(x, y, intra_circulo), aes(x, y, color=intra_circulo)) +
    theme_minimal() + #aplicamos capa de tema
    guides(color=FALSE) +  #no dejamos el mapeo de la escala
    geom_point(size=0.9) + #tamanio de los puntos, entre mas pequenio; mas puntos caben
    ggtitle(paste(lanzamientos, '   Puntos aleatorios')) #titulo del grafico concatenado
  
  return(cuadrante)#le pedimos a la funcion que almacene el resultado para mandarlo a llamar despues
}


"
La funcion (simulacion_PI_MC) aproximara a Pi a traves del metodo de Monte Carlo, 
el ususario decide el numero de simulaciones

"

simulacion_PI_MC <- function(tamanio){
  
  resultados = rep(0,tamanio)#vector donde acumularemos las simulaciones
  
  puntos_en_CI = 0#inicializamos la variable que contara cuantos numeros caen en el circulo unitario
  
  for(i in 1:tamanio){#este ciclo se ejecutara el numero de veces que el usuario eligio
    
    x = runif(2,-1,1)#creamos dos numeros entre -1 y 1 con distribucion uniforme
    
    if(sqrt(x[1]*x[1] + x[2]*x[2]) <= 1){#condicionamos a que los valores aleatorios caigan en el unitario para contarlos
      puntos_en_CI = puntos_en_CI + 1    #contamos las veces que la simulacion cae en el unitario, se actualiza la variable
      #es nuestra indicadora
    }
    proporcion = puntos_en_CI / i   #la proporcion de los caen en el unitario con respecto al total de puntos
    
    
    pi_aprox = proporcion *4        #el estimador Monte Carlo, despejamos pi 
    
    resultados[i] = pi_aprox        #cada estimacion de Pi la almacenamos en el vector
  }
  return(resultados)#le pedimos que nos regrese el valor acualizado
}



"
La funcion (simulacion_PI_MC_Grafica) toma las simulaciones generadas por la funcion (simulacion_PI_MC)
y las grafica para ver como las simulaciones, al crecer n; tienden a Pi

"

simulacion_PI_MC_Grafica<-function(simulaciones,tamanio){#esta funcion recibe dos parametros
  
  "
  Para la grafica, plot() va a tomar de las simulaciones generadas anteriormente que el usuario
  le indique (tamanio), los demas parametros son para edite el grafico
  
  "
  plot(simulaciones[1:tamanio], type = 'l', col = "green3" ,lwd=1.5 , 
       main = 'Simulacion Monte Carlo de PI', xlab = 'Número de simulaciones',
       ylab = 'Valores de la simulaciones')
  
  abline(h=pi,col='red')#garficamos la constante h(x)=PI
  
  #esta funcion es para dar formato y estilo al recuadro de leyenda del grafico
  legend("topright",legend = c("Simulaciones", "PI"), col = c(3,2),pch = 16,
         pt.cex = 1,xjust = 1,yjust = 1, y.intersp = .5)#acotaciones de las letras
}



# Aplicacion de las funciones ----------------------------------------------

puntos_en_Unitario(10000)#como se generan las simulaciones

simulacion_PI_MC(500)#simulaciones

simulacion_PI_MC_Grafica(simulacion_PI_MC(1000),1000)#VISUALIZAMOS

