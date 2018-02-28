#Tarea 2 aplicada
library(readxl)
empleados1 <- read_excel("empleados1.xlsx")
empleadis1M<-empleados1[c(4,10,12,26,28,33,43,45,56,71,79,95),]
empleados1H<-empleados1[-c(4,10,12,26,28,33,43,45,56,71,79,95),] #Me deja solo los hombres para hacer el muestreo
sample(1:87,12) #Me genera 12 números aleatorios, correspondientes a los hombres que vamos a muestrear
#Los generados fueron: (10,29,9,13,47,45,2,79,69,15,41,3)
empleados1H<-empleados1H[c(10,29,9,13,47,45,2,79,69,15,41,3),] #Muestra de 12 hombres obtenida
empleados1<-rbind(empleados1H,empleadis1M) #Me pone en una misma base de datos los hombres y la mujeres, esta será nuestra base de datos final.


