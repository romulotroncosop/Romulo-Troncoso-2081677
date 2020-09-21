#Importar librerias
library("readr")
library("plotrix")

#Leer los datos
datos = read_csv('/home/romulo/Escritorio/UANL/Probabilidad/Tarea1/Cielo_abierto.csv')
datos_invernadero = read_csv('/home/romulo/Escritorio/UANL/Probabilidad/Tarea1/invernadero.csv')

#Definr variables
Ha_totales = 0
porcentaje = 0
Ha_perdidas= 0

#Ciclo para encontar las hectarias cosechadas
for(i in 1:26){
Ha_totales = sum(Ha_totales,datos$Ha_sembrada[i],datos_invernadero$Ha_sembradas[i])
}

#Ciclo para encontrar el porcentaje cosechado de cada cultivo
for(i in 1:26){
        temp = sum(datos$Ha_sembrada[i],datos_invernadero$Ha_sembradas[i])
        temp2 = (temp * 100)/Ha_totales
        porcentaje = c(porcentaje,temp2)
}

#Ciclo para encontrar las hectarias no cosechadas
for(i in 1:26){
        temp3 = datos$Ha_sembrada[i]-datos$Ha_cosechada[i]
        Ha_perdidas = c(Ha_perdidas, temp3)
}

#Graficar los datos de hectarias sembradas, hectarias cosechadas y producción en toneladas a cielo abierto
png('Tablas_Cielo_Abierto.png', width=2000,height=3000,res=300)
my.df <- data.frame(lengthclass = datos$Tipo,
                     Ha_sembradas = as.numeric(datos$Ha_sembrada),
                     Ha_cosechada = as.numeric(datos$Ha_cosechada),
                     Produccion_Tonelada = as.numeric(datos$Toneladas))

barplot(t(as.matrix(my.df[, 2:4])), 
        beside = TRUE,
        main = 'Cielo Abierto',
        col = c("red","green",'blue'),
        names.arg = my.df$lengthclass,
        legend.text = TRUE,
        las=2)
dev.off()

#Graficar las hectarias plantadas y la produccion total en toneladas en invernadero
png('Tablas_invernadero.png', width=2000,height=3000,res=300)
my.df2 <- data.frame(lengthclass = datos$Tipo,
                     Ha_sembradas = as.numeric(datos_invernadero$Ha_sembradas),
                     Toneladas = as.numeric(datos_invernadero$Produccion))

barplot(t(as.matrix(my.df2[, 2:3])), 
        beside = TRUE,
        main = 'Invernadero',
        col = c("red","green"),
        names.arg = my.df2$lengthclass,
        legend.text = TRUE,
        las=2)
dev.off()

#Graficar el total de producción por cultivo
png('Tablas_Produccion_Total.png', width=2000,height=3000,res=300)
my.df3 <- data.frame(lengthclass = datos$Tipo,
                     Ha_sembradas = as.numeric(datos_invernadero$Toneladas))

barplot(t(as.matrix(my.df3[, 2:2])), 
        beside = TRUE,
        main = 'Producción Total en Toneladas',
        col = c("green"),
        names.arg = my.df3$lengthclass,
        legend.text = TRUE,
        las=2)
dev.off()

#Graficar en pastel y en barras el porcentaje de hectarias sembradas y perdida de hectarias en cosecha
png('Tablas_Comparativos.png', width=5000,height=3000,res=300)

par(mfrow=c(2,2))

pie3D(porcentaje[2:27],labels=datos$Tipo,explode=0.5,
   main="Cultivo sembrado en porcentaje ")

my.df4 <- data.frame(lengthclass = datos$Tipo,
                     sembrado_porcentaje =porcentaje[2:27])

barplot(t(as.matrix(my.df4[, 2:2])), 
        beside = TRUE,
        main = 'Cultivo sembrado en porcentaje',
        names.arg = my.df4$lengthclass,
        legend.text = TRUE,
        las=2)


pie3D(Ha_perdidas[2:27],labels=datos$Tipo,explode=0.5,
   main="Hectarias perdidas en cielo abierto en cosecha ")

my.df5 <- data.frame(lengthclass = datos$Tipo,
                     sembrado_porcentaje =Ha_perdidas[2:27])

barplot(t(as.matrix(my.df5[, 2:2])), 
        beside = TRUE,
        main = 'Hectarias perdidas en cielo abierto en cosecha ',
        names.arg = my.df5$lengthclass,
        legend.text = TRUE,
        las=2)
dev.off()