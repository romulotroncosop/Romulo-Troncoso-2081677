if (!require('tidytext')) {
    install.packages('tidytext')
}
if (!require('dplyr')) {
    install.packages('dplyr')
}
if (!require('gutenbergr')) {
    install.packages('gutenbergr')
}

library(gutenbergr)
library(tidytext)
library(dplyr)

#Descargar libro
libro = gutenberg_download(c(8492))
#Dividir las letras del libro
letras = libro %>% unnest_tokens(chars, text, "characters")
#Dividir las palabras del text
palabras = libro %>% unnest_tokens(word, text, "words")
#Graficar las palabras de froma decresiente
png('../Imagenes/Gráfico_Decreciente.png', width=5000,height=5000,res=300)
barplot(sort(table(palabras$word), decreasing=TRUE), main='Grafica ordenada de palabras en forma descendiente',las=2)
#barplot(sort(table(palabras$word), decreasing=TRUE), log="y")

#Poner un marco a los datos
fl = as.data.frame(table(letras$chars))

#Filtrar los datos
flmu = fl[fl$Freq > 100,]

#Graficar
png('../Imagenes/Gráfica_de_las_letras.png', width=5000,height=3000,res=300)
barplot(flmu$Freq, names.arg = flmu$Var1, main='Grafica de las letras',las=2,col=c("#009999", "#0000FF"))


#Gráficar de forma decreciende las letras
mm = flmu[order(flmu$Freq, decreasing=TRUE),]
png('../Imagenes/Gráfica_en_forma_decreciente_y_filtrado_de_las_letras.png', width=5000,height=3000,res=300)
barplot(mm$Freq, names.arg = mm$Var1 ,main='Grafica en forma decreciente y filtrado de las letras',las=2,col=c("#009999", "#0000FF"))



#Pornerle etiquetas a la tabla
names(mm) = c('Letra', 'Frecuencia')




#Palabras
fp = as.data.frame(table(palabras$word))
names(fp) = c('Palabra', 'Frecuencia')


#Filtrar
fpf = fp[fp$Frecuencia > 100,]
png('../Imagenes/Gráfica_en_forma_filtrada_de_las_palabras_con_frecuencia_mayor_a_100.png', width=5000,height=3000,res=300)
barplot(fpf$Frecuencia, names.arg = fpf$Palabra ,main='Grafica en forma filtrada de las palabras con frecuencia mayor a 100',las=2,col=c("#009999", "#0000FF"))


fpi = fpf[fpf$Frecuencia < 1000,]
png('../Imagenes/Gráfica_en_forma_filtrada_de_las_palabras_con_frecuencia_mayor_a_1000.png', width=5000,height=3000,res=300)
barplot(fpi$Frecuencia, names.arg = fpi$Palabra ,main='Grafica en forma filtrada de las palabras con frecuencia mayor a 1000',las=2,col=c("#009999", "#0000FF"))

fpo = fpi[order(fpi$Frecuencia, decreasing=TRUE),]

png('../Imagenes/Gráfica_en_forma_filtrada_de_las_palabras_con_frecuencia_mayor_a_1000_de_forma_decreciente.png', width=5000,height=3000,res=300)
barplot(fpo$Frecuencia, names.arg = fpo$Palabra,main='Grafica en forma filtrada de las palabras con frecuencia mayor a 1000 de forma decreciente',las=2,col=c("#009999", "#0000FF"))
#barplot(fpo$Frecuencia, names.arg = fpo$Palabra)
