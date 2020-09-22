#######------------------------------------#####
#######   Rómulo Enrique Troncoso Pacheco  #####
#######------------------------------------#####

if (!require('tidytext')) {
    install.packages('tidytext')
}
if (!require('dplyr')) {
    install.packages('dplyr')
}
if (!require('gutenbergr')) {
    install.packages('gutenbergr')
}
if (!require('readr')) {
    install.packages('readr')
}

library(gutenbergr)
library(tidytext)
library(dplyr)
library(readr)
#Carga del archivos
libro = gutenberg_download(c(8492))
mas_usadas = read.csv("Palabras_mas_frecuentes.csv",header = TRUE)

#####-----------------------Libro------------------------
#Separación en letras
letrasL1 = libro %>% unnest_tokens(chars, text, "characters")
fl = as.data.frame(table(letras$chars))
flmu = fl[fl$Freq > 1000,]
mm = flmu[order(flmu$Freq, decreasing=TRUE),]
png('../Imagenes/Filtrado_letras_libro.png', width=5000,height=3000,res=300)
barplot(mm$Freq, names.arg = mm$Var1 ,ylab="Frecuencia",xlab="Letras",las=1,col=c("#009999", "#0000FF"),mgp=c(3.3,1,0))
dev.off()
#####-------------------Palabras mas frecuentes-----------
Abecedario = c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z')
frecuencia = ''
letrasMU= ''
#Separar letras de palabras
for (i in 1:length(mas_usadas$Palabra)){
    #temp = strsplit(mas_usadas$Palabra[i]
    letrasMU=c(letrasMU,strsplit(mas_usadas$Palabra[i],""))
}
#Crear frecuencias de letras
for (i in 1:length(Abecedario)){
    for(j in 1:length(letrasMU)){
        if (Abecedario[i] == letrasMU[[j]]){
            num_letra = length(which(letrasMU[[j]] == Abecedario[i]))
            for(h in 1:num_letra){
                frecuencia = c(frecuencia,Abecedario[i])
            }
        }
    }
}


png('../Imagenes/Frecuencia_MU.png', width=2500,height=1800,res=300)
barplot(sort(table(frecuencia),decreasing=TRUE),ylab='Frecuencia',xlab='Letras', las=2,col=c("#009999", "#0000FF"))
dev.off()
print(F_abecedario)

