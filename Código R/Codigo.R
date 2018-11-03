# Importamos la base de datos al software R y utilizamos la función attach()
# para guardar las variables en el dataframe

datos<-read.table("Base de datos.txt", sep="\t", header=TRUE);
attach(datos);

# Imprimimos en pantalla la información ingresada para comprobar de que todo
# esté correctamente

datos;            #Tabla
str(datos);       #Cantidad de variables
summary(datos);   #Resumen de tabla

#### Mostrar Gráficos

### Análisis de la Edad

## Agrupar Edad
#
# pct_edad = Porcentaje de cada intervalo
# etiqu_edad = Títulos de cada intervalo, junto al pocentaje
# limit_edad = Valor de cada intervalo
# inter_edad = Separa la tabla con los intervalos previamente definidos
#
##

pct_edad <- round(EDAD/sum(EDAD)*100);
etiqu_edad <- c("Niños", "Adolescentes", "Jovenes", "Adultos", "Intermedios", "Ancianos");
limit_edad <- c(0, 13, 20, 30, 40, 65, max(EDAD));
inter_edad <- cut(EDAD, limit_edad, etiqu_edad);
etiqu_edad <- paste(etiqu_edad, pct_edad);
etiqu_edad <- paste(etiqu_edad, "%", sep="");

## Histograma Frecuencia de Edad
#
# ylab = Leyenda eje y
# main = Título
# break = Particion de intervalos
# col = color
#
##

hist(EDAD, 
     ylab = "Frecuencia", 
     xlab = "Edad [Años]", 
     main = "Gráfico de Edad en Intervalos", 
     breaks = 20, 
     col = "BLUE");

## Gráfico Circular Rango de Edad
#
# cex = Tamaño de letra
# main = Título
#
##

pie(table(inter_edad), 
    cex = 0.8, 
    main = "Rango de Edad");

## Diagrama de Barra sobre Edad
#
# main = Título
# xlab = Leyenda eje x
# ylab = Leyenda eje y
# col = Color de gráfico
#
##

barplot(table(EDAD), 
        main = "Frecuencia de Edad", 
        xlab = "Edad [Años]", 
        ylab = "Frecuencia ", 
        col="RED");

## Analizando los datos de Edad

table(cut( #Datos Agrupados
  EDAD, 
  limit_edad, 
  c("Niños", "Adolescentes", "Jovenes", "Adultos", "Intermedios", "Ancianos")));

max(EDAD); #Edad persona más vieja
min(EDAD); #Edad persona más jóven

### Análisis de Nacionalidad

## Boxplot
#
# main = Título
# xlab = Leyenda eje x
# ylab = Leyenda eje y
# cex = Tamaño Media
#
##
      
plot(NACIONALIDAD, 
     EDAD, 
     names(NACIONALIDAD),
     main = "Boxplot",
     xlab = "Nacionalidades",
     ylab = "Personas",
     cex = 1);

## Agrupar Nacionalidad
#
# pct_nacionalidad = Porcentaje de cada intervalo con un decimal
# etiqu_nacionalidad = Títulos de cada intervalo, junto al pocentaje
#
##

pct_nacionalidad <- round(table(NACIONALIDAD)/sum((table(NACIONALIDAD)))*100, digits = 1);
etiqu_nacionalidad <- paste(names(table(NACIONALIDAD)), pct_nacionalidad);
etiqu_nacionalidad <- paste(etiqu_nacionalidad, "%", sep="");

## Gráfico Circular de Fallecidos
#
# pie:
# labels = Etiquetas
# col = Color
# main = Título
# radius = Radio del gráfico
#
# legend:
# 1.3 & 1 = Posición eje x e y respectivamente
# fill = Color de etiquetas
# cex = Tamaño de Letra
##

pie(table(NACIONALIDAD),
    labels = NA,
    col = rainbow(length(table(NACIONALIDAD))),
    main = "Fallecidos por Nacionalidad",
    radius = 1.1
    );

legend(1.15,
       1,
       etiqu_nacionalidad,
       fill = rainbow(length(table(NACIONALIDAD))),
       cex = 0.55
       );

## Grafico de Total de Fallecidos
#
# xlab = Etiqueta eje x
# main = Título
# col = Color de Barras
# las = Orientación de Nacionalidades
# horiz = Gráfico Horizontal
# border = Bordes de las Barras
# cex.names = Tamaño de texto Nacionalidades
#
##

barplot(table(NACIONALIDAD),
        xlab = "Personas",
        main = "Fallecidos por Nacionalidad",
        col = "GREEN",
        las = 1,
        horiz = TRUE,
        border = FALSE,
        cex.names = 0.7
        );

## Analizando los datos de Edad

nacional <- table(NACIONALIDAD);
nacional; #Lista de Nacionalidades con las personas fallecidas
max(nacional); #Máxima cantidad de personas fallecidas en un pais
min(nacional); #Mínima catidad de personas fallecidas en un pais



