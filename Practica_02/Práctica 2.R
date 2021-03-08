#conjunto de datos
datos ( ChickWeight)
cabeza (ChickWeight)

head (ChickWeight, 10L)
tail (ChickWeight, 10L)

#Estadística descriptiva de una variable
chick0_descend <- chick0[order(-chick0$weight), ]
head(chick0_descend, 578)

# # Gráficos de densidad
# cargar paquetes
biblioteca ( celosía )
# crear un diseño de parcelas de densidad más simples por atributo
{
  plot(ChickWeight$Time,ChickWeight$weight,xlab = 'time',ylab = 'weight')
}

{
  plot(ChickWeight$weight~ChickWeight$Diet,xlab = 'diet',ylab = 'weight')
}


# # Histogramas
# cargar los datos
datos ( ChickWeight )

# crea histogramas para cada atributo
{
  hist(ChickWeight$weight[ChickWeight$Diet=='1'],breaks = 30,main = 'Histogram: Weight for Diet 1')
  hist(ChickWeight$weight[ChickWeight$Diet=='2'],breaks = 30,main = 'Histogram: Weight for Diet 2')
  hist(ChickWeight$weight[ChickWeight$Diet=='3'],breaks = 30,main = 'Histogram: Weight for Diet 3')
  hist(ChickWeight$weight[ChickWeight$Diet=='4'],breaks = 30,main = 'Histogram: Weight for Diet 4')
}

{
  plot(ChickWeight$weight~ChickWeight$Chick,xlab = 'ordered label by weight within the same diet',ylab = 'weight')
  
}

#Caja bigotes
#cargar los datos
datos ( ChickWeight)

#Crear una caja bigotes
{
  m0 <- lm(weight ~ .,data = ChickWeight)
  summary(m0)
  
  m1 <- lm(weight ~ Time + Diet,data = ChickWeight)
  summary(m1)
  
  ChickWeight2 <- ChickWeight
  ChickWeight2$Time2 <- ChickWeight$Time*ChickWeight$Time
  
  m2 <- lm(weight ~ Time + Diet + Time2, data = ChickWeight2)
  summary(m2)
  
  m3 <- lm(weight ~ Time + Diet + Time2*Diet, data = ChickWeight2)
  summary(m3)
  
  ChickWeight4 <- ChickWeight2
  ChickWeight4$Dt[ChickWeight4$Diet == '1' | ChickWeight4$Diet == '2'] <- 'A'
  ChickWeight4$Dt[ChickWeight4$Diet == '3' | ChickWeight4$Diet == '4'] <- 'B'
  
  boxplot(ChickWeight4$weight~ChickWeight4$Dt)
  
}

#Estadística descriptiva de 2 variables
# # Gráfico de correlación

{
  qqnorm(m0$residuals) 
  qqline(m0$residuals)
}



