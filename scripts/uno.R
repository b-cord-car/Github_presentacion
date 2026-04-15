#Peso de ratons
#10 mediciones antes y después
#Datos pareados
#Determinar si hay un impacto
#Ho: mu1 - mu2 = 0
#Ha: mu1 - mu2 ! = 0
#alfa = 0.05
#Prueba de t pareada
antes <- c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
despues <- c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
diferencia <- antes-despues
#Estadístico de prueba
mediadif <- mean(diferencia)
sddif <- sd(diferencia)
n <- length(diferencia)
tcalc <- mediadif/(sddif/sqrt(n))
#Valor critico
tcrit <- qt(0.025,9)
#p-value
pval <- pt(tcalc,9,lower.tail = TRUE)*2
#Se rechaza Ho
t.test(antes,despues, paired = TRUE)
#Hay diferencias en el peso de ratones antes y despues del tratamiento


#Modificacion