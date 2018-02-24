library(reshape2)
library(doBy)


cop <- read.table("Copepoda.csv", sep = ";", head = T)


str(cop)


#Оценки парамтеров распределения значений признаков
MR <- summaryBy(Right ~ Trait, data = cop, FUN = function(x) mean(x, na.rm = T), keep.names = T) #Средний размер структуры справа

ML <- summaryBy(Left ~ Trait, data = cop, FUN = function(x) mean(x, na.rm = T), keep.names = T) #Средний размер структур слева

SDL <- summaryBy(Left ~ Trait, data = cop, FUN = function(x) sd(x, na.rm = T),  keep.names = T) #SD структуры слева

SDR <- summaryBy(Right ~ Trait, data = cop, FUN = function(x) sd(x, na.rm = T),  keep.names = T) #SD структуры справа


# Общая средняя
cop$Mean_LR <- (cop$Left + cop$Right)/2

MLR <- summaryBy(Mean_LR ~ Trait, data = cop, FUN = function(x) mean(x, na.rm = T), keep.names = T) #Средний размер структуры

SDLR <- summaryBy(Mean_LR ~ Trait, data = cop, FUN = function(x) sd(x, na.rm = T),  keep.names = T) #SD структуры
names(SDLR)[2] <- "SDLR"





FA_real <- data.frame(FA = rep(NA, length(unique(cop$Trait))))


for(i in 1:length(unique(cop$Trait))){
  Trait <- unique(cop$Trait)[i]
  x <- cop[cop$Trait == Trait, ]
  FA_real$Trait[i] <- as.character(Trait)
  Size <- mean(c(x$Left, x$Right), na.rm = T)
  FA_real$Size [i] <- Size
  L_R <- x$Left - x$Right
  FA_real$FA[i] <- sd(L_R, na.rm = T)^2/Size^2
}          


# Реальный паттерн связи FA и размера признака
library(ggplot2)
ggplot(FA_real, aes(x = log(Size), y = log(FA))) + geom_point()






#####Строим теоретические выбрки с теми же парамтерами, что и в реальных данных

Nsamp <- length(unique(cop$ID))

NTraits <- length(unique(cop$Trait))


# Первый подход - вычисляем правое значение через значение признака слева

# Это ключевые параметры, от которых зависит то, насколько будет варьировать отклоненение лева от права 

k_ad <- 1
k_mu <- 0.1

##################



Kad <- rnorm(Nsamp*NTraits, 0, 1*k) #Коэффициент асимметрии при аддитивной модели

Kmult <- rnorm(Nsamp*NTraits, 1, 1*k_mu )#Коэффициент асимметрии при мультипликативной модели.
#Важно! срденюю для жтого коэффициента нельзя брать равной нулю. Она должна быть равна 1. Вопрос: что такое дисперсия в этом случае? Она не может быть равна 1. При SD = 1 возможны отрицательные значения множителя.


# Создаем вектор теоретических значений Left для каждого признака для каждой особи


Trait_teor <- data.frame(ID = cop$ID, Sample = cop$Sample, Trait = cop$Trait) 

Left <- NULL 
  
# data.frame(Left_teor = NA)

Left_teor <- data.frame(Left_teor = rep(NA, Nsamp))

for(i in 1:length(unique(cop$Trait))){
  Trait <- unique(Trait_teor$Trait)[i]
  Left_teor$Left_teor[1:Nsamp] <- rnorm(Nsamp, ML[i,2], SDL[i,2])
  Left <- rbind(Left, Left_teor)  
}

head(Left)
# Left <- Left[-1,]


Trait_teor$Left_teor <- as.vector(Left[,1])


# соответствие реального и теоретического значений Left

qplot(cop$Left, Trait_teor$Left_teor) + geom_abline(slope = 1)




Trait_teor$R_mult <- Trait_teor$Left_teor * Kmult # размер струкуты справа, как функция от размера структуры слева при мультипликативной модели

Trait_teor$R_ad <- Trait_teor$Left_teor + Kad # размер струкуты справа, как функция от размера структуры слева при аддитивной модели


Trait_teor$R_comb <- Trait_teor$Left_teor * Kmult + Kad






# Соотношение реального и теоретического значение левого значения признаков
qplot(cop$Left, Trait_teor$Left_teor)

  coef(lm(cop$Left ~ Trait_teor$Left_teor))[2]



# # Соотношение реального и теоретического значение правого значения признаков
# qplot(cop$Right, Trait_teor$R_mult)
# 
#   coef(lm(cop$Right ~ Trait_teor$R_mult))[2]
# 
# 
# qplot(cop$Right, Trait_teor$R_ad)
# 
#   coef(lm(cop$Right ~ Trait_teor$R_ad))[2]
# 
# qplot(cop$Right, Trait_teor$R_comb)
# 
#   coef(lm(cop$Right ~ Trait_teor$R_comb))[2]
# 
  
# Все симулированные правые признаки на одном графике 
qplot(cop$Right, Trait_teor$R_mult, color = "red") + geom_point(aes(x = cop$Right, y = Trait_teor$R_ad), color = "blue", alpha = 0.5) + geom_point(aes(x = cop$Right, y = Trait_teor$R_comb), color = "green", alpha = 0.5 ) 
  


  
  
  
# Оценка флуктуирующей асимметрии в теоретических выборках и в реальной выборке  
  
FA_teor <- data.frame(FA = rep(NA, length(unique(cop$Trait))))


for(i in 1:length(unique(cop$Trait))){
  Trait <- unique(Trait_teor$Trait)[i]
  x <- Trait_teor[Trait_teor$Trait == Trait, ]
  FA_teor$Trait[i] <- as.character(Trait)
  
  # Size <- mean(c(x$Left), na.rm = T)
  Size <- MLR[i,2]
  FA_teor$Size [i] <- Size
  
  L_R_ad <- x$Left_teor - x$R_ad
  FA_teor$FA_ad[i] <- sd(L_R_ad, na.rm = T)^2/Size^2
  
  L_R_mult <- x$Left_teor - x$R_mult
  FA_teor$FA_mult[i] <- sd(L_R_mult, na.rm = T)^2/Size^2

  L_R_comb <- x$Left_teor - x$R_comb
  FA_teor$FA_comb[i] <- sd(L_R_comb, na.rm = T)^2/Size^2
  
}          



Teor_distr <- ggplot(FA_teor, aes(x = log(Size))) + 
  geom_point(aes(y = log(FA_ad)), color = "blue") + geom_smooth(aes(y = log(FA_ad)), color = "blue", method = "lm", se = F) +   
  geom_point(aes(y = log(FA_mult)), color = "red") + geom_smooth(aes(y = log(FA_mult)), color = "red", method = "lm", se = F) +
  geom_point(aes(y = log(FA_comb)), color = "green") + geom_smooth(aes(y = log(FA_comb)), color = "green", method = "lm", se = F) +
  theme_bw() +
  ylab("log(FA)")


Teor_distr + geom_point(data = FA_real, aes(x = log(Size), y = log(FA))) + geom_smooth(data = FA_real, aes(x = log(Size), y = log(FA)), method = "lm", color = "black") 



####Второй подход: Все вычисления строятся на отклонениях от среднего значения #####

Nsamp <- length(unique(cop$ID))

NTraits <- length(unique(cop$Trait))

# Это ключевые параметры, от которых зависит то, насколько будет варьировать отклоненение лева от права 

k_ad <- 1
k_mu <- 0.1

##################


#Важно! срденюю для этого коэффициента нельзя брать равной нулю. Она должна быть равна 1. Вопрос: что такое дисперсия в этом случае? Она не может быть равна 1. При SD = 1 возможны отрицательные значения множителя.


# Создаем вектор теоретических значений Left и Righ для каждого признака для каждой особи


Trait_teor <- data.frame(ID = cop$ID, Sample = cop$Sample, Trait = cop$Trait) 


Trait_teor <- merge(Trait_teor, MLR)

head(Trait_teor)


Kad <- rnorm(Nsamp*NTraits, 0, 1*k) #Коэффициент асимметрии при аддитивной модели

Trait_teor$Left_teor_ad <- Trait_teor$Mean_LR + Kad  

Kad <- rnorm(Nsamp*NTraits, 0, 1*k) #Коэффициент асимметрии при аддитивной модели

Trait_teor$Right_teor_ad <- Trait_teor$Mean_LR + Kad  



Kmult <- rnorm(Nsamp*NTraits, 1, 1*k_mu )#Коэффициент асимметрии при мультипликативной модели.

Trait_teor$Left_teor_mult <- Trait_teor$Mean_LR * Kmult  

Kmult <- rnorm(Nsamp*NTraits, 1, 1*k_mu )#Коэффициент асимметрии при мультипликативной модели.

Trait_teor$Right_teor_mult <- Trait_teor$Mean_LR * Kmult  



# Оценка флуктуирующей асимметрии в теоретических выборках и в реальной выборке  

FA_teor <- data.frame(FA = rep(NA, length(unique(cop$Trait))))


for(i in 1:length(unique(cop$Trait))){
  Trait <- unique(Trait_teor$Trait)[i]
  x <- Trait_teor[Trait_teor$Trait == Trait, ]
  FA_teor$Trait[i] <- as.character(Trait)
  
  Size <- MLR$Mean_LR[i]
  FA_teor$Size [i] <- Size
  
  L_R_ad <- x$Left_teor_ad - x$Right_teor_ad
  FA_teor$FA_ad[i] <- sd(L_R_ad, na.rm = T)^2/Size^2
  
  L_R_mult <- x$Left_teor_mult - x$Right_teor_mult
  FA_teor$FA_mult[i] <- sd(L_R_mult, na.rm = T)^2/Size^2
  
}          


Teor_distr <- ggplot(FA_teor, aes(x = log(Size))) + 
  geom_point(aes(y = log(FA_ad)), color = "blue") + geom_smooth(aes(y = log(FA_ad)), color = "blue", method = "lm", se = F) +   
  geom_point(aes(y = log(FA_mult)), color = "red") + geom_smooth(aes(y = log(FA_mult)), color = "red", method = "lm", se = F)  +
  theme_bw() +
  ylab("log(FA)")


Teor_distr + geom_point(data = FA_real, aes(x = log(Size), y = log(FA))) + geom_smooth(data = FA_real, aes(x = log(Size), y = log(FA)), method = "lm", color = "black") 








####Третий подход




k_ad <- 1
k_mu <- 0.01

##################


#Важно! срденюю для этого коэффициента нельзя брать равной нулю. Она должна быть равна 1. Вопрос: что такое дисперсия в этом случае? Она не может быть равна 1. При SD = 1 возможны отрицательные значения множителя.


# Создаем вектор теоретических значений Left и Righ для каждого признака для каждой особи


Trait_teor2 <- data.frame(ID = cop$ID, Sample = cop$Sample, Trait = cop$Trait) 



LR_teor <- NULL 

i=1

for(i in 1:length(unique(cop$Trait))){
  x <- data.frame(Left_teor_ad = rep(NA, Nsamp), Right_teor_ad = rep(NA, Nsamp), Left_teor_mult = rep(NA, Nsamp), Right_teor_mult = rep(NA, Nsamp))
  Trait <- unique(Trait_teor2$Trait)[i]
  
  Kad <- rnorm(Nsamp, 0, 1*k_ad) #Коэффициент асимметрии при аддитивной модели
  x$Left_teor_ad[1:Nsamp] <- rnorm(Nsamp, MLR[i,2], SDLR[i,2]) + Kad
  
  Kad <- rnorm(Nsamp, 0, 1*k_ad) #Коэффициент асимметрии при аддитивной модели
  x$Right_teor_ad[1:Nsamp] <- rnorm(Nsamp, MLR[i,2], SDLR[i,2]) + Kad
  
  Kmult <- rnorm(Nsamp, 1, 1*k_mu )#Коэффициент асимметрии при мультипликативной модели.
  x$Left_teor_mult[1:Nsamp] <- rnorm(Nsamp, MLR[i,2], SDLR[i,2]) * Kmult

  Kmult <- rnorm(Nsamp, 1, 1*k_mu )#Коэффициент асимметрии при мультипликативной модели.
  x$Right_teor_mult[1:Nsamp] <- rnorm(Nsamp, MLR[i,2], SDLR[i,2]) * Kmult
  
  LR_teor <- rbind(LR_teor, x)  
}

head(LR_teor)
# Left <- Left[-1,]


Trait_teor2 <- cbind (Trait_teor2, LR_teor)


# qplot(cop$Left, Trait_teor$Left_teor_ad)
# 
# qplot(cop$Right, Trait_teor$Right_teor_ad)
# 
# qplot(cop$Left, Trait_teor$Left_teor_mult)
# 
# qplot(cop$Right, Trait_teor$Right_teor_mult)



qplot(Trait_teor2$Left_teor_mult, Trait_teor$Left_teor_mult) + geom_abline(slope=1)
qplot(Trait_teor2$Right_teor_mult, Trait_teor$Right_teor_mult) + geom_abline(slope=1)


qplot(Trait_teor2$Left_teor_ad, Trait_teor$Left_teor_ad) + geom_abline(slope=1)
qplot(Trait_teor2$Right_teor_ad, Trait_teor$Right_teor_ad) + geom_abline(slope=1)



# Оценка флуктуирующей асимметрии в теоретических выборках и в реальной выборке  

FA_teor <- data.frame(FA = rep(NA, length(unique(cop$Trait))))


for(i in 1:length(unique(cop$Trait))){
  Trait <- unique(Trait_teor$Trait)[i]
  x <- Trait_teor[Trait_teor$Trait == Trait, ]
  FA_teor$Trait[i] <- as.character(Trait)
  
  Size <- MLR$Mean_LR[i]
  FA_teor$Size [i] <- Size
  
  L_R_ad <- x$Left_teor_ad - x$Right_teor_ad
  FA_teor$FA_ad[i] <- sd(L_R_ad, na.rm = T)^2/Size^2
  
  L_R_mult <- x$Left_teor_mult - x$Right_teor_mult
  FA_teor$FA_mult[i] <- sd(L_R_mult, na.rm = T)^2/Size^2
  
}          


Teor_distr <- ggplot(FA_teor, aes(x = log(Size))) + 
  geom_point(aes(y = log(FA_ad)), color = "blue") + geom_smooth(aes(y = log(FA_ad)), color = "blue", method = "lm", se = F) +   
  geom_point(aes(y = log(FA_mult)), color = "red") + geom_smooth(aes(y = log(FA_mult)), color = "red", method = "lm", se = F)  +
  theme_bw() +
  ylab("log(FA)")


Teor_distr + geom_point(data = FA_real, aes(x = log(Size), y = log(FA))) + geom_smooth(data = FA_real, aes(x = log(Size), y = log(FA)), method = "lm", color = "black") 







####Четвертый подход:  #####

Nsamp <- length(unique(cop$ID))

NTraits <- length(unique(cop$Trait))

# Это ключевые параметры, от которых зависит то, насколько будет варьировать отклоненение лева от права 

k_ad <- 1
k_mu <- 1

##################


#Важно! срденюю для жтого коэффициента нельзя брать равной нулю. Она должна быть равна 1. Вопрос: что такое дисперсия в этом случае? Она не может быть равна 1. При SD = 1 возможны отрицательные значения множителя.


# Создаем вектор теоретических значений Left и Righ для каждого признака для каждой особи


Trait_teor2 <- data.frame(ID = cop$ID, Sample = cop$Sample, Trait = cop$Trait) 


Trait_teor2 <- merge(Trait_teor2, MLR)

Trait_teor2 <- merge(Trait_teor2, SDLR, by = "Trait")


head(Trait_teor2)


# Kad <- rnorm(Nsamp*NTraits, 0, 1*k) #Коэффициент асимметрии при аддитивной модели


Trait_teor2$Left_teor_ad <- Trait_teor2$Mean_LR + rnorm(Nsamp*NTraits, 0, Trait_teor2$SDLR ) * k_ad  

Trait_teor2$Right_teor_ad <- Trait_teor2$Mean_LR + rnorm(Nsamp*NTraits, 0, Trait_teor2$SDLR ) * k_ad   



# Kmult <- rnorm(Nsamp*NTraits, 0, 1*k_mu )#Коэффициент асимметрии при мультипликативной модели.

# Trait_teor2$Mean_LR +

Trait_teor2$Left_teor_mult <-  abs((rnorm(Nsamp*NTraits, 0, Trait_teor2$SDLR) * k_mu)) *  Trait_teor2$Mean_LR

# Trait_teor2$Mean_LR +

Trait_teor2$Right_teor_mult <-  abs(rnorm(Nsamp*NTraits, 0, Trait_teor2$SDLR ) * k_mu) *  Trait_teor2$Mean_LR



Trait_teor2$Left_teor_comb <- Trait_teor2$Mean_LR + abs(rnorm(Nsamp*NTraits, 0, Trait_teor2$SDLR ) * k_mu)* Trait_teor2$Mean_LR + rnorm(Nsamp*NTraits, 0, Trait_teor2$SDLR ) * k_ad    


Trait_teor2$Right_teor_comb <- Trait_teor2$Mean_LR + abs(rnorm(Nsamp*NTraits, 0, Trait_teor2$SDLR )* k_mu) * Trait_teor2$Mean_LR + rnorm(Nsamp*NTraits, 0, Trait_teor2$SDLR ) * k_ad    



head(Trait_teor2)
# Оценка флуктуирующей асимметрии в теоретических выборках и в реальной выборке  

FA_teor2 <- data.frame(FA = rep(NA, length(unique(cop$Trait))))

i=1

for(i in 1:length(unique(cop$Trait))){
  Trait <- unique(Trait_teor2$Trait)[i]
  x <- Trait_teor2[Trait_teor2$Trait == Trait, ]
  FA_teor2$Trait[i] <- as.character(Trait)
  
  Size <- MLR$Mean_LR[i]
  FA_teor2$Size [i] <- Size
  
  L_R_ad <- x$Left_teor_ad - x$Right_teor_ad
  FA_teor2$FA_ad[i] <- sd(L_R_ad, na.rm = T)^2/Size^2
  
  L_R_mult <- x$Left_teor_mult - x$Right_teor_mult
  FA_teor2$FA_mult[i] <- sd(L_R_mult, na.rm = T)^2/Size^2
  
  L_R_comb <- x$Left_teor_comb - x$Right_teor_comb
  FA_teor2$FA_comb[i] <- sd(L_R_comb, na.rm = T)^2/Size^2
  
}          


Teor_distr <- ggplot(FA_teor2, aes(x = log(Size))) + 
  geom_point(aes(y = log(FA_ad)), color = "blue") + geom_smooth(aes(y = log(FA_ad)), color = "blue", method = "lm", se = F) +   
  geom_point(aes(y = log(FA_mult)), color = "red") + geom_smooth(aes(y = log(FA_mult)), color = "red", method = "lm", se = F)  +
   geom_point(aes(y = log(FA_comb)), color = "green") + geom_smooth(aes(y = log(FA_comb)), color = "green", method = "lm", se = F)  +
  theme_bw() +
  ylab("log(FA)")



Teor_distr + geom_point(data = FA_real, aes(x = log(Size), y = log(FA))) + geom_smooth(data = FA_real, aes(x = log(Size), y = log(FA)), method = "lm", color = "black") 






#Подход 5##
# В этом подходе мы оцениваем парамтеры распределеиня k_ad и k_mult

k_add_param <- summaryBy((Right - Mean_LR)  ~ Trait, data = cop, FUN = function(x) c(mean(x, na.rm = T), sd(x, na.rm = T)))


names(k_add_param) <- c("Trait", "K_ad_Right", "SD_K_ad")

# k_add_param - датафрейм со средними и Sd для отклонений правых признаков от среднего значения.  



k_mult_param <- summaryBy((Right / Mean_LR)  ~ Trait, data = cop, FUN = function(x) c(mean(x, na.rm = T), sd(x, na.rm = T)))


names(k_mult_param) <- c("Trait", "K_mult_Right", "SD_K_mult")


# k_mult_param - датафрейм со средними и Sd для отклонений правых признаков от среднего значения.  


Nsamp <- length(unique(cop$ID))

NTraits <- length(unique(cop$Trait))




Trait_teor3 <- data.frame(ID = cop$ID, Sample = cop$Sample, Trait = cop$Trait) 


Trait_teor3 <- merge(Trait_teor3, MLR)

Trait_teor3 <- merge(Trait_teor3, SDLR, by = "Trait")


Trait_teor3 <- merge(Trait_teor3, k_add_param, by = "Trait")

Trait_teor3 <- merge(Trait_teor3, k_mult_param, by = "Trait")


head(Trait_teor3)



# Аддитивная модель
Trait_teor3$Left_teor_ad <- rnorm(nrow(Trait_teor3), Trait_teor3$Mean_LR,Trait_teor3$SDLR)  - rnorm(nrow(Trait_teor3), Trait_teor3$K_ad_Right, Trait_teor3$SD_K_ad)


Trait_teor3$Right_teor_ad <- rnorm(nrow(Trait_teor3), Trait_teor3$Mean_LR,Trait_teor3$SDLR) + rnorm(nrow(Trait_teor3), Trait_teor3$K_ad_Right, Trait_teor3$SD_K_ad)



# Мультипликативная модель


Trait_teor3$Left_teor_mult <-  rnorm(nrow(Trait_teor3), Trait_teor3$Mean_LR,Trait_teor3$SDLR) * rnorm(nrow(Trait_teor3), Trait_teor3$K_mult_Right, Trait_teor3$SD_K_mult) 


Trait_teor3$Right_teor_mult <-   rnorm(nrow(Trait_teor3), Trait_teor3$Mean_LR,Trait_teor3$SDLR) * rnorm(nrow(Trait_teor3), Trait_teor3$K_mult_Right, Trait_teor3$SD_K_mult) 





# Оценка флуктуирующей асимметрии в теоретических выборках и в реальной выборке  

FA_teor3 <- data.frame(FA = rep(NA, length(unique(cop$Trait))))

i=1

for(i in 1:length(unique(cop$Trait))){
  Trait <- unique(Trait_teor3$Trait)[i]
  x <- Trait_teor3[Trait_teor3$Trait == Trait, ]
  FA_teor3$Trait[i] <- as.character(Trait)
  
  Size <- MLR$Mean_LR[i]
  FA_teor3$Size [i] <- Size
  
  L_R_ad <- x$Left_teor_ad - x$Right_teor_ad
  FA_teor3$FA_ad[i] <- sd(L_R_ad, na.rm = T)^2/Size^2
  
  L_R_mult <- x$Left_teor_mult - x$Right_teor_mult
  FA_teor3$FA_mult[i] <- sd(L_R_mult, na.rm = T)^2/Size^2
  # 
  # L_R_comb <- x$Left_teor_comb - x$Right_teor_comb
  # FA_teor2$FA_comb[i] <- sd(L_R_comb, na.rm = T)^2/Size^2
  # 
}          


Teor_distr <- ggplot(FA_teor3, aes(x = log(Size))) + 
  geom_point(aes(y = log(FA_ad)), color = "blue") + geom_smooth(aes(y = log(FA_ad)), color = "blue", method = "lm", se = F) +   
  geom_point(aes(y = log(FA_mult)), color = "red") + geom_smooth(aes(y = log(FA_mult)), color = "red", method = "lm", se = F)  +
  theme_bw() +
  ylab("log(FA)")



Teor_distr + geom_point(data = FA_real, aes(x = log(Size), y = log(FA))) + geom_smooth(data = FA_real, aes(x = log(Size), y = log(FA)), method = "lm", color = "black") 






Means_for_traits <- summaryBy(Mean_LR + Left_teor_ad + Left_teor_mult ~ Trait, data = Trait_teor3 )


