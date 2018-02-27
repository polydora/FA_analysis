library(reshape2)
library(doBy)
library(ggplot2)

cop <- read.table("Copepoda.csv", sep = ";", head = T)

head(cop)
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


FA <- function(trait, left, right){
  xx <- data.frame(Trait = trait, Left = left, Right = right)
  FA <- data.frame(Trait = rep(NA, length(unique(trait))), Size = NA, FA = NA)
  for(i in 1:length(unique(xx$Trait))){
    Trait <- unique(xx$Trait)[i]
    x <- xx[xx$Trait == Trait, ]
    FA$Trait[i] <- as.character(Trait)
    Size <- mean(c(x$Left, x$Right), na.rm = T)
    FA$Size[i] <- Size
    L_R <- x$Left - x$Right
    FA$FA[i] <- sd(L_R, na.rm = T)^2/Size^2
  }          
  FA
}

  

FA_real <- FA(cop$Trait, cop$Left, cop$Right)



# Реальный паттерн связи FA и размера признака
library(ggplot2)
ggplot(FA_real, aes(x = log(Size), y = log(FA))) + geom_point()







####Первый подход: парамтры отклонения от срденго стнадартны для всех признаков, то есть Kad или Kmult одинаковы для всех признаков (взяты из одного и того же соответствующего распределения) #####

k_add_mean <- mean((cop$Right - cop$Mean_LR), na.rm = TRUE) 
k_add_sd <- sd((cop$Right - cop$Mean_LR), na.rm = TRUE) 

k_mult_mean <- mean((cop$Right / cop$Mean_LR), na.rm = TRUE) 
k_mult_sd <- sd((cop$Right / cop$Mean_LR), na.rm = TRUE) 




Nsamp <- length(unique(cop$ID))

NTraits <- length(unique(cop$Trait))

# Это множители, которые регулируют степень варьирования теоретических распределиний 

k_ad <- 1
k_mu <- 1

##################


#Важно! срденюю для этого коэффициента нельзя брать равной нулю. Она должна быть равна 1. Вопрос: что такое дисперсия в этом случае? Она не может быть равна 1. При SD = 1 возможны отрицательные значения множителя.


# Создаем вектор теоретических значений Left и Righ для каждого признака для каждой особи


Trait_teor <- data.frame(ID = cop$ID, Sample = cop$Sample, Trait = cop$Trait) 


Trait_teor <- merge(Trait_teor, MLR)

head(Trait_teor)

#Аддитивная модель


Kad <- rnorm(Nsamp*NTraits, k_add_mean, k_add_sd*k_ad) #Коэффициент асимметрии при аддитивной модели

Trait_teor$Left_teor_ad <- Trait_teor$Mean_LR + Kad  

Kad <- rnorm(Nsamp*NTraits, k_add_mean, k_add_sd*k_ad) #Коэффициент асимметрии при аддитивной модели

Trait_teor$Right_teor_ad <- Trait_teor$Mean_LR + Kad  

#Мультипликаттивная модель

Kmult <- rnorm(Nsamp*NTraits, k_mult_mean, k_mult_sd*k_mu )#Коэффициент асимметрии при мультипликативной модели.

Trait_teor$Left_teor_mult <- Trait_teor$Mean_LR * Kmult  

Kmult <- rnorm(Nsamp*NTraits, k_mult_mean, k_mult_sd*k_mu )#Коэффициент асимметрии при мультипликативной модели.

Trait_teor$Right_teor_mult <- Trait_teor$Mean_LR * Kmult  


ggplot(Trait_teor, aes(x =(Right_teor_ad - Mean_LR)  )) + geom_histogram() + facet_wrap(~Trait)


#Комбинативная модель

Kad <- rnorm(Nsamp*NTraits, k_add_mean, k_add_sd*k_ad) #Коэффициент асимметрии при аддитивной модели

Kmult <- rnorm(Nsamp*NTraits, k_mult_mean, k_mult_sd*k_mu )#Коэффициент асимметрии при мультипликативной модели.

Trait_teor$Left_teor_comb <- Trait_teor$Mean_LR * (Kmult/2) + Kad/2



Kad <- rnorm(Nsamp*NTraits, k_add_mean, k_add_sd*k_ad) #Коэффициент асимметрии при аддитивной модели

Kmult <- rnorm(Nsamp*NTraits, k_mult_mean, k_mult_sd*k_mu )#Коэффициент асимметрии при мультипликативной модели.

Trait_teor$Right_teor_comb <- Trait_teor$Mean_LR * (Kmult/2) + Kad/2



# Оценка флуктуирующей асимметрии в теоретических выборках и в реальной выборке  

FA_teor_ad <- FA(Trait_teor$Trait, Trait_teor$Left_teor_ad, Trait_teor$Right_teor_ad)

FA_teor_mult <- FA(Trait_teor$Trait, Trait_teor$Left_teor_mult, Trait_teor$Right_teor_mult)

FA_teor_comb <- FA(Trait_teor$Trait, Trait_teor$Left_teor_comb, Trait_teor$Right_teor_comb)




Teor_distr <- ggplot(FA_teor_ad, aes(x = log(Size))) + 
  geom_point(aes(y = log(FA)), color = "blue") + geom_smooth(aes(y = log(FA)), color = "blue", method = "lm", se = F) +   
  geom_point(data = FA_teor_mult, aes(y = log(FA)), color = "red") + geom_smooth(data = FA_teor_mult, aes(y = log(FA)), color = "red", method = "lm", se = F)  +
  geom_point(data = FA_teor_comb, aes(y = log(FA)), color = "green") + geom_smooth(data = FA_teor_comb, aes(y = log(FA)), color = "green", method = "lm", se = F)  +
  theme_bw() +
  ylab("log(FA)")


Teor_distr + geom_point(data = FA_real, aes(x = log(Size), y = log(FA))) + geom_smooth(data = FA_real, aes(x = log(Size), y = log(FA)), method = "lm", color = "black") 



#############################################



#Второй подход####
# В этом подходе мы оцениваем парамтеры распределеиня k_ad и k_mult для каждого признака независимо, то есть парамтры отклоненеия левого и правого признака от среденго будут разные для каждого признака.

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


Kad <- rep(NA, nrow(Trait_teor3))
for(i in 1:nrow(Trait_teor3)) Kad[i] <- rnorm(1, Trait_teor3$K_ad_Right[i], Trait_teor3$SD_K_ad[i]) # Вектор коэффициентов для аддититвной модели

Trait_teor3$Left_teor_ad <- rnorm(nrow(Trait_teor3), Trait_teor3$Mean_LR,Trait_teor3$SDLR)  + Kad


Kad <- rep(NA, nrow(Trait_teor3))
for(i in 1:nrow(Trait_teor3)) Kad[i] <- rnorm(1, Trait_teor3$K_ad_Right[i], Trait_teor3$SD_K_ad[i]) # Вектор коэффициентов для аддититвной модели

Trait_teor3$Right_teor_ad <- rnorm(nrow(Trait_teor3), Trait_teor3$Mean_LR,Trait_teor3$SDLR) + Kad


ggplot(Trait_teor3, aes(x =(Right_teor_ad - Mean_LR)  )) + geom_histogram() + facet_wrap(~Trait)

# Мультипликативная модель

Kmult <- rep(NA, nrow(Trait_teor3))
for(i in 1:nrow(Trait_teor3)) Kmult[i] <- rnorm(1, Trait_teor3$K_mult_Right[i], Trait_teor3$SD_K_mult[i]) # Вектор коэффициентов для мультипликативой модели


Trait_teor3$Left_teor_mult <-  rnorm(nrow(Trait_teor3), Trait_teor3$Mean_LR,Trait_teor3$SDLR) * Kmult


Kmult <- rep(NA, nrow(Trait_teor3))
for(i in 1:nrow(Trait_teor3)) Kmult[i] <- rnorm(1, Trait_teor3$K_mult_Right[i], Trait_teor3$SD_K_mult[i]) # Вектор коэффициентов для мультипликативой модели

Trait_teor3$Right_teor_mult <-   rnorm(nrow(Trait_teor3), Trait_teor3$Mean_LR,Trait_teor3$SDLR) * Kmult




#Комбинативная модель
Kad <- rep(NA, nrow(Trait_teor3))
for(i in 1:nrow(Trait_teor3)) Kad[i] <- rnorm(1, Trait_teor3$K_ad_Right[i], Trait_teor3$SD_K_ad[i]) # Вектор коэффициентов для аддититвной модели

Kmult <- rep(NA, nrow(Trait_teor3))
for(i in 1:nrow(Trait_teor3)) Kmult[i] <- rnorm(1, Trait_teor3$K_mult_Right[i], Trait_teor3$SD_K_mult[i]) # Вектор коэффициентов для мультипликативой модели


Trait_teor3$Left_teor_comb <-  rnorm(nrow(Trait_teor3), Trait_teor3$Mean_LR,Trait_teor3$SDLR) * Kmult + Kad






Kad <- rep(NA, nrow(Trait_teor3))
for(i in 1:nrow(Trait_teor3)) Kad[i] <- rnorm(1, Trait_teor3$K_ad_Right[i], Trait_teor3$SD_K_ad[i]) # Вектор коэффициентов для аддититвной модели

Kmult <- rep(NA, nrow(Trait_teor3))
for(i in 1:nrow(Trait_teor3)) Kmult[i] <- rnorm(1, Trait_teor3$K_mult_Right[i], Trait_teor3$SD_K_mult[i]) # Вектор коэффициентов для мультипликативой модели


Trait_teor3$Right_teor_comb <-  rnorm(nrow(Trait_teor3), Trait_teor3$Mean_LR, Trait_teor3$SDLR) * Kmult + Kad






# Оценка флуктуирующей асимметрии в теоретических выборках и в реальной выборке  

FA_teor3_ad <- FA(Trait_teor3$Trait, Trait_teor3$Left_teor_ad,  Trait_teor3$Right_teor_ad )
 

  
FA_teor3_mult <- FA(Trait_teor3$Trait, Trait_teor3$Left_teor_mult,  Trait_teor3$Right_teor_mult )


FA_teor3_comb <- FA(Trait_teor3$Trait, Trait_teor3$Left_teor_comb, Trait_teor3$Right_teor_comb)




Teor_distr <- ggplot(FA_teor3_ad, aes(x = log(Size))) + 
  geom_point(aes(y = log(FA)), color = "blue") + 
  geom_smooth(aes(y = log(FA)), color = "blue", method = "lm", se = F) +   
  geom_point(data = FA_teor3_mult, aes(y = log(FA)), color = "red") + 
  geom_smooth(data = FA_teor3_mult, aes(y = log(FA)), color = "red", method = "lm", se = F)  +
  geom_smooth(data = FA_teor3_comb, aes(y = log(FA)), color = "green", method = "lm", se = F)  +
  theme_bw() +
  ylab("log(FA)")



Teor_distr + geom_point(data = FA_real, aes(x = log(Size), y = log(FA))) + geom_smooth(data = FA_real, aes(x = log(Size), y = log(FA)), method = "lm", color = "black") 







