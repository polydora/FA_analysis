
Nsamp <- 10 #Количество измеренных организмов

FA <- data.frame(FA = rep(NA, 100))

Size <- 10 #Условный размер измеряемой структуры


for(i in 1:100) {
 ML <- Size #Средний размер структуры слева
 MR <- Size #Средний размер структуры справа
SDL <- 1 #SD структуры слева
SDR <- 1 #SD структуры справа

k_ad <- 1
k_mu <- 0.1

FA$Size[i] <- Size
Kad <- rnorm(Nsamp, 0, 1*k_ad) #Коэффициент асимметрии при аддитивной модели

Kmult <- rnorm(Nsamp, 1, 1*k_mu )#Коэффициент асимметрии при мультипликативной модели

L <- rnorm(Nsamp,ML, SDL) # Размер струкутры слева, заданный, как выборка из нормального распределения с параметрами ML и SDL

R_mult <- L * Kmult # размер струкуты справа, как функция от размера структуры слева при мультипликативной модели

R_ad <- L + Kad # размер струкуты справа, как функция от размера структуры слева при аддитивной модели

R_comb1 <- L + (R_mult + R_ad)/2 # размер струкуты справа, как функция от размера структуры слева при комбинировании аддитивной и мультпликативной модели

R_comb2 <- (R_mult + R_ad)/2 # размер струкуты справа, как функция от размера структуры слева при комбинировании аддитивной и мультпликативной модели

FA$FA_mult[i] <- sd((L - R_mult))^2/(Size)^2 #Показатель флуктуирующей асимметрии при мультипликативной модели 
FA$FA_ad[i] <- sd((L - R_ad))^2/(Size)^2 #Показатель флуктуирующей асимметрии при аддитивной модели 

FA$FA_comb1[i] <- sd(L - R_comb1)^2/(Size)^2 #Показатель флуктуирующей асимметрии при комбинации мультипликативной и аддитивной модели 

FA$FA_comb2[i] <- sd(L - R_comb2)^2/(Size)^2 #Показатель флуктуирующей асимметрии при комбинации мультипликативной и аддитивной модели 

Size <- Size + 5 #Увеличиваем размер животного
}



ggplot(FA, aes(x = log(Size), y = log(FA_ad))) + geom_point(color = "blue") + geom_smooth(data = FA, aes(x = log(Size), y = log(FA_ad)), method = "lm") + geom_point(data = FA, aes(x = log(Size), y = log(FA_mult)), color = "red") +geom_smooth(data = FA, aes(x = log(Size), y = log(FA_mult)), method = "lm") + geom_point(data = FA, aes(x = log(Size), y = log(FA_comb1))) +geom_smooth(data = FA, aes(x = log(Size), y = log(FA_comb1)), method = "lm") + geom_point(data = FA, aes(x = log(Size), y = log(FA_comb2)), color = "green") +geom_smooth(data = FA, aes(x = log(Size), y = log(FA_comb2)), method = "lm") 






