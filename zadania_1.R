
# 1.2. Napisz kilka wariantów funkcji obliczajacej srednia z próby (zadanej jako wektor v); kolejne warianty moga wykorzystywac np. rózne rodzaje petli ( for, while, repeat ). Porównaj
# dzialanie z funkcja biblioteczna mean.
v = (2:7)

# a) for loop:
avg_for <- function(vec) {
  sum <- 0
  len_v = length(v)
  for (i in v){
    sum <- sum + i
  }
  avg = sum / len_v
  print(avg)
}

avg_for(v)

# b) while loop

avg_mean <- function(vec){
  i <- 1
  sum <- 0
  while (i < length(vec)+1){
    sum <- sum + vec[i]
    i <- i+1
  }
  avg <- sum/length(v)
  print(avg)
}

avg_mean(v)

# c) repeat loop

avg_repeat <- function(vec){
  sum <- 0
  i <- 1
  len_v = length(vec)
  repeat{
    sum <- sum + vec[i]
    if (i == len_v){
      return(sum/len_v)
    }
    i = i+1
  }
}

avg_repeat(v)

mean(v)

# 1.5. Napisz funkcje obliczajaca mediane próby (zadanej jako wektor v). Porównaj dzialanie z
# funkcja biblioteczna median.


v_mix = c(1, 2, 4, 6, 3, 9, 8)

my_mediana <- function(vec) {
  len_vec <- length(vec)
  vec = sort(vec)
  if (len_vec %% 2 == 1){
    return(vec[len_vec/2 +0.5])
  }
  else {
    return((vec[len_vec/2]+vec[(len_vec/2)+1])/2)
  }
}

my_mediana(v_mix)
median(v_mix)

# 1.6. Napisz funkcje obliczajaca rozstep próby (zadanej jako wektor v).

rozstep_proby <- function(vec) {
  len_vec = length(vec)
  vec <- sort(vec)
  min <- vec[1]
  max <- tail(vec, 1)
  return (max-min)
}
rozstep_proby(v_mix)

# 1.7. Napisz funkcje obliczajaca wariancje w próbie (zadanej jako wektor v). Porównaj dzialanie
# z funkcja biblioteczna var. Czy mozna wykorzystac te funkcje do obliczenia wariancji dla calej
# populacji?

wariancja <- function(vec){
  avg <- mean(vec)
  len_vec <- length(vec)
  sum = 0
  for (number in vec) {
    sum = sum + (number - avg)**2
  }
  return (sum/(len_vec-1))
}

wariancja(v)
var(v) # to nie jest wariancja populacji, odebrany jest jeden stopien swobody

# 1.8. Napisz funkcje obliczajaca odchylenie standardowe w próbie (zadanej jako wektor v).
# Porównaj dzialanie z funkcja biblioteczna sd. Czy mozna wykorzystac te funkcje do obliczenia
# odchylenia standardowego dla calej populacji?

odchylenie_standardowe <- function(vec) {
  avg <- mean(vec)
  len_vec <- length(vec)
  sum = 0
  for (number in vec) {
    sum = sum + (number - avg)**2
  }
  return (sqrt(sum/(len_vec-1)))
}

odchylenie_standardowe(v)
sd(v) # nie dla populacji

# 1.10. Napisz funkcje obliczajaca kwartyl dolny (pierwszy) próby (zadanej jako wektor v). Porównaj dzialanie z funkcja biblioteczna fivenum.


kwartyl_dolny <- function(vec) {
  stopifnot(length(vec)>4)
  vec <- sort(vec)
  len_vec <-length(vec)
  if (len_vec %% 2 ==1) {
    q1_vec <- vec[1:floor(len_vec/2)]
    return (median(q1_vec))
  }
  else {
    middle <- median(vec)
    q1 <- median(vec[vec<middle])
    return (q1)
  }
}

kwartyl_dolny(v_mix)
fivenum(v_mix) # wyniki sa rózne dla nieparzystych liczb, rózne metody wyliczenia kwartyla

# 1.11. Napisz funkcje obliczajaca kwartyl górny (trzeci) próby (zadanej jako wektor v). Porównaj dzialanie z funkcja biblioteczna fivenum.

kwartyl_gorny <- function(vec) {
  stopifnot(length(vec)>4)
  vec <- sort(vec)
  len_vec <-length(vec)
  if (len_vec %% 2 ==1) {
    q3_vec <- vec[floor((len_vec/2)+1):len_vec]
    return (median(q3_vec))
  }
  middle = median(vec)
  q3 = median(vec[vec>middle])
  return (q3)
}

kwartyl_gorny(v_mix)
fivenum(v_mix)

# 1.13. Napisz funkcje obliczajaca rozstep miedzykwartylowy próby (zadanej jako wektor v).
# Porównaj dzialanie z funkcja biblioteczna IQR.

roztep_miedzykwartylowy <- function(vec){
  k_g <- kwartyl_gorny(vec)
  k_d <- kwartyl_dolny(vec)
  return (k_g - k_d)
}

roztep_miedzykwartylowy(c(v_mix, 5))
IQR(c(v_mix, 5))

# 1.17. Wyjasnij zastosowania wykresów: slupkowego (barplot), kolowego (pie i pie3D), histogramu (hist), ramkowego/pudelkowego (boxplot) i skrzypcowego (vioplot).
Slupkowy - przedstawienie wartosci zmiennej w zaleznosci od innej zmiennej np. czasu
kolowy - wykres majacy na celu ekspozycje proporcji miedzy kategoriami 
Histogram -rodzaj wykresu slupkowego, który grupuje dane w "kontenery"
ramkowo/pudelkowy - przedstawienie rozkladu cechy statystycznej
Skrzypcowy - rodzaj wykresu pudelkowego, lecz przedstawia wiecej danych, m. in. gestosc prawdopodobienstwa danych dla róznych wartoscii

# 1.18. Opisz elementy wykresu ramkowego/pudelkowego.
Szerokosc - wartosc rozstepu cwiartkowego
lewy bok - pierwszy kwartyl
prawy bok - trzeci kwartyl
konce lini odchadzacych od pudelka moga okreslac wartosc min i max
na srodku moze byc zaznaczona mediana