# VERI_MADENCILIGI2
VERI_MADENCILIGI_ODEVI_2-ORNEK_SCRIPT
# oluşturulan veri setini gösterme
head(data)

# faktörel değişkene dönüştürme
x1_f_factor <- as.factor(x1_f)

# sayısal değişkene dönüştürme
x2_f_numeric <- as.numeric(x2_f)

# Değişken türlerini kontrol etme
str(x1_f_factor)
str(x2_f_numeric)

#eksik veri tespiti ve doldurma

# Eksik veri tespiti
is_na_x1 <- is.na(x1_ev)
is_na_x2 <- is.na(x2_ev)


# Eksik verileri ortalama ile doldurma
mean_x1_ev <- mean(x1_ev, na.rm = TRUE)
x1_filled <- ifelse(is.na(x1_ev), mean_x1_ev, x1_ev)
#--- 
#na.locf() fonksiyonu 
install.packages("zoo")
library(zoo)
# örnek vektör oluşturma
vec <- c(1, NA, NA, 4, NA, 6, NA, 8, NA)

# Eksik değerleri LOCF yöntemiyle doldurma
vec_filled <- na.locf(vec)
print(vec_filled)

# Eksik verileri önceki değerle doldurma
x2_filled <- na.locf(x2_ev)
# Eksik verileri medyan ile doldurma
median_y_ev <- median(y_ev, na.rm = TRUE)
y_filled <- ifelse(is.na(y_ev), median_y_ev, y_ev)


# Kutu grafiği oluşturma
boxplot(data)
sort(data)

# Z-puanı hesaplama
z_scores <- scale(data)

# Aykırı değerleri belirleme
outliers <- abs(z_scores) > 3


# Alt ve üst çeyreklikleri hesaplama
Q1 <- quantile(data, 0.25)
Q3 <- quantile(data, 0.75)
# Alt ve üst sınırı hesaplama
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Aykırı değerleri belirleme
outliers_iqr <- data[data < lower_bound | data > upper_bound]

# Sonuçları görüntüleme
outliers_z 
outliers_iqr

# Histogram oluşturma
hist(data)
# Kutu grafiği oluşturma
boxplot(data)
# Q-Q plot oluşturma
qqnorm(data)
qqline(data)

# Kantillerden yararlanma, veri setinin özet istatitklerine bakma
summary(data)

# Korelasyon katsayısını hesaplama
correlation <- cor(x, y)

# Korelasyon katsayısını ekrana yazdırma
print(correlation)
#multicollinearity
# çoklu doğrusal regresyon modelini oluşturma
model <- lm(y ~ x1 + x2 + x3)

# Model özetini alma
summary(model)

# Bağımsız değişkenler arasındaki korelasyonu hesaplama
correlation_matrix <- cor(data.frame(x1, x2, x3))
print(correlation_matrix)

# corrplot paketini yükleme
library(corrplot)

# Korelasyon matrisini görselleştirme
corrplot(correlation_matrix, method = "color")

# Bağımsız değişkenlerin grafiğini çizme
par(mfrow=c(1,3)) 


# Veriyi standartlaştırma
x1_standardized <- scale(x1)

# Standartlaştırılmış veriyi kontrol etme
summary(x1_standardized)


#kategorik keşif
# caTools paketini yükleme
install.packages("caTools")
library(caTools)


# Verinin boyutlarını kontrol etme
dim(train_data)
dim(test_data)
