# Import Importan
install.packages(c("psych", "FactoMineR", "factoextra", "ggplot2", "gridExtra"))

library(psych)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(gridExtra)


# Autobots
# Import data
data <- read.csv("C:/Users/naufa/Downloads/expect_japan_life.csv")

# Statistika Deskriptif
summary(data) 
describe(data)

# Ringkasan Statistik Deskriptif
summary(data)  
describe(data) 

# Visualisasi Statistik Deskriptif
# Histogram untuk distribusi setiap variabel numerik
par(mfrow=c(2,3)) 
for (col in names(data)) {
  if (is.numeric(data[[col]])) {
    hist(data[[col]], main=paste("Histogram:", col), xlab=col, col="skyblue", border="black")
  }
}
par(mfrow=c(1,1))


# Boxplot untuk melihat outlier
par(mfrow=c(2,3)) # Atur layout
for (col in names(data)) {
  if (is.numeric(data[[col]])) {
    boxplot(data[[col]], main=paste("Boxplot:", col), col="lightgreen")
  }
}
par(mfrow=c(1,1)) 

# Density Plot untuk distribusi data
ggplot(stack(data), aes(x=values, fill=ind)) +
  geom_density(alpha=0.5) +
  labs(title="Density Plot untuk Semua Variabel") +
  theme_minimal()

# Ambil hanya variabel numerik untuk korelasi
num_data <- data[sapply(data, is.numeric)]  

# Korelasi antar variabel (Heatmap)
cor_matrix <- cor(num_data, use="complete.obs")  
heatmap(cor_matrix, main="Heatmap Korelasi", col=topo.colors(10), scale="column")


# Hapus kolom kategori (Prefecture)
data_num <- data[ , -1]

# Cek apakah ada nilai yang hilang
sum(is.na(data_num))

# Standarisasi data
data_scaled <- scale(data_num)                    

# Lakukan PCA
pca_result <- PCA(data_scaled, scale.unit = TRUE, graph = FALSE)

print(pca_result$eig)


# Tampilkan proporsi varians
print(pca_result$eig)

# Scree plot untuk melihat komponen utama yang signifikan
fviz_eig(pca_result, addlabels = TRUE, barfill = "skyblue", barcolor = "darkblue", linecolor = "red")

# Biplot PCA untuk melihat hubungan antar variabel dan data
fviz_pca_biplot(pca_result, geom.ind = "point", addEllipses = TRUE)

# Visualisasi kontribusi variabel terhadap PC1 dan PC2
fviz_pca_var(pca_result, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

# Manual (PCA dan FA)
# Impor dan Preprocessing Data
data <- read.csv("C:/Users/naufa/Downloads/expect_japan_life.csv")

# Hapus kolom non-numerik (Prefecture)
data_num <- data[, sapply(data, is.numeric)]

# Cek missing values
sum(is.na(data_num))

# Standarisasi data
scale_data <- scale(data_num)

# Cek Kelayakan Data untuk PCA & FA
# KMO Test(KMO Test (Kaiser-Meyer-Olkin)
library(psych)
r <- cor(scale_data) 
KMO(r)  

# Bartlets Test
library(psych)
cortest.bartlett(scale_data)

# Principal Component Anlysis (PCA)
# PCA secara manual
r <- cov(scale_data)  
pc <- eigen(r)  

# Proporsi varians
sumvar <- sum(pc$values)
propvar <- (pc$values / sumvar) * 100
cumvar <- cumsum(propvar)

# Hasil PCA
pc$vectors  # Eigenvector (komponen utama)
scores <- as.matrix(scale_data) %*% pc$vectors  # Nilai PCA
head(scores)  

# Menggunakan prcomp()
PCA.mod <- prcomp(scale_data)
summary(PCA.mod)

# Visualisasi PCA
library(factoextra)
fviz_eig(PCA.mod)  # Scree plot
fviz_pca_biplot(PCA.mod)  # Biplot PCA

# Faktor Anlysis (FA)
fa_result <- fa(r = scale_data, nfactors = 3, rotate = "varimax")
print(fa_result$loadings)

# Diagram Faktor
fa.diagram(fa_result$loadings)

# Summary Rumusan Masalah
# No 1
# Kontribusi variabel dalam PCA (untuk menentukan faktor utama)
fviz_pca_var(pca_result, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

# Menampilkan faktor loading FA (untuk menentukan faktor utama)
print(fa_result$loadings)


# No 2
# PCA
# Biplot PCA untuk melihat hubungan antar variabel
fviz_pca_biplot(pca_result, geom.ind = "point", addEllipses = TRUE)
# FA
# Diagram faktor FA untuk melihat pola antar variabel
fa.diagram(fa_result$loadings)


# NO 3
# Proporsi varians dari setiap komponen utama dalam PCA
print(pca_result$eig)

# Proporsi varians faktor dalam FA
fa_result$Vaccounted











