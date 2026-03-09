#Pacotes
library(psych)
library(corrplot)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(reshape2)
library(GGally)
library(fmsb)
library(MASS)
library(mclust)
library(cluster)
library(RColorBrewer)
library(isotree)
library(tibble)
library(factoextra)   




#TRATAMENTO DOS DADOS----------------------------------------------------------------------------------------------------------------------------------------

dataset <- read.csv("dados_acidentes.csv")

#Filtra o dataset de acordo com o nosso grupo
dataset_filtrado <- dataset[dataset$T1GR04==1,]


#Remove as colunas binárias de cada grupo
df_final <- dataset_filtrado[, !(names(dataset_filtrado) %in% c("T1GR01", "T1GR02", "T1GR03", "T1GR04", "T1GR05", "T1GR06", "T1GR07", "T1GR08", "T1GR09", "T1GR10" 
                                                                ,"T1GR11", "T1GR12", "T1GR13", "T1GR14", "T2GR01", "T2GR02",  "T2GR03", "T2GR04", "T2GR05","T2GR06", "T2GR07" ,"T2GR08", "T2GR09" ,"T2GR10" ,"PLGR01", "PLGR02",
                                                                "PLGR03" ,"PLGR04" ,"PLGR05" ,"PLGR06", "PLGR07", "PLGR08", "PLGR09", "PLGR10"))]

nrow(df_final)

#Trata os valores omissos e ?
df_final$property_damage[df_final$property_damage == "?"] <- "unknown"
df_final$police_report_available[df_final$police_report_available == "?"] <- "unknown"
df_final$collision_type[df_final$collision_type == "?"] <- "unknown"
df_final

#Verifica se já substituímos todos os ? por unknown
"?" %in% df_final



#Número de unknowns nas variáveis binárias
sum(df_final$property_damage == "unknown", na.rm = TRUE)
sum(df_final$police_report_available == "unknown", na.rm = TRUE)



#Remove as variáveis que não conseguimos converter em variáveis categóricas
df_pca <- cbind(df_final[,c(8,9,10,11,13,14,15,16,17,18,20,21,22)]) #vars que vamos usar para a pca
df_perfil <- cbind(df_final[,c(1,2,3,4,5,6,7,12,23,24)]) #as vars que não vamos usar para a pca (vars de perfil)


#Cria escalas para as variáveis categóricas com ordem
if ("incident_severity" %in% names(df_pca)) {
  df_pca$incident_severity <- factor(df_pca$incident_severity,
                                     levels = c("Trivial Damage", "Minor Damage", "Major Damage", "Total Loss"),
                                     ordered = TRUE)
}

#Faz One-hot Encoding para as variáveis binárias
binary_vars <- c("property_damage", "police_report_available")
for (col in binary_vars) {
  if (col %in% names(df_pca)) {
    df_pca[[col]] <- tolower(trimws(as.character(df_pca[[col]])))
  }
}


for (col in binary_vars) {
  if (col %in% names(df_pca)) {
    dummies <- model.matrix(~ get(col) - 1, data = df_pca)
    colnames(dummies) <- paste0(col, "_", gsub("get\\(col\\)", "", colnames(dummies)))
    df_pca <- cbind(df_pca, dummies)
  }
}
#Remove as colunas originais binárias
df_pca <- df_pca[, !names(df_pca) %in% binary_vars]

#Converte variáveis nominais com One-Hot Encoding
nominal_vars <- c("collision_type", "incident_type")

existing_nominals <- intersect(nominal_vars, names(df_pca))
if (length(existing_nominals) > 0) {
  df_pca[, existing_nominals] <- lapply(df_pca[, existing_nominals], factor)
  dummies_nom <- model.matrix(~ . - 1, data = df_pca[, existing_nominals])
  dummies_nom <- as.data.frame(dummies_nom)
  df_pca <- cbind(df_pca[, !names(df_pca) %in% existing_nominals], dummies_nom)
}
#One-hot encoding para a variável incidente_date por mês (janeiro, fevereiro, março)
if ("incident_date" %in% names(df_pca)) {
  df_pca$incident_date <- as.Date(df_pca$incident_date)
  df_pca$month <- factor(format(df_pca$incident_date, "%m"), levels = sprintf("%02d", 1:12))
  
#One-hot encoding dos meses
  dummies_month <- model.matrix(~ month - 1, data = df_pca)
  dummies_month <- as.data.frame(dummies_month)
  df_pca <- cbind(df_pca[, !names(df_pca) %in% "month"], dummies_month)
}



#Verifica quais são os ano que existem no nosso datset
if ("incident_date" %in% names(df_pca)) {
  
  #Seleciona os anos da coluna
  anos <- unique(format(df_pca$incident_date, "%Y"))
  
  #Mostra os anos
  print(anos)
}

#Retira as colunas com meses a zeros 
df_pca <- df_pca[, !names(df_pca) %in% c("month04", "month05","month06","month07","month08","month09","month10","month11","month12")]
df_pca



ordinal_vars <- c("incident_severity")
for (col in ordinal_vars) {
  if (col %in% names(df_pca) && is.factor(df_pca[[col]])) {
    df_pca[[col]] <- as.numeric(df_pca[[col]])
  }
}

#Mantém apenas as colunas numéricas
df_pca_final <- df_pca[, sapply(df_pca, is.numeric)]
""

#Retira um dummy por variável onde foi utilizado One-Hot Encoding (o com menos dados, por isso menor soma)
sum(df_pca$`incident_typeParked Car`)
sum(df_pca$`incident_typeSingle Vehicle Collision`)
sum(df_pca$`incident_typeVehicle Theft`)
sum(df_pca$month01)
sum(df_pca$month02)
sum(df_pca$month03)

#Guarda as variáveis que vamos eliminar do dataset principal
dummies_retirados <- cbind(df_pca_final$property_damage_unknown,df_pca_final$police_report_available_unknown,df_pca_final$collision_typeunknown,df_pca_final$`incident_typeParked Car`,
                           df_pca_final$month03)
dummies_retirados

#Remove as colunas
df_pca_final <- df_pca_final[ , !(names(df_pca_final) %in% c("property_damage_unknown","police_report_available_unknown","collision_typeunknown","incident_typeParked Car",
                                                             "month03")) ]
df_pca_final

#-------------------------------------------------------------------------------------------------------


#OUTLIERS-----------------------------------------------------------------------------------------------

#ISOLATION FOREST - DETEÇÃO DE OUTLIERS COM VALORES ORIGINAIS

#Seleção de variáveis numéricas e exclusão de PCs
X_num <- df_pca_final %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(-matches("^(PC|Comp|Dim)\\d+$"))

if (ncol(X_num) == 0) stop("Sem variáveis numéricas úteis.")

#Escala robusta (apenas para construir o modelo)
robust_scale <- function(x) {
  med <- median(x, na.rm = TRUE)
  madv <- mad(x, constant = 1, na.rm = TRUE)
  if (!is.finite(madv) || madv == 0) madv <- 1
  (x - med) / madv
}

X_scaled <- X_num %>% mutate(across(everything(), robust_scale))
Xmat <- as.matrix(X_scaled)


#Construção do modelo Isolation Forest
iso_model <- isolation.forest(
  Xmat,
  ntrees = 500,
  sample_size = min(256, nrow(Xmat)),
  ndim = 2,
  missing_action = "impute",
  seed = 42
)


#Previsão dos scores e definição de outliers
iso_scores <- predict(iso_model, Xmat, type = "score")

iso_threshold <- quantile(iso_scores, 0.97)
iso_outlier <- iso_scores >= iso_threshold

cat("Threshold Isolation Forest:", round(iso_threshold, 3), "\n")
cat("Número de outliers detectados:", sum(iso_outlier), "de", nrow(X_num), "\n")


#Tabela de outliers 
outliers_orig <- df_pca_final[iso_outlier, , drop = FALSE]

cat("\nPrimeiras 10 observações outliers (valores originais):\n")
print(head(outliers_orig, 10))

# Gráfico dos scores -> outliers destacados
score_df <- tibble(
  row_id = seq_len(nrow(X_scaled)),
  score = iso_scores,
  outlier = iso_outlier
)

ggplot(score_df, aes(x = row_id, y = score, color = outlier)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  geom_hline(yintercept = iso_threshold, linetype = "dashed", color = "blue") +
  labs(
    title = "Isolation Forest - Scores e Outliers",
    x = "Observações",
    y = "Score de anomalia",
    color = "Outlier"
  ) +
  theme_minimal()

#Seleciona todas as observações que são outliers 
outliers_all <- df_pca_final[iso_outlier, , drop = FALSE]

#Número de outliers
cat("Número total de outliers detectados:", nrow(outliers_all), "\n")

outliers_all_ord <- outliers_all[
  order(
    -outliers_all$police_report_available_no,
    outliers_all$property_damage_yes,
    outliers_all$`collision_typeSide Collision`,
    outliers_all$`collision_typeFront Collision`,
    outliers_all$`incident_typeSingle Vehicle Collision`),
]

outliers_all_ord


#Cria uma cópia do df sem os outliers 
df_limpo <- df_pca_final[!iso_outlier, , drop = FALSE]
df_perfil_limpo <- df_perfil[!iso_outlier, , drop = FALSE]

#Verifica o número de linhas antes e depois
cat("Número de observações original:", nrow(df_pca_final), "\n")
cat("Número de observações após remover outliers:", nrow(df_limpo), "\n")

#--------------------------------------------------------------------------------------------------------------------------------------------------



#PCA----------------------------------------------------------------------------------------------------

correlation <- cor(df_limpo)
corrplot.mixed(correlation,
               order="hclust",
               tl.pos="lt",
               upper="ellipse")




#Correlation matrix
round(correlation, 3)

#Teste de Bartlett
cortest.bartlett(correlation)

#Medida KMO  
KMO(correlation)

ncol(df_limpo)

#Standartiza o dataset
data_scaled <- scale(df_limpo)

#Cria 19 componentes para as 19 variáveis
pc19 <- principal(data_scaled, nfactors=19, rotate="none")
pc19$loadings

#Screeplot 
plot(pc19$values, 
     type = "b", 
     pch = 19,
     main = "Scree Plot ",
     xlab = "Número de Componentes",
     ylab = "Valor próprio")
abline(h = 1, col = "red", lty = 2)

#Valores próprios
round(pc19$values,3)


#Variância explicada recomenda seis componentes 
pc6 <- principal(data_scaled,nfactors = 6,rotate="none",score=TRUE)
pc6

#Comunalidades da solução de 6 componentes
round(pc6$communality,3)
pc6r <- principal(data_scaled, nfactors=6, rotate="varimax")
pc6r$loadings

#Heatmap

#Obtém as loadings como um dataframe
load_mat <- as.data.frame(unclass(pc6r$loadings))
load_mat$Variable <- rownames(load_mat)

#Reorganiza para formato longo
load_long <- melt(load_mat, id.vars = "Variable",
                  variable.name = "Component",
                  value.name = "Loading")

#Heatmap com valores escritos em cima das cores
ggplot(load_long, aes(x = Component, y = Variable, fill = Loading)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Loading, 2)), size = 3, color = "black") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, name = "Loading") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 16, face = "bold")) +
  ggtitle("Heatmap dos Loadings dos Componentes Principais (com valores)")

load_melt <- load_long %>%
  dplyr::filter(abs(Loading) >= 0.3)


ggplot(load_melt, aes(x = Component, y = Variable, fill = Loading)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Loading,2)), size = 5, fontface = "bold") +  # valores maiores e em negrito
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  xlab("Componente PCA") +
  ylab("Variável") +
  ggtitle("Heatmap das Loadings do PCA (≥ 0.3)") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12, face = "bold")   # nomes das variáveis em negrito
  )



#Radrachart

# Extrair loadings Varimax
loadings_df <- as.data.frame(pc6r$loadings[1:ncol(data_scaled), 1:6])
colnames(loadings_df) <- c("Nível de gravidade do acidente",
                           "Efeitos temporais",
                           "Dinâmica e nº de veiculos envolvidos",
                           "Existência de relatório policial",
                           "Existência de danos á propriedade",
                           "Tipo de colisão")

#Usa o módulo dos loadings
abs_loadings <- abs(loadings_df)

#Limites para radarchart
max_load <- 1
min_load <- 0

#Layout 2 linhas x 3 colunas
par(mfrow=c(2,3), mar=c(2,2,5,2))

#Cores diferentes para cada componente
cores <- rainbow(6)

#Vetor de diminutivos na ordem correta das colunas do comp_chart
diminutivos <- c(
  "inc_sev",      # incident_severity
  "hour_day",     # incident_hour_of_the_day
  "nveic",    # number_of_vehicles_involved
  "b_injuries",      # bodily_injuries
  "witnesses",     # witnesses
  "injur_claim",   # injury_claim
  "prop_claim",   # property_claim
  "vehic_claim",   # vehicle_claim
  "PD_No",    # property_damage_no
  "PD_Yes",   # property_damage_yes
  "PR_No",    # police_report_available_no
  "PR_Yes",   # police_report_available_yes
  "col_front",# collision_typeFront Collision
  "col_rear", # collision_typeRear Collision
  "col_side", # collision_typeSide Collision
  "inc_sing", # incident_typeSingle Vehicle Collision
  "inc_theft",# incident_typeVehicle Theft
  "M01",      # month01
  "M02"       # month02
)
for(i in 1:6){
  # Cria o dataframe para o radarchart da componente i
  comp_chart <- rbind(rep(max_load, nrow(abs_loadings)),  # máximo
                      rep(min_load, nrow(abs_loadings)),  # mínimo
                      as.data.frame(t(abs_loadings[,i, drop=FALSE])))
  
  colnames(comp_chart) <- rownames(loadings_df)
  
  radarchart(comp_chart,
             axistype=1,
             pcol=cores[i],
             pfcol=adjustcolor(cores[i], alpha.f=0.3),
             plwd=2,
             cglcol="grey", cglty=1, cglwd=0.8,
             vlabels=diminutivos,       # usa os diminutivos definidos
             vlcex=1.5,                 # aumenta tamanho dos nomes das variáveis
             cex.main=1.8,
             title=colnames(loadings_df)[i],
             font.main=2                # título em negrito
  )
}



#Critério de Kaiser para a solução das 8 componentes

#Solução das 8 componentes sem rotação
pc8<- principal(data_scaled, nfactors=8, rotate="none", scores=TRUE)
pc8

#Comunalidades
round(pc8$communality,3)
pc8r <- principal(data_scaled, nfactors=8, rotate="varimax")
pc8r$loadings


#Cria dataframe para guardar os scores da pca
pca_final <- data.frame(matrix(nrow = nrow(pc6r$scores), ncol = 0))

#Compute Scores
pca_final$pc1 <- pc6r$scores[,1]
pca_final$pc2 <- pc6r$scores[,2]
pca_final$pc3 <- pc6r$scores[,3]
pca_final$pc4 <- pc6r$scores[,4]
pca_final$pc5 <- pc6r$scores[,5]
pca_final$pc6 <- pc6r$scores[,6]

scores <- as.data.frame(pc6r$scores)


pca_final

#-------------------------------------------------------------------------------------------------------------------------------------------------





#Clusters-------------------------------------------------------------------------------------------------------------------------------------------



#Clustering Hierárquico
par(mfrow=c(1,1))

demodist <- dist(pca_final) #calcular a distância
hclust_demo <- hclust(demodist,method='ward.D2')


groups.k6 <- cutree(hclust_demo, k=6) 



sil <- silhouette(groups.k6, demodist)


plot(sil,
     col = brewer.pal(length(unique(groups.k6)), "Set2"), 
     border = NA,
     main = "Gráfico de Silhouette (Clustering Hierarquico)",
     cex.names = 0.6,
     cex.axis = 0.8,
     cex.main = 1.2)


k <- 6

par(lwd = 2)

plot(hclust_demo, labels = FALSE, main = "Dendrograma Clustering Hierárquico")
rect.hclust(hclust_demo, k = k, border = 2:5)

aggregate(pca_final,list(groups.k5),mean)


#K-médias com centóides
kmeans.k6 <- kmeans(pca_final,6,nstart = 100)
attributes(kmeans.k6)
kmeans.k6$centers
kmeans.k6$size


pca_df <- as.data.frame(pca_final)
pca_df$cluster <- factor(kmeans.k6$cluster)


centers_df <- as.data.frame(kmeans.k6$centers)

table(groups.k6,kmeans.k6$cluster) #comparar o clustering hierárquico com o K-médias 

#Gráfico de dispersão com ggplot2
ggplot(pca_df, aes(x = pca_df[,3], y = pca_df[,6], color = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_point(data = centers_df, aes(x = centers_df[,1], y = centers_df[,2]),
             color = "black", size = 5, shape = 8) +
  labs(title = "Clusters com K-means",
       x = "PC3", y = "PC6") +
  theme_minimal() +
  theme(legend.position = "right")


#Dataframe com PCs e clusters
pca_df <- as.data.frame(pca_final)
colnames(pca_df) <- paste0("PC", 1:ncol(pca_df))
pca_df$cluster <- factor(kmeans.k5$cluster)

#Todas as combinações possíveis de PCs 
pairs(pca_df[,1:6], col=pca_df$cluster, pch=19,
      main="Clusters Kmedias nas combinações de PCs")


sil <- silhouette(kmeans.k5$cluster, demodist)


plot(sil,
     col = brewer.pal(length(unique(kmeans.k5$cluster)), "Set2"), # paleta Set2
     border = NA,
     main = "Gráfico de Silhouette (K-means)",
     cex.names = 0.6,
     cex.axis = 0.8,
     cex.main = 1.2)

#K-médias com medóides

#PAM clustering
pam.k6 <- pam(pca_final, 6)


sil <- silhouette(pam.k5)


plot(sil,
     col = brewer.pal(length(unique(pam.k5$clustering)), "Set2"), #paleta Set2
     border = NA,
     main = "Silhouette - K-medoids",
     cex.names = 0.6,
     cex.axis = 0.8,
     cex.main = 1.2)



# Clusters Medóides
pca_df$cluster <- factor(pam.k6$clustering)

# Medóides 
medoids_idx <- pam.k6$id.med
medoids_df <- pca_df[medoids_idx, ]

# Gráfico PCA
ggplot(pca_df, aes(x = pca_df[,3], y = pca_df[,6], color = cluster)) +
  geom_point(alpha=0.7, size=2) +
  geom_point(data=medoids_df, aes(x = medoids_df[,1], y = medoids_df[,2]),
             color="black", size=5, shape=8) +
  labs(title="Clusters com K-medoids", x="PC3", y="PC6") +
  theme_minimal()


#Dataframe com PCs e clusters
pca_df <- as.data.frame(pca_final)
colnames(pca_df) <- paste0("PC", 1:ncol(pca_df))
pca_df$cluster <- factor(pam.k5$cluster)

#Todas as combinações possíveis de PCs
pairs(pca_df[,1:6], col=pca_df$cluster, pch=19,
      main="Clusters Kmedoides nas combinações de PCs")




#Clustering Probablístico

results.ky <- Mclust(pca_final,G=6)
summary(results.ky,parameters=TRUE)

#Probabilidades de cada observação para cada cluster
prob_clusters <- results.ky$z 

#Função de entropia por observação
entropy <- function(probs) {
  -rowSums(probs * log(probs + 1e-10))  
}

#Entropia média do clustering
mean_entropy <- mean(entropy(prob_clusters))
cat("Entropia média do clustering probabilístico:", mean_entropy, "\n")

BIC <- mclustBIC(pca_final)
plot(BIC)


#Dataframe com PCs e clusters
pca_df <- as.data.frame(pca_final)
colnames(pca_df)[1:3] <- c("PC1","PC2","PC3") 
pca_df$cluster <- factor(results.ky$classification)
pca_df$uncertainty <- results.ky$uncertainty



#Gráfico PCA
ggplot(pca_df, aes(x = pca_df[,3], y = pca_df[,6], color = cluster)) +
  geom_point(alpha=0.7, size=2) +
  geom_point(data=medoids_df, aes(x = medoids_df[,1], y = medoids_df[,2]),
             color="black", size=5, shape=8) +
  labs(title="Clusters probablistico ", x="PC3", y="PC6") +
  theme_minimal()


#GRÁFICO CLASSIFICAÇÃO TODAS AS COMBINAÇÔES

#Dataframe com PCs e clusters
pca_df <- as.data.frame(pca_final)
colnames(pca_df) <- paste0("PC", 1:ncol(pca_df))
pca_df$cluster <- factor(results.ky$classification)

#Todas as combinações possíveis de PCs
pairs(pca_df[,1:6], col=pca_df$cluster, pch=19,
      main="Clusters Probabilísticos (Mclust) nas combinações de PCs")



#Intrepretação com variáveis originais de perfil e sem ser de perfil
df_limpo <- cbind(df_perfil_limpo)

df_limpo$cluster <- results.ky$classification


caracterizar_clusters <- function(df_limpo, cluster_col) {
  library(dplyr)
  
  clusters <- unique(df_limpo[[cluster_col]])
  resultado <- list()
  
  for (c in clusters) {
    cluster_df <- df_limpo %>% filter(!!sym(cluster_col) == c) %>% select(-all_of(cluster_col))
    
   
    numericas <- cluster_df %>% 
      select(where(is.numeric)) %>% 
      summarise(across(everything(),
                       list(media = mean, mediana = median, sd = sd, min = min, max = max),
                       na.rm = TRUE))
    
  
    categoricas <- cluster_df %>% 
      select(where(is.factor) | where(is.character)) %>% 
      lapply(function(x) table(x))
    
    resultado[[as.character(c)]] <- list(
      num_observacoes = nrow(cluster_df),
      numericas = numericas,
      categoricas = categoricas
    )
  }
  
  return(resultado)
}


resultado <- caracterizar_clusters(df_limpo, "cluster")


for (c in names(resultado)) {
  cat("\nCluster:", c, "\n")
  cat("Número de observações:", resultado[[c]]$num_observacoes, "\n")
  cat("Resumo variáveis numéricas:\n")
  print(resultado[[c]]$numericas)
  cat("Resumo variáveis categóricas:\n")
  print(resultado[[c]]$categoricas)
}






#----------------------------------------------------------------------------------------------------------


# CLUSTERING  EM PC3 E PC6---------------------------------------------------------------------


#Scores da pca
scores_pca <- as.data.frame(pca_final)
colnames(scores_pca) <- paste0("PC", 1:ncol(scores_pca))

#Seleciona apenas PC3 e PC6
X_pc36 <- scores_pca[, c("PC3", "PC6")]



#CLUSTERING HIERÁRQUICO 

dist_pc36 <- dist(X_pc36)
hc_pc36 <- hclust(dist_pc36, method = "ward.D2")
clusters_hc <- cutree(hc_pc36, k = k)

#Dendrograma
dev.new()
par(lwd = 2)
plot(
  hc_pc36,
  labels = FALSE,
  hang = -1,
  main = "Dendrograma - Hierárquico (Ward.D2)"
)
rect.hclust(hc_pc36, k = k, border = brewer.pal(k, "Set2"))

#Gráfico de Silhoueta 
sil_hc <- silhouette(clusters_hc, dist_pc36)
dev.new()
plot(
  sil_hc,
  col = brewer.pal(k, "Set2"),
  border = NA,
  main = "Silhouette - Hierárquico (PC3 e PC6)",
  cex.names = 0.6,
  cex.axis = 0.8,
  cex.main = 1.2
)
cat("Silhouette média - Hierárquico:", round(mean(sil_hc[,3]),3), "\n")



#K-MÉDIAS COM CENTRÓIDES

set.seed(123)
k <- 6 
km_pc36 <- kmeans(X_pc36, centers = k, nstart = 50)
clusters_km <- km_pc36$cluster

#Gráfico de Silhueta
sil_km <- silhouette(clusters_km, dist(X_pc36))
dev.new()
plot(
  sil_km,
  col = brewer.pal(k, "Set2"),
  border = NA,
  main = "Silhouette - K-means (PC3 e PC6)",
  cex.names = 0.6,
  cex.axis = 0.8,
  cex.main = 1.2
)

#Gráfico de dispersão 
plot_df_km <- data.frame(
  PC3 = X_pc36$PC3,
  PC6 = X_pc36$PC6,
  Cluster = factor(clusters_km)
)

ggplot(plot_df_km, aes(PC3, PC6, color = Cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(
    title = "K-means (PC3 vs PC6)",
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set2")


table(clusters_hc,km_pc36$cluster)#comparar hierarquico e kmedia


#INTERPRETAÇÃO: VARIÁVEIS ORIGINAIS

df_orig <- as.data.frame(df_limpo) 
df_orig$cluster <- clusters_km       


caracterizar_clusters_orig <- function(df, cluster_col = "cluster") {
  clusters <- unique(df[[cluster_col]])
  resultado <- list()
  for (c in clusters) {
    cluster_df <- df %>% filter(!!sym(cluster_col) == c) %>% select(-all_of(cluster_col))
    
    
    numericas <- cluster_df %>% 
      select(where(is.numeric)) %>% 
      summarise(across(everything(),
                       list(media = mean, mediana = median, sd = sd, min = min, max = max),
                       na.rm = TRUE))
    
    
    categoricas <- cluster_df %>% 
      select(where(is.factor) | where(is.character)) %>% 
      lapply(function(x) table(x))
    
    resultado[[as.character(c)]] <- list(
      num_observacoes = nrow(cluster_df),
      numericas = numericas,
      categoricas = categoricas
    )
  }
  return(resultado)
}


resultado_orig <- caracterizar_clusters_orig(df_orig, "cluster")


for (c in names(resultado_orig)) {
  cat("\nCluster:", c, "\n")
  cat("Número de observações:", resultado_orig[[c]]$num_observacoes, "\n")
  cat("Resumo variáveis numéricas:\n")
  print(resultado_orig[[c]]$numericas)
  cat("Resumo variáveis categóricas:\n")
  print(resultado_orig[[c]]$categoricas)
}


#K-MÉDIAS COM MEDÓIDES

pam_pc36 <- pam(X_pc36, k = k)
clusters_pam <- pam_pc36$clustering

#Gráfico de Silhueta
sil_pam <- silhouette(clusters_pam, dist(X_pc36))
dev.new()
plot(
  sil_pam,
  col = brewer.pal(k, "Set2"),
  border = NA,
  main = "Silhouette - K-medoids (PC3 e PC6)",
  cex.names = 0.6,
  cex.axis = 0.8,
  cex.main = 1.2
)

#Gráfico de dispersão
plot_df_pam <- data.frame(
  PC3 = X_pc36$PC3,
  PC6 = X_pc36$PC6,
  Cluster = factor(clusters_pam)
)

ggplot(plot_df_pam, aes(PC3, PC6, color = Cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(
    title = "K-medoids (PAM) (PC3 vs PC6)",
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set2")

table(results.ky$classification,km_pc36$cluster) #comparar clustering hierárquico e k-média com medóides


