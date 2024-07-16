## Lonomia obliqua distribution - Brazil
## Marília Melo Favalesso - mariliabioufpr@gmail.com

# 1. Limpando a memória
rm(list = ls())
gc()
memory.limit(size = 1.75e13)

# Carregando pacotes
library(raster)
library(rgdal)
library(dismo)
library(gam)
library(randomForest)
library(kernlab)
library(rJava)
library(vegan)

# Verificar pacotes carregados
search()

###---------------------------------------------------------------------------###

# Ocorrências
po <- read.table("lonomia_obliqua.txt", header = TRUE)
head(po, 10)
plot(po$long, po$lat, pch = 20)

# Importar as variáveis
tif_files <- list.files(pattern = "tif")
tif_files

# Filtrar variáveis por sufixos
suffixes <- c("0k", "6k", "21k")
environments <- lapply(suffixes, function(suffix) {
  files <- grep(suffix, tif_files, value = TRUE)
  stack(files)
})

names_list <- paste0("bio", c("02", "04", "10", "16", "17"))
for (env in environments) {
  names(env) <- names_list
  plot(env)
}

# Plotar o primeiro ambiente e pontos de ocorrência
plot(environments[[1]][[1]])
points(po$long, po$lat, pch = 20)

# Extração das coordenadas para o background ou pseudoausências
cell_ids <- 1:ncell(environments[[1]])
coordinates <- xyFromCell(environments[[1]], cell_ids)
valid_values <- na.omit(data.frame(coordinates, values(environments[[1]])[, 1]))

cs <- valid_values[, 1:2]
colnames(cs) <- c("long", "lat")

plot(environments[[1]][[1]])
points(cs, pch = "o", cex = 0.1)

###---------------------------------------------------------------------------###

# Verificando o Maxent
maxent_path <- paste0(system.file(package = "dismo"), "/java/maxent.jar")
if (!file.exists(maxent_path)) {
  stop("Maxent não encontrado. Verifique o caminho.")
}

###---------------------------------------------------------------------------###

# Modelagem ENMs
output_dir <- "../02_output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}
setwd(output_dir)

# Definir AOGCM
AOGCM <- "CCSM"

# Função auxiliar para avaliação
evaluate_model <- function(test_data, model, threshold_method = "spec_sens") {
  eval <- evaluate(p = test_data[test_data[, 1] == 1, -1], a = test_data[test_data[, 1] == 0, -1], model = model)
  threshold_index <- which(eval@t == as.numeric(threshold(eval, threshold_method)))
  c(eval@t[threshold_index], eval@auc, (eval@TPR[threshold_index] + eval@TNR[threshold_index] - 1))
}

# Loop principal para cada espécie
for (specie in levels(po[, 1])) {
  specie_data <- po[po[, 1] == specie, 2:3]
  background_data <- cs[sample(nrow(cs), nrow(specie_data)), ]
  
  evaluations <- list(Bioclim = NULL, Gower = NULL, Maha = NULL, Maxent = NULL, SVM = NULL)
  
  for (replica in 1:5) {
    # Separar dados de treino e teste
    train_indices <- sample(nrow(specie_data), round(0.7 * nrow(specie_data)))
    test_data <- na.omit(prepareData(x = environments[[1]], p = specie_data[-train_indices, ], b = background_data[-train_indices, ]))
    train_data <- na.omit(prepareData(x = environments[[1]], p = specie_data[train_indices, ], b = background_data[train_indices, ]))
    
    # Algoritmos de modelagem
    models <- list(
      Bioclim = bioclim(train_data[train_data[, 1] == 1, -1]),
      Gower = domain(train_data[train_data[, 1] == 1, -1]),
      Maha = mahal(train_data[train_data[, 1] == 1, -1]),
      Maxent = maxent(train_data[, -1], train_data[, 1]),
      SVM = ksvm(pb ~ ., data = train_data)
    )
    
    # Projeções e avaliações
    for (model_name in names(models)) {
      model <- models[[model_name]]
      for (env_index in 1:length(environments)) {
        projection <- predict(environments[[env_index]], model)
        writeRaster(projection, paste0(AOGCM, "_", tolower(model_name), "_", suffixes[env_index], "_", specie, sprintf("%02d", replica), ".tif"), format = "GTiff", overwrite = TRUE)
      }
      eval <- evaluate_model(test_data, model)
      evaluations[[model_name]] <- rbind(evaluations[[model_name]], eval)
      
      print(paste0("Yeh! The model of ", specie, ", algorithm '", model_name, "', replica ", sprintf("%02d", replica), " is done!"))
    }
  }
  
  # Salvar avaliações
  for (model_name in names(evaluations)) {
    eval_matrix <- evaluations[[model_name]]
    rownames(eval_matrix) <- paste0(specie, sprintf("%02d", 1:5))
    colnames(eval_matrix) <- c("thrs", "AUC", "TSS")
    write.table(eval_matrix, paste0("zEval_", AOGCM, "_", tolower(model_name), "_", specie, ".txt"))
  }
}

print("Yeh! It's over!!!")
###----------------------------------------------------------------------------###
