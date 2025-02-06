input_path = "D:/FCDados/[17] - Mini Projeto 1/[01] - InputData/"
output_path = "D:/FCDados/[17] - Mini Projeto 1/[02] - OutputData/"
projeto_path = "D:/FCDados/[17] - Mini Projeto 1/[03] - Projetos/"
setwd(projeto_path)
getwd()

install.packages('data.table')
library('data.table')

setwd(input_path)
dataset <- fread("dataset.csv", stringsAsFactors = TRUE)
View(dataset)
str(dataset)
summary(dataset)

dataset_nan_omited <- na.omit(dataset)
View(dataset_nan_omited)


# Pergunta 1
library('ggplot2')
?cor.test
Pergunta1 <- cor.test(dataset_nan_omited$`Log GDP per capita`, dataset_nan_omited$`Healthy life expectancy at birth`, method = "pearson")
Pergunta1

ggplot(dataset_nan_omited, 
       aes(dataset_nan_omited$`Log GDP per capita`, 
          dataset_nan_omited$`Healthy life expectancy at birth`)) + 
geom_point() +
geom_smooth(method = "lm", se = FALSE) +
labs(title = "PIB x Expectativa de Vida",
     x = "PIB",
     y = "Expectativa de Vida")


# Pergunta 2

Pergunta2 <- cor.test(dataset_nan_omited$`Life Ladder`, dataset_nan_omited$`Perceptions of corruption`, method = "pearson")
Pergunta2

ggplot(dataset_nan_omited, 
       aes(dataset_nan_omited$`Life Ladder`, 
           dataset_nan_omited$`Perceptions of corruption`)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Escala de Vida x Corrupção",
       x = "Escala de Vida",
       y = "Corrupção")

# Pergunta 3

Pergunta3 <- cor.test(dataset_nan_omited$`Positive affect`, dataset_nan_omited$`Life Ladder`, method = "pearson")
Pergunta3

ggplot(dataset_nan_omited, 
       aes(dataset_nan_omited$`Positive affect`, 
           dataset_nan_omited$`Life Ladder`)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Felicidade x Escala de Vida",
       x = "Felicidade",
       y = "Escala de Vida")

# Pergunta 4

Pergunta4 <- cor.test(dataset_nan_omited$`Social support`, dataset_nan_omited$`Perceptions of corruption`, method = "pearson")
Pergunta4

ggplot(dataset_nan_omited, 
       aes(dataset_nan_omited$`Social support`, 
           dataset_nan_omited$`Perceptions of corruption`)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Suporte Social x Corrupção",
       x = "Suporte Social",
       y = "Corrupção")

# Pergunta 5

Pergunta5 <- cor.test(dataset_nan_omited$`Positive affect`, dataset_nan_omited$Generosity, method = "pearson")
Pergunta5

ggplot(dataset_nan_omited, 
       aes(dataset_nan_omited$`Positive affect`, 
           dataset_nan_omited$Generosity)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Felicidade x Generosidade",
       x = "Felicidade",
       y = "Generosidade")
