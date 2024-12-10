#Instalando pacotes 
pacotes <- c("tidyverse", 
             "dplyr",
             "rpart",
             "corrplot",
             "caret",
             "psych", 
             "kableExtra",
             "reshape2",
             "scales")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Carregando a base
BaseBotNet <- read_csv("BoTNeTIoT-L01-v2.csv")


#renomear as variáveis da base 
BaseBotNet <- rename(BaseBotNet, 
                           MI_dir_weight = MI_dir_L0.1_weight,
                           MI_dir_mean = MI_dir_L0.1_mean,
                           MI_dir_variance = MI_dir_L0.1_variance,
                           H_weight = H_L0.1_weight,
                           H_mean = H_L0.1_mean,
                           H_variance = H_L0.1_variance,
                           HH_weight = HH_L0.1_weight,
                           HH_mean = HH_L0.1_mean,
                           HH_std = HH_L0.1_std,
                           HH_magnitude = HH_L0.1_magnitude,
                           HH_radius = HH_L0.1_radius,
                           HH_covariance = HH_L0.1_covariance,
                           HH_pcc = HH_L0.1_pcc,
                           HH_jit_weight = HH_jit_L0.1_weight,
                           HH_jit_mean = HH_jit_L0.1_mean,
                           HH_jit_variance = HH_jit_L0.1_variance,
                           HpHp_weight = HpHp_L0.1_weight,
                           HpHp_mean = HpHp_L0.1_mean,
                           HpHp_std = HpHp_L0.1_std,
                           HpHp_magnitude = HpHp_L0.1_magnitude,
                           HpHp_radius = HpHp_L0.1_radius,
                           HpHp_covariance = HpHp_L0.1_covariance,
                           HpHp_pcc = HpHp_L0.1_pcc)



#Nos ataques Mirai, não consta tráfego normal, apenas de ataques, nesse caso
#vou pegar observações de tráfego normal de outros ataques e as obs. Mirai
#colocar na mesma base 

Base_Bot_Net_Mirai_Trafego_Normal <- BaseBotNet %>% filter(Attack == "mirai" | label == 1)


#################################################################################
#                         CÁLCULO CORRELAÇÕES                                   #
#################################################################################
Base_Calculo_Correlacao <- Base_Bot_Net_Mirai_Trafego_Normal[,1:23]
summary(Base_Calculo_Correlacao)
Matriz_Correlacao <- cor(Base_Calculo_Correlacao)

#plot correlacoes
corrplot(Matriz_Correlacao, type = "lower", method = "number")

#calcula matriz de correlação e uma matriz de valor P (não deu certo)
MatrizCorrelacaoEPvalor <- rcorr(as.matrix(BaseCalculoCorrelacao))
#grafico 
corrplot(MatrizCorrelacaoEPvalor$r, p.mat = MatrizCorrelacaoEPvalor$p, sig.level = 0.005, method="color", type="lower")

#Separando as menores correlacoes
#Coordenada dos valores menores que 0.7 na matrix original

Base_Maiores_Correlacoes <- findCorrelation(Matriz_Correlacao, cutoff = 0.7, verbose = FALSE, names = FALSE, exact = FALSE)

#Filtrando na base de dados as variáveis com correlação maior que 0.7
Base_Bot_Net_Maiores_Correlacoes_no_label <- Base_Calculo_Correlacao[, Base_Maiores_Correlacoes]

#filtrando a variável label da base original
label <- Base_Bot_Net_Mirai_Trafego_Normal$label

#incluindo a variável label na base com as variáveis mais correlacionadas
Base_Bot_Net_Maiores_Correlacoes <- cbind(Base_Bot_Net_Maiores_Correlacoes_no_label, label)  

#matriz de correlação com as variáveis mais correlacionadas
Matriz_Maiores_Correlacoes <- cor(Base_Bot_Net_Maiores_Correlacoes_no_label)

#plot das variáveis mais correlacionadas
corrplot(Matriz_Maiores_Correlacoes, type = "lower", method = "square")

#################################################################################
#                    ÁRVORE DE REGRESSÃO TODAS AS VARIÁVEIS                     #
#################################################################################

#Com todas as  variáveis da base 
#Para reprodução
set.seed(123)

#Retirando amostra de 70%
Base_Amostra <- sample(nrow(Base_Bot_Net_Mirai_Trafego_Normal), 0.7*nrow(Base_Bot_Net_Mirai_Trafego_Normal))

#Base de treino e teste
Base_Treino <- Base_Bot_Net_Mirai_Trafego_Normal[Base_Amostra, ]

Base_Teste <- Base_Bot_Net_Mirai_Trafego_Normal[-Base_Amostra, ]

#Base Treino
Tempo_Inicial <- Sys.time()

arvore_TODAS_VARIAVEIS_Treino <- rpart(label ~ MI_dir_weight + MI_dir_mean + MI_dir_variance + H_weight + H_mean + H_variance + HH_weight + HH_mean + HH_std + HH_magnitude + HH_radius + HH_covariance + HH_pcc + HH_jit_weight + HH_jit_variance + HpHp_weight + HpHp_mean + HpHp_std + HpHp_magnitude + HpHp_radius + HpHp_covariance + HpHp_pcc,
                                       data= Base_Treino,
                                       #subset = Base_Treino,
                                       control = rpart.control(maxdepth = 5, cp = 0.9, minsplit = 20)
                                       
)
Tempo_Final <- Sys.time()

Tempo_Processamento <- Tempo_Final - Tempo_Inicial

summary(arvore_TODAS_VARIAVEIS_Treino)

#Base Teste
Tempo_Inicial <- Sys.time()

arvore_TODAS_VARIAVEIS_Teste <- rpart(label ~ MI_dir_weight + MI_dir_mean + MI_dir_variance + H_weight + H_mean + H_variance + HH_weight + HH_mean + HH_std + HH_magnitude + HH_radius + HH_covariance + HH_pcc + HH_jit_weight + HH_jit_variance + HpHp_weight + HpHp_mean + HpHp_std + HpHp_magnitude + HpHp_radius + HpHp_covariance + HpHp_pcc,
                          data= Base_Teste,
                          control = rpart.control(maxdepth = 4, cp = 0.5, minsplit = 20)
                          
)
summary(arvore_TODAS_VARIAVEIS__Teste)

Tempo_Final <- Sys.time()

Tempo_Processamento <- Tempo_Final - Tempo_Inicial


#Probabilidade de ataques 
#Treino
Probabilidade_Treino = predict(arvore_TODAS_VARIAVEIS_Treino, Base_Treino)
#Teste
Probabilidade_Teste = predict(arvore_TODAS_VARIAVEIS_Teste, Base_Teste)

#Tentando transformar o objeto probabilidade em Matriz
Probabilidade_Treino. <- as.matrix(Probabilidade_Treino)
Probabilidade_Teste. <- as.matrix(Probabilidade_Teste)

#Classificação 
Classificacao_Treino = Probabilidade_Treino.[,1]>.5
Classificacao_Teste = Probabilidade_Teste.[,1]>.5

#Matriz de confusão
Tabela_Treino <- table(Classificacao_Treino, Base_Treino$label)
Tabela_Teste <- table(Classificacao_Teste, Base_Teste$label)

#Acurácia 
Acuracia <- (Tabela_Treino[1,1]+Tabela_Treino[2,2])/nrow(Base_Treino)
sprintf("Acurácia na base de Teste: %s", percent(Acuracia))

Acuracia <- (Tabela_Teste[1,1]+Tabela_Teste[2,2])/nrow(Base_Teste)
sprintf("Acurácia na base de Teste: %s", percent(Acuracia))

#################################################################################
#                    ÁRVORE DE REGRESSÃO VARIÁVEIS ÁRVORE                       #
#################################################################################

#Com todas as  variáveis da base 
#Para reprodução
set.seed(123)

#Retirando amostra de 70%
Base_Amostra <- sample(nrow(Base_Bot_Net_Mirai_Trafego_Normal), 0.7*nrow(Base_Bot_Net_Mirai_Trafego_Normal))

#Base de treino e teste
Base_Treino <- Base_Bot_Net_Mirai_Trafego_Normal[Base_Amostra, ]

Base_Teste <- Base_Bot_Net_Mirai_Trafego_Normal[-Base_Amostra, ]

#Base Treino
Tempo_Inicial <- Sys.time()

arvore_VARIAVEIS_ARVORE_Treino <- rpart(label ~ H_weight + MI_dir_weight + HpHp_radius + HpHp_std + HH_radius + HH_covariance,
                                       data= Base_Treino,
                                       #subset = Base_Treino,
                                       control = rpart.control(maxdepth = 5, cp = 0.9, minsplit = 20)
                                       
)
Tempo_Final <- Sys.time()

Tempo_Processamento <- Tempo_Final - Tempo_Inicial

summary(arvore_VARIAVEIS_ARVORE_Treino)

#Base Teste
Tempo_Inicial <- Sys.time()

arvore_VARIAVEIS_ARVORE_Teste <- rpart(label ~ H_weight + MI_dir_weight + HpHp_radius + HpHp_std + HH_radius + HH_covariance,
                          data= Base_Teste,
                          control = rpart.control(maxdepth = 4, cp = 0.5, minsplit = 20)
                          
)
summary(arvore_TODAS_VARIÁVEIS__Teste)

Tempo_Final <- Sys.time()

Tempo_Processamento <- Tempo_Final - Tempo_Inicial


#Probabilidade de ataques 
#Treino
Probabilidade_Treino = predict(arvore_VARIAVEIS_ARVORE_Treino, Base_Treino)
#Teste
Probabilidade_Teste = predict(arvore_VARIAVEIS_ARVORE_Teste, Base_Teste)

#Tentando transformar o objeto probabilidade em Matriz
Probabilidade_Treino. <- as.matrix(Probabilidade_Treino)
Probabilidade_Teste. <- as.matrix(Probabilidade_Teste)

#Classificação 
Classificacao_Treino = Probabilidade_Treino.[,1]>.5
Classificacao_Teste = Probabilidade_Teste.[,1]>.5

#Matriz de confusão
Tabela_Treino <- table(Classificacao_Treino, Base_Treino$label)
Tabela_Teste <- table(Classificacao_Teste, Base_Teste$label)

#Acurácia 
Acuracia <- (Tabela_Treino[1,1]+Tabela_Treino[2,2])/nrow(Base_Treino)
sprintf("Acurácia na base de Teste: %s", percent(Acuracia))

Acuracia <- (Tabela_Teste[1,1]+Tabela_Teste[2,2])/nrow(Base_Teste)
sprintf("Acurácia na base de Teste: %s", percent(Acuracia))



#################################################################################
#                     CORRELAÇÃO DE PEARSON + ÁRVORE DE REGRESSÃO               #
#################################################################################

#Com todas as  variáveis da base 
#Para reprodução
set.seed(123)

#Retirando amostra de 70%
Base_Amostra <- sample(nrow(Base_Bot_Net_Maiores_Correlacoes), 0.7*nrow(Base_Bot_Net_Maiores_Correlacoes))

#Base de treino e teste
Base_Treino <- Base_Bot_Net_Maiores_Correlacoes[Base_Amostra, ]

Base_Teste <- Base_Bot_Net_Maiores_Correlacoes[-Base_Amostra, ]

#Base Treino
Tempo_Inicial <- Sys.time()

arvore_Pearson_Treino<- rpart(label ~ H_weight + H_mean + H_variance + HpHp_mean + HpHp_magnitude + MI_dir_mean + HH_mean + HH_std + HH_weight + HH_jit_weight + HH_radius,
                              data = Base_Teste,
                              control = rpart.control(maxdepth = 5, cp = 0.9, minsplit = 20)
                      )

Tempo_Final <- Sys.time()

Tempo_Processamento <- Tempo_Final - Tempo_Inicial

summary(arvore_Pearson_Treino)

#Base Teste
Tempo_Inicial <- Sys.time()

arvore_Pearson_Teste <- rpart(label ~ H_weight + H_mean + H_variance + HpHp_mean + HpHp_magnitude + MI_dir_mean + HH_mean + HH_std + HH_weight + HH_jit_weight + HH_radius,
                              data = Base_Teste,
                              control = rpart.control(maxdepth = 4, cp = 0.5, minsplit = 20)
)
summary(arvore_Pearson_Teste)

Tempo_Final <- Sys.time()

Tempo_Processamento <- Tempo_Final - Tempo_Inicial


#Probabilidade de ataques 
#Treino
Probabilidade_Treino = predict(arvore_Pearson_Treino, Base_Treino)
#Teste
Probabilidade_Teste = predict(arvore_Pearson_Teste, Base_Teste)

#Tentando transformar o objeto probabilidade em Matriz
Probabilidade_Treino. <- as.matrix(Probabilidade_Treino)
Probabilidade_Teste. <- as.matrix(Probabilidade_Teste)

#Classificação 
Classificacao_Treino = Probabilidade_Treino.[,1]>.5
Classificacao_Teste = Probabilidade_Teste.[,1]>.5

#Matriz de confusão
Tabela_Treino <- table(Classificacao_Treino, Base_Treino$label)
Tabela_Teste <- table(Classificacao_Teste, Base_Teste$label)

#Acurácia 
Acuracia <- (Tabela_Treino[1,1]+Tabela_Treino[2,2])/nrow(Base_Treino)
sprintf("Acurácia na base de Teste: %s", percent(Acuracia))

Acuracia <- (Tabela_Teste[1,1]+Tabela_Teste[2,2])/nrow(Base_Teste)
sprintf("Acurácia na base de Teste: %s", percent(Acuracia))




#################################################################################
#                             ANÁLISE PCA                                       #
#################################################################################

#Teste de esfericidade de Bartlett para adequação dos dados
cortest.bartlett(Matriz_Correlacao)
#pra esse resultado talvez possa ser usado MCD (Minimum Covariance Determinant)

#Análise fatorial
#o R informou que nem todos os eigenvalues são positivos e que alguns são zeros
#e que isso pode impactar a base de dados. 
Analise_Fatorial <- principal(Base_Calculo_Correlacao,
                              nfactors = length(Base_Calculo_Correlacao),
                              rotate = "none",
                              scores = TRUE)
#Autovalores 
Eigenvalues <- round(Analise_Fatorial$values, 5)
print(Eigenvalues)

#Autovalores maiores que 1 
Autovalores_Maiores_Um <- sum(Eigenvalues > 1)

#Analise fatorial para 6 fatores
Analise_Fatorial <- principal(Base_Calculo_Correlacao,
                              nfactors = Autovalores_Maiores_Um,
                              rotate = "none",
                              scores = TRUE)
#Autovalores 
Eigenvalues <- round(Analise_Fatorial$values, 5)
print(Eigenvalues)

#Autovalores maiores que 1 
Autovalores_Maiores_Um <- sum(Eigenvalues > 1)


#Analisar a variância compartilhada
Variancia_compartilhada <- as.data.frame(Analise_Fatorial$Vaccounted) %>% 
  slice(1:3)

rownames(Variancia_compartilhada) <- c("Autovalores",
                                       "Prop. da Variância", 
                                       "Prop. da Variância Acumulada")

#Visualizando 
Variancia_compartilhada %>%
  slice(2) %>% 
  melt() %>% 
  ggplot(aes(x = variable, 
             y = value)) + 
  geom_col(fill = "orange", color = "black") +
  geom_text(aes(label = paste0(round(value*100, 2),"%") , vjust = -0.1))+
  labs(x = "Fatores",
       y = "Variância Compartilhada") +
  theme_bw()

#base reduzida, com os seis fatores PCA.
Base_Bot_Net_Reduzida_PCA_no_label <- predict(Analise_Fatorial, Base_Calculo_Correlacao)[, 1:Autovalores_Maiores_Um]

label <- Base_Bot_Net_Mirai_Trafego_Normal$label
Base_Bot_Net_Reduzida_PCA <- as.data.frame(cbind(Base_Bot_Net_Reduzida_PCA_no_label, label))

####################### até aqui acho que deu certo, os outros passos depois
#são do script da aula, mas não sei se eu precisava deles, por que 
#meu interesse maior era a variância compartilhada. 
#######################################################################

#Extração das cargas fatoriais 
cargas_fatoriais <- as.data.frame(unclass(Analise_Fatorial$loadings))

#visualizando 
cargas_fatoriais %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 12)

#Extraindo as comunalidades 
comunalidades <- as.data.frame(unclass(Analise_Fatorial$communality)) %>% 
  rename(comunalidades = 1)

# Visualizando as Comunalidades
comunalidades %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Relatório das cargas fatoriais e das comunalidades
cargas_fatoriais %>%
  mutate(Comunalidades = rowSums(cargas_fatoriais ^ 2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Identificação dos Scores Fatoriais
loadings_Analise_Fatorial <- loadings(Analise_Fatorial)
scores_fatoriais <- as.data.frame.matrix(loadings_Analise_Fatorial)

# Visualizando os Scores Fatoriais
scores_fatoriais %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Cálculo dos fatores extraídos 
fatores <- as.data.frame(Analise_Fatorial$scores)

Base_Bot_Net_Mirai_Trafego_Normal <- bind_cols(Base_Bot_Net_Mirai_Trafego_Normal,
                                               "fator_1" = fatores$PC1,
                                               "fator_2" = fatores$PC2,
                                               "fator_3" = fatores$PC3,
                                               "fator_4" = fatores$PC4,
                                               "fator_5" = fatores$PC5,
                                               "fator_6" = fatores$PC6
                                               )


#################################################################################
#                           ÁRVORE DE REGRESSÃO   PCA                           #
#################################################################################

#Com todas as  variáveis da base 
#Para reprodução
set.seed(123)

#Retirando amostra de 70%
Base_Amostra <- sample(nrow(Base_Bot_Net_Mirai_Trafego_Normal), 0.7*nrow(Base_Bot_Net_Mirai_Trafego_Normal))

#Base de treino e teste
Base_Treino <- Base_Bot_Net_Mirai_Trafego_Normal[Base_Amostra, ]

Base_Teste <- Base_Bot_Net_Mirai_Trafego_Normal[-Base_Amostra, ]

#Base Treino
Tempo_Inicial <- Sys.time()

arvore_PCA_Treino <- rpart(label ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, 
                data= Base_Treino,
                #subset = Base_Treino,
                control = rpart.control(maxdepth = 5, cp = 0.9, minsplit = 20)
                
)
Tempo_Final <- Sys.time()

Tempo_Processamento <- Tempo_Final - Tempo_Inicial

summary(arvore_maiores_correlacoes_Treino)

#Base Teste
Tempo_Inicial <- Sys.time()

arvore_PCA_Teste <- rpart(label ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6,
                                data= Base_Teste,
                                control = rpart.control(maxdepth = 4, cp = 0.5, minsplit = 20)
                                
)
summary(arvore_variaveis_arvore_Teste)

Tempo_Final <- Sys.time()

Tempo_Processamento <- Tempo_Final - Tempo_Inicial


#Probabilidade de ataques 
#Treino
Probabilidade_Treino = predict(arvore_PCA_Treino, Base_Treino)
#Teste
Probabilidade_Teste = predict(arvore_PCA_Teste, Base_Teste)

#Tentando transformar o objeto probabilidade em Matriz
Probabilidade_Treino. <- as.matrix(Probabilidade_Treino)
Probabilidade_Teste. <- as.matrix(Probabilidade_Teste)

#Classificação 
Classificacao_Treino = Probabilidade_Treino.[,1]>.5
Classificacao_Teste = Probabilidade_Teste.[,1]>.5

#Matriz de confusão
Tabela_Treino <- table(Classificacao_Treino, Base_Treino$label)
Tabela_Teste <- table(Classificacao_Teste, Base_Teste$label)

#Acurácia 
Acuracia <- (Tabela_Treino[1,1]+Tabela_Treino[2,2])/nrow(Base_Treino)
sprintf("Acurácia na base de Teste: %s", percent(Acuracia))

Acuracia <- (Tabela_Teste[1,1]+Tabela_Teste[2,2])/nrow(Base_Teste)
sprintf("Acurácia na base de Teste: %s", percent(Acuracia))

