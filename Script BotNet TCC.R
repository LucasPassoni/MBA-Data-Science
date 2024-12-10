library("tidyverse")
library("dplyr")
library("correlation")
library("rpart")
library(readr)
BaseBotNet <- read_csv("BoTNeTIoT-L01-v2.csv")
summarise(BaseBotNet)
summary(BaseBotNet)

unique(BaseBotNet$Attack)
unique(BaseBotNetMirai$Device_Name)
count(BaseBotNetMirai$Attack_subType) #transformandoa esse char em int, da pra contar quais tipos de sub ataques tiveram mais sucesso em quais dispositivos


#Separa os ataques Mirai
BaseBotNet %>% filter(Attack == "mirai") 
BaseBotNetMirai <- BaseBotNet %>% filter(Attack == "mirai") 
summary(BaseBotNetMirai)
str(BaseBotNetMirai)

remove(BoTNeTIoT_L01_v2)
remove(BaseBotNet)

#quantidade de subAtaques
View(BaseBotNetMirai$Attack_subType)

unique(BaseBotNetMirai$Attack_subType)

nrow(BaseBotNetMirai[BaseBotNetMirai$Attack_subType == "ack",]) # = 643821
nrow(BaseBotNetMirai[BaseBotNetMirai$Attack_subType == "scan",]) # = 537979
nrow(BaseBotNetMirai[BaseBotNetMirai$Attack_subType == "syn",]) # = 733299
nrow(BaseBotNetMirai[BaseBotNetMirai$Attack_subType == "udp",]) # = 1229999
nrow(BaseBotNetMirai[BaseBotNetMirai$Attack_subType == "udpplain",]) # = 523304

#relacionar os dados de tipos de dispositivos com a quantidade de subataques

head(BaseBotNetMirai)
BaseBotNetnoduplicates <- BaseBotNetMirai %>% distinct( 
                                                       MI_dir_L0.1_weight,
                                                       MI_dir_L0.1_mean,
                                                       MI_dir_L0.1_variance,
                                                       H_L0.1_weight,
                                                       H_L0.1_mean,
                                                       H_L0.1_variance,
                                                       HH_L0.1_weight,
                                                       HH_L0.1_mean,
                                                       HH_L0.1_std,
                                                       HH_L0.1_magnitude,
                                                       HH_L0.1_radius,
                                                       HH_L0.1_covariance,
                                                       HH_L0.1_pcc,
                                                       HH_jit_L0.1_weight,
                                                       HH_jit_L0.1_mean,
                                                       HH_jit_L0.1_variance,
                                                       HpHp_L0.1_weight,
                                                       HpHp_L0.1_mean,
                                                       HpHp_L0.1_std,
                                                       HpHp_L0.1_magnitude,
                                                       HpHp_L0.1_radius,
                                                       HpHp_L0.1_covariance,
                                                       HpHp_L0.1_pcc,
                                                       Device_Name,
                                                       Attack,
                                                       Attack_subType,
                                                       label, .keep_all = TRUE)



remove(BaseBotNetnoduplicates)

BaseBotNetnoduplicates <- BaseBotNetMirai %>% distinct(MI_dir_L0.1_weight, .keep_all = TRUE)

BaseBotNetnoduplicates <- BaseBotNetMirai %>% distinct(BaseBotNetMirai)


#renomear as variáveis da base 
BaseBotNetMirai1 <- rename(BaseBotNetMirai, 
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

#para calcular a correlação entre os dados vou retirar as variáveis que são caracteres
#e verificar se em todo a base existem números faltantes

cor(BaseBotNetMirai1) #a funcao cor retornou um valor "x", estou colocando a função na.fail pra verificar
                     #se tem algum outro valor 

na.fail(BaseBotNetMirai1) #não tem valores 

#calculo correlacoes

BaseCalculoCorrelacao <- BaseBotNetMirai1[,1:23]
summary(BaseCalculoCorrelacao)
MatrizCorrelacao <- cor(BaseCalculoCorrelacao)

########PARA LOCALIZAR AS CORRELAÇÕES SEM A FUNÇÃO FINDCORRELATION

Menores_Correlacoes <- which(abs(Matriz_Correlacao) < 0.7 & Matriz_Correlacao != 1, arr.ind = TRUE)

Menores_Correlacoes_Coluna <- unique(Menores_Correlacoes[, 2])

Matriz_Menores_Correlacoes_Teste <- Matriz_Menores_Correlacoes[, -Menores_Correlacoes_Coluna]

DF_Menores_Correlacoes_Teste <- as.data.frame(Matriz_Menores_Correlacoes_Teste)

Matriz_Menores_Correlacoes <- Matriz_Correlacao[Menores_Correlacoes[,1], Menores_Correlacoes[,2]]

#para transformar os nomes 
colnames(DF_Menores_Correlacoes_Teste) <- make.names(colnames(DF_Menores_Correlacoes_Teste))

#Transformar o vetor em data frame
DF_Menores_Correlacoes <- Matriz_Menores_Correlacoes[, unique(Matriz_Menores_Correlacoes[,2])]
Matriz_Menores_Correlacoes <- findCorrelation(DF_Menores_Correlacoes, cutoff = 0.7)


#plot correlacoes

install.packages("corrplot")
library(corrplot)

corrplot(MatrizCorrelacao, type = "lower", method = "number")

#calcula matriz de correlação e uma matriz de valor P

install.packages("Hmisc")

library(Hmisc)

MatrizCorrelacaoEPvalor <- rcorr(as.matrix(BaseCalculoCorrelacao))
View(MatrizCorrelacaoEPvalor)
#grafico 
corrplot(MatrizCorrelacaoEPvalor$r, p.mat = MatrizCorrelacaoEPvalor$p, sig.level = 0.005, method="color", type="lower")

chart.Correlation(BaseCalculoCorrelacao)
#retiradas as variáveis 

#Arvore de decisão com todas as variáveis
#depois vou rodar um modelo de árvore de decisão sem as variáveis zeradas. 

dados_tranformados <- model.matrix(~ ., data = BaseBotNetMirai1[, -27])

#verificando levels da variável da categórica
levels(BaseBotNetMirai1$label)

#transformando as variáveis caracter em factor, categórica 
BaseBotNetMirai1$Device_Name <- factor(BaseBotNetMirai1$Device_Name)
BaseBotNetMirai1$Attack <- factor(BaseBotNetMirai1$Attack)
BaseBotNetMirai1$Attack_subType <- factor(BaseBotNetMirai1$Attack_subType)
BaseBotNetMirai1$label <- factor(BaseBotNetMirai1$label)

#tentando transformar variáveis fatores em numericas
#primeiro eu pensei que precisava estar como factor as variáveis char
#mas o chat GPT ta sugerindo trocar pra num, então vou tentar. 
#testei valores em branco ou faltantes, mas a funcao any(is.na) retornou false. 

BaseBotNetMirai1$Device_Name <- as.numeric(BaseBotNetMirai1$Device_Name)
BaseBotNetMirai1$Attack <- as.numeric(BaseBotNetMirai1$Attack)
BaseBotNetMirai1$Attack_subType <- as.numeric(BaseBotNetMirai1$Attack_subType)
BaseBotNetMirai1$label <- factor(BaseBotNetMirai1$label, levels = c("attack", "normal"))
BaseBotNetMirai1$label <- ifelse(BaseBotNetMirai1$label == 0, "attack", "normal")
BaseBotNetMirai1$label <- as.numeric(BaseBotNetMirai1$label)

as.vector(BaseBotNetMirai1, mode - "")

#agora pode ser que tenha alguns valores faltantes na variável Label
#eu não sei como isso é possível, se ela tem duas classes, mas ok vou tentar
#colocar a função que o chat sugeriu 

any(is.na(BaseBotNetMirai1$label)) #retornou true, depois da transformação em 2 leveis

BaseBotNetMirai1 <- BaseBotNetMirai

BaseBotNetMiraiSemNA <- BaseBotNetMirai1[complete.cases(BaseBotNetMirai1$label)]
BaseBotNetMiraiSemNA <- BaseBotNetMirai1[complete.cases(BaseBotNetMirai1), ]

train_test_split_index <- round(0.8 * nrow(BaseBotNetMirai1))


treino <- data.frame(BaseBotNetMirai1[1:train_test_split_index,])
teste <- data.frame(BaseBotNetMirai1[(train_test_split_index+1): nrow(BaseBotNetMirai1),])


set.seed(123)

arvore <- rpart(label ~ H_weight + H_mean + H_variance + HH_weight + HH_mean + HH_std + HH_magnitude + HH_radius + HH_covariance + HH_pcc + HH_jit_weight + HH_jit_mean + HH_jit_variance + HpHp_weight + HpHp_mean + HpHp_std + HpHp_magnitude + HpHp_radius + HpHp_covariance + HpHp_pcc + Device_Name + Attack + Attack_subType,
                data= BaseBotNetMirai1,
                parms = list(split = 'gini'), # podemos trocar para  'information'
                method='class' # Essa opção indica que a resposta é qualitativa
)

#para variáveis contínuas 
arvore <- rpart(label ~ H_weight + H_mean + H_variance + HH_weight + HH_mean + HH_std + HH_magnitude + HH_radius + HH_covariance + HH_pcc + HH_jit_weight + HH_jit_mean + HH_jit_variance + HpHp_weight + HpHp_mean + HpHp_std + HpHp_magnitude + HpHp_radius + HpHp_covariance + HpHp_pcc + Device_Name + Attack + Attack_subType,
                data= BaseBotNetMirai1,
                control = rpart.control(maxdepth = 2, cp = 0.0001)
                
)

any(is.na(BaseBotNetMirai1))


##AGORA VOU RETIRAR AS VARIÁVEIS COM CORRELAÇÃO MENORES DO QUE 0.6
#E RODAR A PRIMEIRA ÁRVORE
Matriz_maiores_correlacoes <- which(MatrizCorrelacao >= 0.6, arr.ind = TRUE, useNames = TRUE)

Matriz_maiores_correlacoes1 <- BaseCalculoCorrelacao[, -unique(Matriz_maiores_correlacoes[,"col"])]

##tentando outra função 
library(caret)
Matriz_maiores_correlacoes <- findCorrelation(MatrizCorrelacao, cutoff = 0.6, verbose = FALSE, names = FALSE)
  
corrplot(Matriz_maiores_correlacoes, type = "lower", method = "number")

sum(BaseBotNet$label == 1, na.rm = TRUE)


##o dataset não ta balanceado, todas as respostas da variavel label estao como 0
#ou seja, ataque, então nesse caso todo tráfego de rede do mirai é de ataques
#então agora eu vou rodar o modelo SMOTE (Synthetic Minority Over-sampling Technique)
#pra balancear o data set 
install.packages("DMwR2")
library(DMwR2)


Da