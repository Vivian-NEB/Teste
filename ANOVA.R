
######################### ANOVA de uma via #########################


# Passo 1: Carregar os pacotes usados

if(!require(dplyr)) install.packages("dplyr")
library(dplyr)                                
if(!require(RVAideMemoire)) install.packages("RVAideMemoire") 
library(RVAideMemoire)                                        
if(!require(car)) install.packages("car")   
library(car)                                
if(!require(psych)) install.packages("psych") 
library(psych)                                
if(!require(rstatix)) install.packages("rstatix") 
library(rstatix)                                
if(!require(DescTools)) install.packages("DescTools") 
library(DescTools)
if(!require(dplyr)) install.packages("haven")
library(haven)

# Diretório de trabalho
setwd("C:/Users/HANEB01/Downloads")

# Banco de dados

dados <- read_sav('Exercicio_pratico01 (peso x unidade).sav') 
View(dados) # visualização dos dados
str(dados) # visualização da estrutura dos dados
dados_f <- haven::as_factor(dados) # convertendo variáveis com rótulos em fatores 
str(dados_f) # visualização da estrutura dos dados novamente    

# Verificando os pressupostos
# Normalidade dos dados
#Shapiro por grupo (pacote RVAideMemoire)

byf.shapiro(peso ~ unidade, dados_f) # testando a normalidade por grupo. Obs: poderíamos utilizar a biblioteca shapiro.test no entando ela não testa a normalidade por grupos.

## Outra opção para verificar normalidade: pelo pacote rstatix:
dados_f %>% 
  group_by(unidade) %>% 
  shapiro_test(peso)

# Homogeneidade de variâncias
## Teste de Levene (pacote car)
leveneTest(peso ~ unidade, dados_f, center = mean)

# Observação:
# Por default, o teste realizado pelo pacote car tem como base a mediana (median)
# O teste baseado na mediana é mais robusto
# Baseado na média (comparável ao SPSS)


# Presença de outliers (por grupo) - Pacotes dplyr e rstatix
dados_f %>% 
  group_by(unidade) %>% 
  identify_outliers(peso)

## Pelo boxplot:
boxplot(peso ~ unidade, data = dados_f,
        ylab = "Peso (kg)", xlab = "Unidade")

# ANOVA
## Modelo
anova_peso <- aov(peso ~ unidade, dados_f)
summary(anova_peso) # para imprimir os resultados

# Análise post-hoc - Pacote DescTools
# Post-hocs permitidos: "hsd", "bonferroni", "lsd", "scheffe", "newmankeuls", "duncan"

# Uso do Duncan
PostHocTest(anova_peso, method = "duncan", conf.level = 0.95)

# Uso do TukeyHSD
PostHocTest(anova_peso, method = "hsd")

# Uso do Bonferroni
PostHocTest(anova_peso, method = "bonf")

## Resumindo em uma tabela mais de um post-hoc
round(
  cbind(duncan = PostHocTest(anova_peso, method="duncan")$unidade[,"pval"],
        bonf = PostHocTest(anova_peso, method="bonf")$unidade[,"pval"],
        hsd = PostHocTest(anova_peso, method="hsd")$unidade[,"pval"]),
  6)

# Análise descritiva dos dados
describeBy(dados_f$peso, group = dados_f$unidade)

#adicionar gráfico

