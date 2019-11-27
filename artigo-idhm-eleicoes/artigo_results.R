################################################
##
## Carga dos tidy datasets
##
################################################
candidatos_dep <- read.csv('candidatos_dep.csv')
candidatos_dep$ano <- as.factor(candidatos_dep$ano)
candidatos_dep$eleito <- as.factor(candidatos_dep$eleito)

idhm <- read.csv('idhm.csv')

idhm_mulheres <- read.csv('idhm_eleicao.csv')

################################################
##
## Subsets para análise
##
################################################
candidatos_dep2010 <- subset(candidatos_dep, ano==2010)
idhm2010_mulheres <- subset(idhm_mulheres, ano==2010)

obter_candidatos_dep_mulheres <- function(ano_eleicao) {
  ano_key <- as.character(ano_eleicao)
  candidatos_dep <- subset(candidatos_dep, ano==ano_eleicao)
  tbl_part_cand <- table(candidatos_dep$ano, candidatos_dep$uf, candidatos_dep$sexo)
  tbl_part <- table(candidatos_dep$ano, candidatos_dep$uf, candidatos_dep$sexo, candidatos_dep$eleito)
  candidatos_dep_mulheres <- as.data.frame(cbind(
    prop.table(tbl_part_cand[ano_key,,], 1)[,'FEMININO'],
    prop.table(tbl_part[ano_key,,,'1'], 1)[,'FEMININO'],
    tbl_part_cand[ano_key,,][,'FEMININO'],
    tbl_part[ano_key,,,'1'][,'FEMININO']
  ))
  colnames(candidatos_dep_mulheres) <- c('perc_mulheres_candidatas', 'perc_mulheres_eleitas', 'qtd_mulheres_candidatas', 'qtd_mulheres_eleitas')
  candidatos_dep_mulheres$uf <- as.factor(rownames(candidatos_dep_mulheres))
  candidatos_dep_mulheres$ano = as.factor(ano_eleicao)
  candidatos_dep_mulheres[is.nan(candidatos_dep_mulheres$perc_mulheres_candidatas), 'perc_mulheres_candidatas'] <- 0
  candidatos_dep_mulheres[is.nan(candidatos_dep_mulheres$perc_mulheres_eleitas), 'perc_mulheres_eleitas'] <- 0
  candidatos_dep_mulheres$perc_aprovacao_mulheres = candidatos_dep_mulheres$qtd_mulheres_eleitas / candidatos_dep_mulheres$qtd_mulheres_candidatas
  candidatos_dep_mulheres[is.nan(candidatos_dep_mulheres$perc_aprovacao_mulheres), 'perc_aprovacao_mulheres'] <- 0
  candidatos_dep_mulheres
}

obter_candidatos_dep_mulheres_part <- function(ano_eleicao) {
  candidatos_dep <- subset(candidatos_dep, ano==ano_eleicao)
  ano_key <- as.character(ano_eleicao)
  tbl_part_cand <- table(candidatos_dep$ano, candidatos_dep$partido, candidatos_dep$uf, candidatos_dep$sexo)
  tbl_part <- table(candidatos_dep$ano, candidatos_dep$partido, candidatos_dep$uf, candidatos_dep$sexo, candidatos_dep$eleito)
  candidatos_dep_mulheres_part1 <- merge(
    as.data.frame(prop.table(tbl_part_cand[ano_key,,,], 1:2)[,,'FEMININO']),
    as.data.frame(prop.table(tbl_part[ano_key,,,,'1'], 1:2)[,,'FEMININO']),
    by.x = c(1, 2), by.y = c(1, 2)
  )
  candidatos_dep_mulheres_part2 <- merge(
    as.data.frame(tbl_part_cand[ano_key,,,][,,'FEMININO']),
    as.data.frame(tbl_part[ano_key,,,,'1'][,,'FEMININO']),
    by.x = c(1, 2), by.y = c(1, 2)
  )
  candidatos_dep_mulheres_part <- merge(candidatos_dep_mulheres_part1, candidatos_dep_mulheres_part2, by.x = c(1, 2), by.y = c(1, 2))
  colnames(candidatos_dep_mulheres_part) <- c('partido', 'uf', 'perc_mulheres_candidatas', 'perc_mulheres_eleitas', 'qtd_mulheres_candidatas', 'qtd_mulheres_eleitas')
  candidatos_dep_mulheres_part$ano = as.factor(ano_eleicao)
  candidatos_dep_mulheres_part[is.nan(candidatos_dep_mulheres_part$perc_mulheres_candidatas), 'perc_mulheres_candidatas'] <- 0
  candidatos_dep_mulheres_part[is.nan(candidatos_dep_mulheres_part$perc_mulheres_eleitas), 'perc_mulheres_eleitas'] <- 0
  candidatos_dep_mulheres_part$perc_aprovacao_mulheres = candidatos_dep_mulheres_part$qtd_mulheres_eleitas / candidatos_dep_mulheres_part$qtd_mulheres_candidatas
  candidatos_dep_mulheres_part[is.nan(candidatos_dep_mulheres_part$perc_aprovacao_mulheres), 'perc_aprovacao_mulheres'] <- 0
  candidatos_dep_mulheres_part
}

candidatos_dep_mulheres <- rbind(obter_candidatos_dep_mulheres(2010), obter_candidatos_dep_mulheres(2014), obter_candidatos_dep_mulheres(2018))
candidatos_dep_mulheres_part <- rbind(obter_candidatos_dep_mulheres_part(2010), obter_candidatos_dep_mulheres_part(2014), obter_candidatos_dep_mulheres_part(2018))

################################################
##
## Análise descritiva - UF
##
################################################
library(ggplot2)
ggplot(candidatos_dep, aes(uf, fill = ano)) + geom_bar(position = "dodge")
ggplot(candidatos_dep, aes(uf, fill = sexo)) + geom_bar()

################################################
##
## Análise descritiva - idade
##
################################################
ggplot(candidatos_dep, aes(idade, fill=ano)) + geom_density(alpha = 0.2)
ggplot(subset(candidatos_dep, sexo=='FEMININO'), aes(idade, fill=ano)) + geom_density(alpha = 0.2)
ggplot(subset(candidatos_dep, sexo!='FEMININO'), aes(idade, fill=ano)) + geom_density(alpha = 0.2)
summary(candidatos_dep$idade)
subset(candidatos_dep, idade < 10 | idade > 90, select=c(ano, uf, cargo, nome, dt_nasc, idade, eleito))

################################################
##
## Análise descritiva
##     - percentual mulheres candidatas (*)
##
################################################
ggplot(candidatos_dep_mulheres, aes(uf, perc_mulheres_candidatas, color = ano)) + geom_point()

################################################
##
## Análise descritiva (*)
##     - percentual mulheres eleitas
##
################################################
ggplot(candidatos_dep_mulheres, aes(uf, perc_mulheres_eleitas, color = ano, size = 1)) + geom_point()

################################################
##
## Análise descritiva
##     - percentual aprovação mulheres
##
################################################
ggplot(candidatos_dep_mulheres, aes(uf, perc_aprovacao_mulheres, color = ano, size = 1)) + geom_point()

################################################
##
## Análise descritiva
##     - UF x IDHM
##     - UF x Perc.Candidatas
##
################################################
par(mfrow=c(2,1))
plot(idhm$uf, idhm$idhm, main='UF x IDHM')
plot(candidatos_dep_mulheres$uf, candidatos_dep_mulheres$perc_mulheres_candidatas, main='UF x Perc.Candidatas')

################################################
##
## Análise descritiva
##     - UF x IDHM
##     - UF x Perc.Eleitas
##
################################################
par(mfrow=c(2,1))
plot(idhm$uf, idhm$idhm, main='UF x IDHM')
plot(candidatos_dep_mulheres$uf, candidatos_dep_mulheres$perc_mulheres_eleitas, main='UF x Perc.Eleitas')

################################################
##
## Análise descritiva
##     - UF x IDHM
##     - UF x Perc.Eleitas
##
################################################
par(mfrow=c(2,1))
plot(idhm$uf, idhm$idhm, main='UF x IDHM')
plot(candidatos_dep_mulheres$uf, candidatos_dep_mulheres$perc_aprovacao_mulheres, main='UF x Perc.Aprov.Feminina')

################################################
##
## Regressão linear
##    - perc_mulheres_candidatas ~ IDHM
##
################################################
ggplot(idhm2010_mulheres, aes(x = idhm, y = perc_mulheres_candidatas)) + geom_point() + geom_smooth(method = "lm")

################################################
##
## Regressão linear
##    - perc_mulheres_eleitas ~ IDHM
##
################################################
ggplot(idhm2010_mulheres, aes(x = idhm, y = perc_mulheres_eleitas)) + geom_point() + geom_smooth(method = "lm")
fit <- lm(perc_mulheres_eleitas ~ idhm + idhm_renda + idhm_longevidade + idhm_educacao, data = idhm2010_mulheres)
summary(fit)

################################################
##
## Regressão linear
##    - perc_aprovacao_mulheres ~ IDHM
##
################################################
ggplot(idhm2010_mulheres, aes(x = idhm, y = perc_aprovacao_mulheres)) + geom_point() + geom_smooth(method = "lm")

################################################
##
## Experimentos com partido
##
################################################

formulas_eleicao <- c(
  'perc_mulheres_eleitas ~ perc_mulheres_candidatas',
  'perc_mulheres_eleitas ~ partido',
  'perc_mulheres_eleitas ~ uf',
  'perc_mulheres_eleitas ~ partido + uf',
  'perc_mulheres_eleitas ~ perc_mulheres_candidatas + partido',
  'perc_mulheres_eleitas ~ perc_mulheres_candidatas + uf',
  'perc_mulheres_eleitas ~ perc_mulheres_candidatas + partido + uf'
)
linear_regression(formulas_eleicao, data=candidatos_dep_mulheres_part)

################################################
##
## Experimentos com IDHM
##
################################################

formulas_IDHM <- c(
  'perc_mulheres_eleitas ~ perc_mulheres_candidatas',
  'perc_mulheres_eleitas ~ idhm',
  'perc_mulheres_eleitas ~ idhm_renda',
  'perc_mulheres_eleitas ~ idhm_longevidade',
  'perc_mulheres_eleitas ~ idhm_educacao',
  'perc_mulheres_eleitas ~ idhm_renda + idhm_longevidade',
  'perc_mulheres_eleitas ~ idhm_renda + idhm_educacao',
  'perc_mulheres_eleitas ~ idhm_longevidade + idhm_educacao',
  'perc_mulheres_eleitas ~ idhm_renda + idhm_longevidade + idhm_educacao',
  'perc_mulheres_eleitas ~ perc_mulheres_candidatas + idhm',
  'perc_mulheres_eleitas ~ perc_mulheres_candidatas + idhm_renda',
  'perc_mulheres_eleitas ~ perc_mulheres_candidatas + idhm_longevidade',
  'perc_mulheres_eleitas ~ perc_mulheres_candidatas + idhm_educacao',
  'perc_mulheres_eleitas ~ perc_mulheres_candidatas + idhm_renda + idhm_longevidade',
  'perc_mulheres_eleitas ~ perc_mulheres_candidatas + idhm_renda + idhm_educacao',
  'perc_mulheres_eleitas ~ perc_mulheres_candidatas + idhm_longevidade + idhm_educacao',
  'perc_mulheres_eleitas ~ perc_mulheres_candidatas + idhm_renda + idhm_longevidade + idhm_educacao',
  'perc_aprovacao_mulheres ~ idhm',
  'perc_aprovacao_mulheres ~ idhm_renda',
  'perc_aprovacao_mulheres ~ idhm_longevidade',
  'perc_aprovacao_mulheres ~ idhm_educacao',
  'perc_aprovacao_mulheres ~ idhm_renda + idhm_longevidade',
  'perc_aprovacao_mulheres ~ idhm_renda + idhm_educacao',
  'perc_aprovacao_mulheres ~ idhm_longevidade + idhm_educacao',
  'perc_aprovacao_mulheres ~ idhm_renda + idhm_longevidade + idhm_educacao'
)

#######
## Procurar melhor modelo
#######

for (ano_eleicao in c(2010, 2014)) {
  cat('Eleições de', ano_eleicao)
  print(linear_regression(formulas_IDHM, data = subset(idhm_mulheres, ano == ano_eleicao)))
  cat('\n')
}
cat('Totas as eleições')
print(linear_regression(formulas_IDHM, data = idhm_mulheres))
cat('\n')
# melhor modelo 2010      : perc_aprovacao_mulheres ~ idhm_renda + idhm_longevidade + idhm_educacao 0.289785 0.045324
# melhor modelo 2014      : perc_aprovacao_mulheres ~ idhm_renda + idhm_longevidade + idhm_educacao 0.134348 0.335548
# melhor modelo 2010,2014 : perc_aprovacao_mulheres ~ idhm_renda + idhm_longevidade + idhm_educacao 0.217883 0.006112

#######
## Identificar grupo 2 - UFs que prejudicam o melhor modelo encontrado
#######

for (ano_eleicao in c(2010, 2014)) {
  cat('Eleições de', ano_eleicao)
  print(linear_regression_removing_uf('perc_aprovacao_mulheres ~ idhm_renda + idhm_longevidade + idhm_educacao', data = subset(idhm_mulheres, ano == ano_eleicao)))
  cat('\n')
}
cat('Totas as eleições')
print(linear_regression_removing_uf('perc_aprovacao_mulheres ~ idhm_renda + idhm_longevidade + idhm_educacao', data = idhm_mulheres))
cat('\n')
# Grupo 2 para 2010      : AL|SE|CE|RR|RS|DF|SC|AC|ES|MG|MA|RN|PE|MT|MS|PI  0.989386  0.000000
# Grupo 2 para 2014      : AP|PA|AL|AM|PB|RN|SC|DF|SP|TO|RR|CE|BA|PR|MT     0.965648  0.000003
# Grupo 2 para 2010+2014 : AL|PA|AM|SE|AP|TO|MS|MT|AC|RR|SC|SP|DF|RN        0.722770  0.000002

#######
## Procurar melhor modelo para os grupo 1 e 2
## considerando o grupo2 que otimiza o modelo no período 2010+2014:
##    - AL|PA|AM|SE|AP|TO|MS|MT|AC|RR|SC|SP|DF|RN
#######
grupo2_global <- '(AL|PA|AM|SE|AP|TO|MS|MT|AC|RR|SC|SP|DF|RN)'
for (ano_eleicao in c(2010, 2014)) {
  idhm_ano <- subset(idhm_mulheres, ano == ano_eleicao)
  cat('Eleições de', ano_eleicao, ' - melhor modelo para o grupo1:', '\n')
  print(linear_regression(formulas_IDHM, data = idhm_ano[!grepl(grupo2_global, idhm_ano$uf),], top = 1))
  cat('\nEleições de', ano_eleicao, ' - melhor modelo para o grupo2', grupo2, ':\n')
  print(linear_regression(formulas_IDHM, data = idhm_ano[grepl(grupo2_global, idhm_ano$uf),], top = 1))
  cat('\n')
}
# 2010
#   melhor modelo grupo 1: perc_aprovacao_mulheres ~ idhm_renda + idhm_longevidade + idhm_educacao 0.749334 0.004558
#   melhor modelo grupo 2: perc_aprovacao_mulheres ~ idhm_longevidade + idhm_educacao 0.202266 0.288549
# 2014
#  melhor modelo grupo 1: perc_aprovacao_mulheres ~ idhm_renda + idhm_longevidade + idhm_educacao 0.613884 0.029525
#  melhor modelo grupo 2: perc_mulheres_eleitas ~ perc_mulheres_candidatas + idhm_renda + idhm_longevidade + idhm_educacao 0.11507 0.875607

#######
## Procurar melhor modelo para os grupo 1 e 2
## considerando a interseção entre cada grupo2 que otimiza o modelo nos períodos 2010, 2014 e 2010+2014:
##    - AL|MT|RR|SC|DF|RN
# Grupo 2 para 2010      : *AL|-SE|-CE|*RR|-RS|*DF|*SC|-AC|ES|MG|MA|*RN|PE|*MT|-MS|PI  0.989386  0.000000
# Grupo 2 para 2014      : -AP|-PA|*AL|-AM|PB|*RN|*SC|*DF|-SP|-TO|*RR|-CE|BA|PR|*MT     0.965648  0.000003
# Grupo 2 para 2010+2014 : *AL|-PA|-AM|-SE|-AP|-TO|-MS|*MT|-AC|*RR|*SC|-SP|*DF|*RN        0.722770  0.000002


#######
grupo2_global <- '(AL|MT|RR|SC|DF|RN)'
for (ano_eleicao in c(2010, 2014)) {
  idhm_ano <- subset(idhm_mulheres, ano == ano_eleicao)
  cat('Eleições de', ano_eleicao, ' - melhor modelo para o grupo1:', '\n')
  print(linear_regression(formulas_IDHM, data = idhm_ano[!grepl(grupo2_global, idhm_ano$uf),], top = 1))
  cat('\nEleições de', ano_eleicao, ' - melhor modelo para o grupo2', grupo2, ':\n')
  print(linear_regression(formulas_IDHM, data = idhm_ano[grepl(grupo2_global, idhm_ano$uf),], top = 1))
  cat('\n')
}
# 2010
#   melhor modelo grupo 1: perc_mulheres_eleitas ~ perc_mulheres_candidatas + idhm_renda + idhm_longevidade + idhm_educacao 0.464421 0.031923
#   melhor modelo grupo 2: perc_mulheres_eleitas ~ perc_mulheres_candidatas + idhm_renda + idhm_longevidade + idhm_educacao 0.493584 0.887253
# 2014
#  melhor modelo grupo 1: perc_aprovacao_mulheres ~ idhm_renda + idhm_longevidade + idhm_educacao 0.167045 0.362524
#  melhor modelo grupo 2: perc_mulheres_eleitas ~ perc_mulheres_candidatas + idhm_renda + idhm_longevidade + idhm_educacao 0.637021 0.794373


#######
## Ano 2014
## Procurar melhor modelo para os grupo 1 e 2
#######
ano_eleicao <- 2014
idhm_ano <- subset(idhm_mulheres, ano == ano_eleicao)
grupo2 <- '(AP|PA|AL|AM|PB|RN|SC|DF|SP|TO|RR|CE|BA|PR|MT)'
cat('Eleições de', ano_eleicao, '\n')
print('Melhor modelo para o grupo1')
linear_regression(formulas_IDHM, data = idhm_ano[!grepl(grupo2, idhm_ano$uf),], top = 1)
cat('Melhor modelo para o grupo2:', grupo2, '\n')
linear_regression(formulas_IDHM, data = idhm_ano[grepl(grupo2, idhm_ano$uf),], top = 1)
# 2014
#   melhor modelo grupo 1: perc_aprovacao_mulheres ~ idhm_renda + idhm_longevidade + idhm_educacao 0.989386 0.000000
#   melhor modelo grupo 2: perc_aprovacao_mulheres ~ idhm_renda + idhm_longevidade + idhm_educacao 0.236579 0.338453

################################################
##
## Sugestão para trabalhos futuros
##     - acrescentar idade no subset IDHM
##
################################################
