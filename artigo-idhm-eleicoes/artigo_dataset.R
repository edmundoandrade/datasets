###################################################
##
## Coleta de dados dos eleitos em 2010, 2014 e 2018
##
###################################################
install.packages("eeptools")
library(eeptools)
library(readr)
obter_candidatos_dep <- function(ano) {
  dt_eleicao <- as.Date(paste(ano, '10', '07', sep='-'))
  dt_eleicao <- dt_eleicao - as.numeric(format(dt_eleicao, "%w"))
  print(dt_eleicao)
  tse_file_name <- paste('consulta_cand_', ano, '.zip', sep='')
  tse_url <- paste('http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/', tse_file_name, sep='')
  if (!file.exists(tse_file_name)) download.file(tse_url, tse_file_name)
  files <- unzip(tse_file_name, list=TRUE)
  file_names <- files[grep("(\\.txt|\\.csv)", files$Name),][-grep("_BR", files$Name), 'Name']
  if (exists('candidatos_tidy')) rm(candidatos_tidy)
  for (file_name in file_names) {
    cat('\t', file_name)
    csv_file <- unz(tse_file_name, file_name)
    csv <- readLines(csv_file)
    close(csv_file)
    Encoding(csv) <- "latin1"
    if (endsWith(file_name,'.txt')) {
      # Corrigir linhas que apresentarem desbalanceamento de aspas (ex: "D"AVILA" -> "D'AVILA")
      csv <- gsub('([^;])"([^;])', '\\1\'\\2', csv)
      candidatos_uf <- read.csv(text=csv, sep=';', quote = '"', header = FALSE, stringsAsFactors = FALSE)
      candidatos_uf <- candidatos_uf[, c('V3', 'V6', 'V10', 'V11', 'V19', 'V27', 'V31', 'V43')]
      candidatos_uf$V27 <- with(candidatos_uf, parse_date(sub("-(0.)$", "-20\\1", sub("-([1-9].)$", "-19\\1", V27)), "%d-%b-%Y"))
    } else {
      candidatos_uf <- read.csv(text=csv, sep=';', quote = '"', header = TRUE, stringsAsFactors = FALSE)
      candidatos_uf <- candidatos_uf[, c('ANO_ELEICAO', 'SG_UF', 'DS_CARGO', 'NM_CANDIDATO', 'SG_PARTIDO', 'DT_NASCIMENTO', 'DS_GENERO', 'DS_SIT_TOT_TURNO')]
      candidatos_uf$DT_NASCIMENTO <- with(candidatos_uf, parse_date(DT_NASCIMENTO, "%d/%m/%Y"))
    }
    colnames(candidatos_uf) <- c('ano', 'uf', 'cargo', 'nome', 'partido', 'dt_nasc', 'sexo', 'resultado')
    candidatos_uf$idade <- with(candidatos_uf, age_calc(dob = dt_nasc, enddate = dt_eleicao, units = "years"))
    candidatos_uf$eleito <- grepl('^ELEITO', candidatos_uf$resultado)*1
    # Marcar como NA os dados não preenchidos ou que possuam valor inválido
    candidatos_uf[candidatos_uf$resultado == '#NULO#', 'resultado'] <- NA
    candidatos_uf[candidatos_uf$idade > 200, 'idade'] <- NA
    # Considerar somente cargos de Deputado Estadual ou Deputado Distrital
    candidatos_uf <- candidatos_uf[grep('^(DEPUTADO|DEPUTADO ESTADUAL|DEPUTADO DISTRITAL)$', candidatos_uf$cargo),]
    if (exists('candidatos_tidy')) {
      candidatos_tidy <- rbind(candidatos_tidy, candidatos_uf)
    } else {
      candidatos_tidy <- candidatos_uf
    }
  }
  cat('\n')
  candidatos_tidy
}

candidatos_dep_2010 <- obter_candidatos_dep(2010)
candidatos_dep_2014 <- obter_candidatos_dep(2014)
candidatos_dep_2018 <- obter_candidatos_dep(2018)

########################################
##
## Coleta de dados do IDHM de 2010
## (Página de ranking - site do PNUD)
##
########################################
library(rvest)

html_table_idhm <- tryCatch(
  {
    read_html('https://www.br.undp.org/content/brazil/pt/home/idh0/rankings/idhm-uf-2010.html') %>% html_table(fill=TRUE)
  },
  error = function(cond) {
    # O código abaixo substitui o código acima em ambientes de rede sob controle de proxy
    download.file('https://www.br.undp.org/content/brazil/pt/home/idh0/rankings/idhm-uf-2010.html', destfile="idhm-uf-2010.html", quiet=TRUE)
    print('Download alternativo executado.')
    html <- readChar('idhm-uf-2010.html', file.info('idhm-uf-2010.html')$size)
    Encoding(html) <- "UTF-8"
    read_html(html) %>% html_table(fill=TRUE)
  }
)

idhm_2010 <- as.data.frame(html_table_idhm)
idhm_2010$ano <- 2010
# Converter campo de UF para a sigla correspondente
DescricaoUFs <- c("ACRE", "ALAGOAS", "AMAZONAS", "AMAPÁ", "BAHIA", "CEARÁ", "DISTRITO FEDERAL", "ESPÍRITO SANTO", "GOIÁS", "MARANHÃO", 
                  "MINAS GERAIS", "MATO GROSSO DO SUL", "MATO GROSSO", "PARÁ", "PARAÍBA", "PERNAMBUCO", "PIAUÍ", "PARANÁ", "RIO DE JANEIRO", 
                  "RIO GRANDE DO NORTE", "RONDÔNIA", "RORAIMA", "RIO GRANDE DO SUL", "SANTA CATARINA", "SERGIPE", "SÃO PAULO", "TOCANTINS")
Siglas_UFs <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT",
                "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO")
idhm_2010$uf <- Siglas_UFs[match(toupper(idhm_2010$Unidade.da..Federação), DescricaoUFs)]
# Converter campos IDHM para o tipo numérico correspondente
idhm_2010$IDHM.2010 <- as.numeric(gsub(",", ".", idhm_2010$IDHM.2010))
idhm_2010$IDHM..Renda..2010 <- as.numeric(gsub(",", ".", idhm_2010$IDHM..Renda..2010))
idhm_2010$IDHM.Longevidade.2010 <- as.numeric(gsub(",", ".", idhm_2010$IDHM.Longevidade.2010))
idhm_2010$IDHM.Educação.2010 <- as.numeric(gsub(",", ".", idhm_2010$IDHM.Educação.2010))
# Redefinir colunas
idhm_2010 <- cbind(idhm_2010$ano, idhm_2010$uf, idhm_2010[3:6], stringsAsFactors = FALSE)
colnames(idhm_2010) <- c('ano', 'uf', 'idhm', 'idhm_renda', 'idhm_longevidade', 'idhm_educacao')
idhm_2010 <- idhm_2010[order(idhm_2010$uf),]

############################################
##
## Coleta de dados do IDHM de 2011 a 2015
## (Área de download do Atlas Brasil)
##
############################################
library("readxl")
atlas_url <- 'http://www.atlasbrasil.org.br/2013/data/rawData/Indicadores%20Atlas%20-%20RADAR%20IDHM.xlsx'
atlas_file_name <- 'Indicadores Atlas - RADAR IDHM.xlsx'
#download.file(atlas_url, atlas_file_name)
#download manual: http://www.atlasbrasil.org.br/2013/data/rawData/Indicadores%20Atlas%20-%20RADAR%20IDHM.xlsx
idhm_2011_2015 <- read_excel(atlas_file_name)
idhm_2011_2015 <- idhm_2011_2015[, c('ANO', 'NOME_AGREGA', 'IDHM', 'IDHM_R', 'IDHM_L', 'IDHM_E')]
colnames(idhm_2011_2015) <- c('ano', 'uf', 'idhm', 'idhm_renda', 'idhm_longevidade', 'idhm_educacao')
# Considerar somente o nível de UF
idhm_2011_2015 <- idhm_2011_2015[grep("^..$", idhm_2011_2015$uf),]
idhm_2011_2015 <- idhm_2011_2015[order(idhm_2011_2015$ano, idhm_2011_2015$uf),]

#################################################
##
## Geração dos tidy datasets:
##   - candidatos_dep.csv (2010, 2014 e 2018)
##   - idhm.csv (2010 a 2015)
##   - idhm_eleicao.csv (2010 e 2014)
##
#################################################

candidatos_dep <- rbind(candidatos_dep_2010, candidatos_dep_2014, candidatos_dep_2018)
write.csv(candidatos_dep,'candidatos_dep.csv', row.names=FALSE)

idhm <- rbind(idhm_2010, idhm_2011_2015)
write.csv(idhm,'idhm.csv', row.names=FALSE)

obter_idhm_mulheres <- function(ano_eleicao) {
  ano_key <- as.character(ano_eleicao)
  candidatos_dep <- subset(candidatos_dep, ano==ano_eleicao)
  tbl_cand <- table(candidatos_dep$ano, candidatos_dep$uf, candidatos_dep$sexo)
  tbl <- table(candidatos_dep$ano, candidatos_dep$uf, candidatos_dep$sexo, candidatos_dep$eleito)
  idhm_mulheres = cbind(subset(idhm, ano==ano_eleicao), perc_mulheres_candidatas=prop.table(tbl_cand[ano_key,,], 1)[,'FEMININO'])
  idhm_mulheres = cbind(idhm_mulheres, perc_mulheres_eleitas=prop.table(tbl[ano_key,,,'1'], 1)[,'FEMININO'])
  idhm_mulheres = cbind(idhm_mulheres, qtd_mulheres_candidatas=tbl_cand[ano_key,,][,'FEMININO'])
  idhm_mulheres = cbind(idhm_mulheres, qtd_mulheres_eleitas=tbl[ano_key,,,'1'][,'FEMININO'])
  idhm_mulheres$perc_aprovacao_mulheres = idhm_mulheres$qtd_mulheres_eleitas / idhm_mulheres$qtd_mulheres_candidatas
  idhm_mulheres
}

idhm_mulheres <- rbind(obter_idhm_mulheres(2010), obter_idhm_mulheres(2014))
write.csv(idhm_mulheres,'idhm_eleicao.csv', row.names=FALSE)
