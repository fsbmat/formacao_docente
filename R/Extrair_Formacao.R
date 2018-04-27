rm(list=ls())
cat("\014")
library(tidyverse)
library(xml2)
library(janitor)
library(data.table)
library(writexl)
library(pbapply)
library(purrr)

#### INPUTS MANUAIS ####
# definir pasta dos arquivos R - Linux
#pasta_codigos <- "/home/sillas/Dropbox/R-Lattes/R"
# definir pasta onde os xml estao salvos
#pasta_input <- "/home/sillas/Dropbox/R-Lattes/"
# definir pasta onde os csvs serao salvos
#pasta_output <- "/home/sillas/Dropbox/R-Lattes/output/"

#### INPUTS MANUAIS WINDOWS ####
# definir pasta dos arquivos R
pasta_codigos <- "E:/Documentos/GitHub/Servidores/Extrair_lattes/R"
# definir pasta onde os xml estao salvos
pasta_input <- "E:\\Documentos\\GitHub\\Servidores\\Extrair_lattes\\Lattes_XML"
# definir pasta onde os csvs serao salvos
pasta_output <- "E:\\Documentos\\GitHub\\Servidores\\Extrair_lattes\\output"

#### Rodar script ####
# carregar funcoes definidas em scripts

scripts <- dir(pasta_codigos, full.names = TRUE, pattern = "extrair")

for (script in scripts) {
  source(script)
}

arquivos <- dir(pasta_input, full.names = TRUE, pattern = "*.xml")
n <- length(arquivos)

df_lattes <- vector("list", n)

# criar funcao para encapsular o codigo abaixo
extrair_lattes_UFV <- function(arquivo_xml){
  lat_doc <- read_xml(arquivo_xml)
  #9772451905214345
  # extrair id, nome e endereco
  atributos <- extrair_id(lat_doc)
  nome <- extrair_nome_completo(lat_doc)
  ender_prof <- extrair_endereco_profissional(lat_doc)
  form_prof <- extrair_formacao_professor(lat_doc)
  #FORMACAO-ACADEMICA-TITULACAO
  # extrair id do lattes
  id_col <- "numero_identificador"
  id <- atributos[[id_col]]
  
  # extrair artigos apenas se o lattes for da UFV
  uni <- unique(ender_prof[["nome_instituicao_empresa"]])
  
  #### se uni nao for UFV, quebrar o loop
  if (is.null(uni) || !str_detect(uni, "Universidade Federal de Viçosa")){
    return(NA)
  } 
  ### 
  
  #formacao <- 
  
  ###
  
  artigos <- extrair_todos_artigos(lat_doc)
  if (ncol(artigos) > 0) {
    artigos <- clean_names(artigos)
    # criar identificador nas colunas referentes aos artigos
    names(artigos) <- paste0("art_", names(artigos))
    artigos[[id_col]] <- id
  }
  
  # adicionar id do lattes em todos os dataframe para os juntar em um so
  nome <- data.frame(nome_completo = nome, stringsAsFactors = FALSE)
  nome[[id_col]] <- id
  
  ender_prof <- named_vector_to_df(ender_prof)
  ender_prof[[id_col]] <- id 
  
  form_prof <- named_vector_to_df(form_prof)
  form_prof[[id_col]] <- id 
  
  
  df_loop <- left_join(nome, atributos, by = id_col)
  df_loop <- left_join(df_loop, ender_prof, by = id_col)
  df_loop <- left_join(df_loop, form_prof, by = id_col)
  if (ncol(artigos) > 0) df_loop <- left_join(df_loop, artigos, by = id_col)
  
  # adicionar coluna de nome de arquivo
  df_loop$nome_arquivo <- basename(arquivo_xml)
  # salvar dataframe
  
  arq_output <- paste0(pasta_output, basename(arquivo_xml))
  arq_output <- str_replace(arq_output, ".xml", ".csv")
  
  message(paste0("Salvando arquivo ", arq_output))
  
  write.csv2(df_loop, arq_output, row.names = FALSE, fileEncoding = "ISO-8859-1")
  return(df_loop)
  
}

safe_extrair_lattes_UFV <- function(arquivo_xml){
  x <- try(extrair_lattes_UFV(arquivo_xml), silent = TRUE)
  if (inherits(x, "try-error")) return(NULL) else return(x)
}

lst <- pblapply(arquivos, safe_extrair_lattes_UFV)


# for (i in 1:n){
#   # mostrar progresso de 100 em 100
#   if (i %% 100 == 0) print(i)
#   lat_doc <- read_xml(arquivos[i])
#   
#   # extrair id, nome e endereco
#   atributos <- extrair_id(lat_doc)
#   nome <- extrair_nome_completo(lat_doc)
#   ender_prof <- extrair_endereco_profissional(lat_doc)
#   
#   # extrair id do lattes
#   id_col <- "numero_identificador"
#   id <- atributos[[id_col]]
#   
#   # extrair artigos apenas se o lattes for da UFV
#   uni <- unique(ender_prof[["nome_instituicao_empresa"]])
#   
#   #### se uni nao for UFV, quebrar o loop
#   if (is.null(uni) || uni != "Universidade Federal de Alagoas") next
#   ### 
#   
#   artigos <- extrair_todos_artigos(lat_doc)
#   if (ncol(artigos) > 0) {
#     artigos <- clean_names(artigos)
#     # criar identificador nas colunas referentes aos artigos
#     names(artigos) <- paste0("art_", names(artigos))
#     artigos[[id_col]] <- id
#     }
#     
#   # adicionar id do lattes em todos os dataframe para os juntar em um so
#   nome <- data.frame(nome_completo = nome, stringsAsFactors = FALSE)
#   nome[[id_col]] <- id
#   
#   ender_prof <- named_vector_to_df(ender_prof)
#   ender_prof[[id_col]] <- id 
#   
#   
#   df_loop <- left_join(nome, atributos, by = id_col)
#   df_loop <- left_join(df_loop, ender_prof, by = id_col)
#   if (ncol(artigos) > 0) df_loop <- left_join(df_loop, artigos, by = id_col)
#   
#   # adicionar coluna de nome de arquivo
#   df_loop$nome_arquivo <- basename(arquivos[i])
#   # salvar dataframe
#   
#   arq_output <- paste0(pasta_output, basename(arquivos[i]))
#   arq_output <- str_replace(arq_output, ".xml", ".csv")
#   
#   write.csv2(df_loop, arq_output, row.names = FALSE, fileEncoding = "ISO-8859-1")
#   
# }

# juntar todos os csvs individuais em um so e salvar em xlsx
arq_output <- dir(pasta_output, full.names = TRUE, pattern = "*.csv")

df_lattes <- map(arq_output, fread, sep = ';', dec = ',')
df_lattes <- rbindlist(df_lattes, fill = TRUE)

write_xlsx(df_lattes, paste0(pasta_output, "todos_juntos.xlsx"))


