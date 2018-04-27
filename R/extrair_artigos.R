library(xml2)
library(tidyverse)

extrair_artigo_metadados <- function(x, debug = FALSE){
  
  if (debug) browser()
  
  children_artigos <- x %>% xml_children() %>% xml_name()
  # extrair posicao do artigo na sequencia do dono do lattes
  id_sequencia <- x %>% xml_attr("SEQUENCIA-PRODUCAO")
  # extrair dados nao relacionados a autores
  extrair_node <- function(node) {
    res <- x %>% xml_child(node)
    
    if (inherits(res, "xml_missing")) {
      res <- data.frame()
    }  else {
      res <- res %>% xml_attrs()  %>% named_vector_to_df()
    }
    res
  }
  
  dados_basicos <- extrair_node("DADOS-BASICOS-DO-ARTIGO")
  detalhamentos <- extrair_node("DETALHAMENTO-DO-ARTIGO")
  palavras_chaves <- extrair_node("PALAVRAS-CHAVE")
  
  # puxar areas do conhecimento apenas se esse campo existir
  if ("AREAS-DO-CONHECIMENTO" %in% children_artigos){
    areas <- xml_child(x, "AREAS-DO-CONHECIMENTO")
    
    if (xml_length(areas) == 0) {
      areas <- data.frame()
      } else {
        areas <- xml_child(areas, 1)
        areas <- xml_attrs(areas)
        areas <- named_vector_to_df(areas)
        }
    } else {
      areas <- data.frame()
      }
  
  
  # extrair autores
  ind_autores <- which(children_artigos == "AUTORES")
  
  # inicializar lista vazia para armazenar autores
  df_autores <- vector("list", length(ind_autores))
  # iniciar loop para coletar autores
  for(i in seq_along(ind_autores)){
    df_autores[[i]] <- x %>%
      xml_child(ind_autores[i]) %>%
      xml_attrs() %>%
      named_vector_to_df()
  }
  df_autores <- bind_rows(df_autores)
  df_autores[["id_sequencia"]] <- id_sequencia
  
  # reunir tudo em um dataframe final
  df_final <- bind_cols(
    dados_basicos, detalhamentos, palavras_chaves, areas
  ) %>% 
    mutate(id_sequencia = id_sequencia) %>% 
    left_join(df_autores, by = 'id_sequencia')
  
  
  df_final
}

# iterar sobre todos os artigos
extrair_todos_artigos <- function(lattes_xml_document){

  artigos <- xml_child(lattes_xml_document, "PRODUCAO-BIBLIOGRAFICA")# %>% xml_child("ARTIGOS-PUBLICADOS") %>% xml_children()
  
  if (inherits(artigos, "xml_missing")) return(data.frame())
  
  artigos <- xml_child(artigos, "ARTIGOS-PUBLICADOS")
  
  if (inherits(artigos, "xml_missing")) return(data.frame())
  
  artigos <- xml_children(artigos)
  
  # aplicar funcao para extrair metadados de todos os artigos do autor
  numero_artigos <- length(artigos)
  df_artigos <- vector("list", numero_artigos)
  
  for (i in seq_len(numero_artigos)){
    res <- try({extrair_artigo_metadados(artigos[[i]])}, silent = TRUE)
    if (inherits(res, "try-error")){
      res <- extrair_artigo_metadados(artigos[[i]], debug = TRUE)
      #stop(paste0("Problema no artigo ", i ))
    }
    df_artigos[[i]] <- res
  }
  df_artigos <- bind_rows(df_artigos)
  df_artigos
}
