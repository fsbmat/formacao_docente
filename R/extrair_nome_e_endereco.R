
extrair_nome_completo <- function(lattes_xml_document){
  res <- lattes_xml_document %>%
    xml_child("DADOS-GERAIS") %>%
    xml_attr("NOME-COMPLETO")
  res
}

extrair_endereco_profissional <- function(lattes_xml_document){
  res <- lattes_xml_document %>% 
    xml_child("DADOS-GERAIS") %>% 
    xml_child("ENDERECO") %>% 
    xml_child("ENDERECO-PROFISSIONAL") %>% 
    xml_attrs()
  
  if (all(!is.na(res))) res <-  named_vector_to_df(res)
  
  
  if (is.data.frame(res)){
    res <- clean_names(res)
  } else {
    res <- data.frame()
  }
  
  res
}

extrair_formacao_professor <- function(lattes_xml_document){
  res <- lattes_xml_document %>% 
    xml_child("DADOS-GERAIS") %>% 
    xml_child("FORMACAO-ACADEMICA-TITULACAO") %>% 
    xml_child("GRADUACAO") %>% 
    xml_attrs()
  
  if (all(!is.na(res))) res <-  named_vector_to_df(res)
  
  
  if (is.data.frame(res)){
    res <- clean_names(res)
  } else {
    res <- data.frame()
  }
  
  res
}

extrair_id <- function(lattes_xml_document){
  atributos <- xml_attrs(lattes_xml_document)
  atributos <- named_vector_to_df(atributos)
  atributos <- clean_names(atributos)
  
  id_col <- "numero_identificador"
  if (!id_col %in% names(atributos)){
    atributos[[id_col]] <- "sem_id"
  }
  
  if (atributos[[id_col]] == "") atributos[[id_col]] <- "sem_id"
  atributos
}
