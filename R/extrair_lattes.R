named_vector_to_df <- function(vec, transpose = FALSE){
  
  df <- as.data.frame(matrix(data = NA_character_, nrow = 1, ncol = length(vec)),
                      stringsAsFactors = FALSE)
  for (i in seq_along(vec)){
    df[, i] <- unname(vec[i])
    names(df)[i] <- names(vec)[i]
  }
  if (transpose) t(df) else (df)
}


extrair_lattes <- function(lattes_xml_document, debug = FALSE){
  if (debug) browser() 
  
  atributos <- xml_attrs(lattes_xml_document)
  atributos <- named_vector_to_df(atributos)
  atributos <- clean_names(atributos)
  
  id_col <- "numero_identificador"
  if (!id_col %in% names(atributos)){
    atributos[[id_col]] <- "sem_id"
  }
  
  if (atributos[[id_col]] == "") atributos[[id_col]] <- "sem_id"
  id <- atributos[[id_col]]
  
  nome <- extrair_nome_completo(lattes_xml_document)
  ender_prof <- extrair_endereco_profissional(lattes_xml_document)
  form_prof <- extrair_formacao_professor(lattes_xml_document)
  #FORMACAO-ACADEMICA-TITULACAO
  
  artigos <- extrair_todos_artigos(lattes_xml_document)
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
  
  df <- left_join(nome, atributos, by = id_col)
  df <- left_join(df, ender_prof, by = id_col)
  df <- left_join(df, form_prof, by = id_col)
  if (ncol(artigos) > 0) df <- left_join(df, artigos, by = id_col)
  df
}



