df <- df_rais[,.(id_pessoa,data_adm,data_deslig)]

df <- df[, count := as.character(rowid(id_pessoa))]

df <- df %>% 
  tidyr::pivot_wider(names_from = 'count',
                     values_from = c('data_adm','data_deslig'),
                     values_fill = NULL)