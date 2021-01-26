### A2: Read RAIS data ################

# Load libraries --------

source('setup.R')


### 1. Read data of firms (RAIS ESTAB) ----------------

# Function to read RAIS data on a given year

read_rais <- function(year){
  
  message('Working on ', year)
  
  # Data on firms
  df_estab <- data.table::fread(
    file = paste0(path_rais,'csv','/estab',year,'.csv'), #nrows = 50)
    select = c(
      "id_estab","logradouro","bairro",
      "codemun","cep", "subs_ibge",'clas_cnae20', "qt_vinc_ativos"))
  
  df_estab <- data.table::setDT(df_estab, key='id_estab')[qt_vinc_ativos > 0 & 
                                                          as.character(codemun) %in% as.character(metros_br$cd_6)] %>% unique() 
  
  # Combine address columns and drop columns
  #df_estab <- df_estab[, address := paste(logradouro,bairro, sep = " - ")]
  df_estab <- df_estab[,cep:= stringr::str_trim(cep)]

  # Setkey
  
  df_estab <- df_estab[,.(qt_workers = sum(as.numeric(qt_vinc_ativos),na.rm = TRUE)),
                       by = .(id_estab,codemun,logradouro,bairro,cep,subs_ibge,clas_cnae20)] %>% unique()
  
  filtro <- df_estab %>% 
   dplyr::group_by(id_estab) %>% 
  dplyr::mutate(filtro=dplyr::n_distinct(logradouro)) %>% 
  dplyr::ungroup() %>% dplyr::filter(filtro>1)
  
  df_estab <- df_estab[id_estab %nin% filtro$id_estab]
  
  data.table::setkey(df_estab, key = 'id_estab')
  
  # Data on workers

    # Load file
  df_vinc <- data.table::fread(
    file = paste0(path_rais,'csv/','brasil',year,'.csv'), #nrows = 10) #,
    select = c(
      "id_estab","codemun", "cpf",'nome_trab', "data_nasc", "idade", 
      "genero", "raca_cor", "grau_instr", "tipo_vinculo", "emp_31dez", "cbo2002",
      "data_adm","data_deslig","temp_empr","horas_contr",
      "salario","rem_med_r","rem_med_sm",
      "vl_rem_01","vl_rem_02","vl_rem_03","vl_rem_04","vl_rem_05","vl_rem_06",
      "vl_rem_07","vl_rem_08","vl_rem_09","vl_rem_10","vl_rem_11","vl_rem_12"))
  
  # Filter firms in SP metro area
  df_vinc <- df_vinc[as.character(codemun) %in% as.character(metros_br$cd_6)] %>% unique()
  
  df_vinc <- df_vinc[, vl_rem_ano := vl_rem_01+vl_rem_02+vl_rem_03+vl_rem_04+vl_rem_05+vl_rem_06+
                                     vl_rem_07+vl_rem_08+vl_rem_09+vl_rem_10+vl_rem_12+vl_rem_11]
  
  df_vinc <- df_vinc[,!c("vl_rem_01","vl_rem_02","vl_rem_03","vl_rem_04","vl_rem_05","vl_rem_06",
                         "vl_rem_07","vl_rem_08","vl_rem_09","vl_rem_10","vl_rem_11","vl_rem_12",'codemun')]
  
  data.table::setkey(df_vinc, key = 'id_estab')
  
  # Merge
  df_vinc <- data.table::merge.data.table(df_vinc,
                                          df_estab,
                             all.x = TRUE,
                              by = 'id_estab') %>% unique()
  
  data.table::setDT(df_vinc, key = c('id_estab', 'cpf'))
  
  workers <- df_vinc[emp_31dez == 1]
  workers <- workers[,.(workers = .N), by = .(id_estab,codemun,logradouro,bairro,cep,subs_ibge,clas_cnae20,qt_workers)]
  
  readr::write_rds(df_vinc, here::here('data-raw','RAIS', paste0('rais_',year,'.rds')), compress = 'gz')
  readr::write_rds(workers, here::here('data-raw','RAIS', paste0('workers_',year,'.rds')), compress = 'gz')
  
  rm(df_vinc,df_estab,workers)
  
}


### 4. Apply functions
years <- as.character(seq(2011,2019,1))
purrr::walk(.x = years, .f = read_rais)
