### A2: Read RAIS data ################

# Load libraries --------

source('setup.R')

year <- '2018'
metro <- 'for'
### 1. Read data of firms (RAIS ESTAB) ----------------

# Function to read RAIS data on a given year

read_rais <- function(year, metros = 'all'){
  
  message('Working on ', year)
  
  read_rais_un <- function(metro) {
    
    message('Working on ', metro)
    
    df_metro <- metros_br %>% dplyr::filter(ab_metro == metro)
  
  # Data on firms
  df_estab <- data.table::fread(
    file = paste0(path_rais,'csv','/estab',year,'.csv'), #nrows = 50)
    select = c(
      "id_estab","cnpj_raiz","logradouro","bairro",
      "codemun","cep", "subs_ibge", "qt_vinc_ativos"))
  
  df_estab <- data.table::setDT(df_estab, key='id_estab')[qt_vinc_ativos > 0 & 
                                                          as.character(codemun) %in% as.character(df_metro$cd_6)] %>% unique() 
  
  # Combine address columns and drop columns
  df_estab <- df_estab[, address := paste(logradouro,bairro, sep = " - ")]
  df_estab <- df_estab[,cep:= stringr::str_trim(cep)]
  df_estab <- df_estab[, !c('logradouro','bairro')] 
  
  # Setkey
  
  df_estab <- df_estab[,.(qt_workers = sum(as.numeric(qt_vinc_ativos),na.rm = TRUE)),
                       by = .(id_estab,cnpj_raiz,codemun,address,cep,subs_ibge)] %>% unique()
  
  filtro <- df_estab %>% 
   dplyr::group_by(id_estab) %>% 
  dplyr::mutate(filtro=dplyr::n_distinct(address)) %>% 
  dplyr::ungroup() %>% dplyr::filter(filtro>1)
  
  df_estab <- df_estab[id_estab %nin% filtro$id_estab]
  
  data.table::setkey(df_estab, key = 'id_estab')
  
  # Data on workers

    # Load file
  df_vinc <- data.table::fread(
    file = paste0(path_rais,'csv/','brasil',year,'.csv'), #nrows = 10) #,
    select = c(
      "id_estab","codemun", "cpf",'nome_trab', "data_nasc", "idade", 
      "genero", "raca_cor", "grau_instr", "tipo_vinculo", "emp_31dez", 
      "data_adm","data_deslig","temp_empr","horas_contr",
      "salario","rem_med_r","rem_med_sm"))
  
  # Filter firms in SP metro area
  df_vinc <- df_vinc[as.character(codemun) %in% as.character(df_metro$cd_6)] %>% unique()
  
  data.table::setkey(df_vinc, key = 'id_estab')
  
  # Merge
  df_vinc <- data.table::merge.data.table(df_vinc,
                                          df_estab[,.(id_estab,cnpj_raiz,address,cep,subs_ibge)] %>% unique(),
                             all.x = TRUE,
                              by = 'id_estab') %>% unique()
  
  data.table::setkey(df_vinc, key = 'cpf')
  
  readr::write_rds(df_vinc, here::here('data-raw',metro,'RAIS', paste0('rais_',metro,'_',year,'.rds')), compress = 'gz')
  rm(df_vinc,df_estab)
  
  }
  
  # 2. Apply function
  
  if (metros == "all") {
    
    x = unique(metros_br$ab_metro)
    
  } else (x = metros)
  
  
  purrr::walk(.x = x,.f = read_rais_un)
}

### 4. Apply functions
years <- as.character(seq(2011,2019,1))
purrr::walk(.x = years, .f = read_rais)
