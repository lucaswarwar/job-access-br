### A1 - Get CADUnico identified data

# Load libraries --------

source('setup.R')


get_everyone <- function(metro){
  
  message('Working on, ', metro)
  
  files <- list.files(here::here('data-raw',metro,'CADUnico'))
  
  read_cad <- function(x) {
    
    df <- readr::read_rds(here::here('data-raw',metro,'CADUnico',x))
    df <- data.table::setDT(df, key = 'cpf')[,.(cpf,gend,race,dtbirth,
                                                address,cd_ibge,cep,dt_upd)] %>% unique()
    df <- df[!is.na(cpf)]
    
    return(df)
 }  
  
  df_cpf <- purrr::map(.x = files,.f = read_cad) %>% data.table::rbindlist() %>% unique()
  df_cpf <- data.table::setDT(df_cpf, key = 'cpf')
  
  df_cpf <- df_cpf[, address := gsub("00000000000000","",address)] %>% unique()
  df_cpf <- df_cpf[, address := gsub("0000000000000","",address)] %>% unique()
  df_cpf <- df_cpf[, address := gsub("000000000000","",address)] %>% unique()
  df_cpf <- df_cpf[, dt_upd  := stringr::str_sub(dt_upd,1,4)]
  
  df_cpf <- df_cpf %>% 
    dplyr::group_by(cpf, address) %>% 
    dplyr::mutate(year = min(as.numeric(dt_upd))) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-dt_upd) %>% 
    data.table::setDT(key = 'cpf') %>% 
    unique()
  
  files2 <- list.files(here::here('data-raw',metro,'RAIS'))
  
  read_rais <- function(x) {
    
    df <- readr::read_rds(here::here('data-raw',metro,'RAIS',x))
    df <- data.table::setDT(df, key = 'cpf')[,.(id_estab,cpf,data_adm,data_deslig,emp_31dez,
                                                salario,horas_contr,rem_med_r,rem_med_sm,temp_empr,
                                                subs_ibge,address,codemun,cep)] %>% unique()
    
    df <- df[!is.na(cpf) & cpf > 0]
    df <- df[emp_31dez == 0]
    
    return(df)
  }  
  
  df_rais <- purrr::map(.x = files2,.f = read_rais) %>% data.table::rbindlist() %>% unique()
  df_rais <- data.table::setDT(df_rais, key = 'cpf')
  df_rais <- df_rais[cpf %in% df_cpf$cpf,!c('emp_31dez')]
  
  readr::write_rds(df_cpf, here::here('data',metro,paste0('people_',metro,'.rds')), compress = 'gz')
  readr::write_rds(df_rais, here::here('data',metro,paste0('panel_',metro,'.rds')), compress = 'gz')
  
}

metros <- metros_br$ab_metro %>% unique()
purrr::walk(.x = metros,.f = get_everyone)

anonimize <- function(metro){
  
  people <- readr::read_rds(here::here('data',metro,paste0('people_',metro,'.rds'))) %>% data.table::setDT(key = 'cpf')
  panel  <- readr::read_rds(here::here('data',metro,paste0('panel_',metro,'.rds'))) %>% data.table::setDT(key = 'cpf')
  
  people <- people %>% 
    dplyr::group_by(cpf) %>% 
    dplyr::mutate(id_pessoa = dplyr::cur_group_id()) %>% 
    dplyr::ungroup() %>% 
    data.table::setDT(key = 'id_pessoa')
  
  panel <- data.table::merge.data.table(panel,
                                        people[,.(cpf, id_pessoa)],
                                        all.x = TRUE,
                                        by = 'cpf') %>% 
    data.table::setDT(key = 'id_pessoa')
  
  readr::write_rds(people, here::here('data',metro,paste0('people_',metro,'.rds')), compress = 'gz')
  readr::write_rds(panel, here::here('data',metro,paste0('panel_',metro,'.rds')), compress = 'gz')
  readr::write_rds(people[,!c('cpf')], here::here('data',metro,paste0('people_anon_',metro,'.rds')), compress = 'gz')
  readr::write_rds(panel[,!c('cpf')], here::here('data',metro,paste0('panel_anon_',metro,'.rds')), compress = 'gz')
}

metros <- metros_br$ab_metro %>% unique()
purrr::walk(.x = metros,.f = anonimize)