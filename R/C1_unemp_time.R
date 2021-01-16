### C1: Calculate unemployment spells

# Load libraries --------

source('setup.R')

calculate_unemp_spell <- function(metro){
  
  message('Working on, ', metro)
  
  panel <- readr::read_rds(here::here('data',metro,paste0('panel_',metro,'.rds'))) %>% 
    data.table::setDT()
  
  df <- panel[,.(id_pessoa,data_adm,data_deslig)]
  data.table::setkey(df, id_pessoa,data_adm)
  
  df <- df[, count := as.character(data.table::rowid(id_pessoa))]
  
  df <- df %>% 
    tidyr::pivot_wider(names_from = 'count',
                       values_from = c('data_adm','data_deslig'),
                       values_fill = NULL)
  
  unemp_time <- data.table::data.table(
    id_pessoa = df$id_pessoa,
    unemp1 = gsub('d 0H 0M 0S','',lubridate::interval(df$data_deslig_1,df$data_adm_2)) %>% lubridate::as.period('days'),
    unemp2 = gsub('d 0H 0M 0S','',lubridate::interval(df$data_deslig_2,df$data_adm_3)) %>% lubridate::as.period('days'),
    unemp3 = gsub('d 0H 0M 0S','',lubridate::interval(df$data_deslig_3,df$data_adm_4)) %>% lubridate::as.period('days'),
    unemp4 = gsub('d 0H 0M 0S','',lubridate::interval(df$data_deslig_4,df$data_adm_5)) %>% lubridate::as.period('days'),
    unemp5 = gsub('d 0H 0M 0S','',lubridate::interval(df$data_deslig_5,df$data_adm_6)) %>% lubridate::as.period('days'),
    unemp6 = gsub('d 0H 0M 0S','',lubridate::interval(df$data_deslig_6,df$data_adm_7)) %>% lubridate::as.period('days'),
    unemp7 = gsub('d 0H 0M 0S','',lubridate::interval(df$data_deslig_7,df$data_adm_8)) %>% lubridate::as.period('days'),
    unemp8 = gsub('d 0H 0M 0S','',lubridate::interval(df$data_deslig_8,df$data_adm_9)) %>% lubridate::as.period('days'),
    unemp9 = gsub('d 0H 0M 0S','',lubridate::interval(df$data_deslig_9,df$data_adm_10)) %>% lubridate::as.period('days'),
    unemp10 = gsub('d 0H 0M 0S','',lubridate::interval(df$data_deslig_10,df$data_adm_11)) %>% lubridate::as.period('days')
  ) %>% data.table::setkey(id_pessoa)
  
  readr::write_rds(unemp_time, here::here('data',metro,paste0('enemp_spell_',metro,'.rds')), compress = 'gz')

}

metros <- metros_br$ab_metro %>% unique()
purrr::walk(.x = metros,.f = calculate_unemp_spell)
