### A2 - Build panel from CADUnico

# Load libraries --------

source('setup.R')

#metro <- 'spo'

build_panel_metro <- function(metro){
  
  message('Building panel ',metro)
  
  cad_unico <- readr::read_rds(here::here('data-raw','CADUnico','cadunico_2019.rds')) %>% data.table::setDT()
  
  df_metro <- metros_br[ab_metro == metro]
  
  cad_unico <- cad_unico[cd_ibge %in% df_metro$cd_ibge]
  
  cad_unico <- cad_unico[, year := '2019']
  
  panel <- cad_unico[,.(cd_fam,cpf,nis,gend,race,dtbirth,cd_ibge_nasc,year,in_school,in_wk12mo,qt_wk12mo,remtot,rememp,rem_unemp_in,
                        in_wkform,in_wkinf,remmd_fam,cd_ibge,in_h2o_can,in_h20_abas,in_esgoto,in_lixo,in_ilum,in_calc,
                        tp_logradouro,no_logradouro,nu_logradouro,localidade,cep)]
  
  panel <- panel[!is.na(nis)]
  
  rm(cad_unico)
  
  pile_years <- function(ano){
    
    message('Pile year ', ano)
    
    panel2 <- panel[, year := ano]
    
    panel2 <- panel2[,.(cd_fam,cpf,nis,gend,race,dtbirth,cd_ibge_nasc,year)]
    
    cad_unico <- readr::read_rds(here::here('data-raw','CADUnico',paste0('cadunico_',ano,'.rds'))) %>% data.table::setDT()
    
    cad_unico <- cad_unico[cd_ibge %in% df_metro$cd_ibge]
    cad_unico <- cad_unico[!is.na(nis)]
    
    panel2 <- data.table::merge.data.table(panel2,
                                           cad_unico[,.(nis,in_school,in_wk12mo,qt_wk12mo,remtot,rememp,rem_unemp_in,
                                                        in_wkform,in_wkinf,remmd_fam,cd_ibge,in_h2o_can,in_h20_abas,
                                                        in_esgoto,in_lixo,in_ilum,in_calc,tp_logradouro,
                                                        no_logradouro,nu_logradouro,localidade,cep)],
                                           all.x = TRUE,
                                           by = 'nis') %>% unique()
    
    rm(cad_unico)
    
    return(panel2)
    
  }
    
    years <- as.character(c(2018:2011))
    panel3 <- purrr::map(.x = years, .f = pile_years) %>% data.table::rbindlist()
    
    panel <- dplyr::bind_rows(panel,panel3) %>% data.table::setDT(key = c('cd_fam','nis','year'))
    
    readr::write_rds(panel, here::here('data','cadunico',paste0('panel_',metro,'.rds')), compress = 'gz')
    
}

cidades <- metros_br$ab_metro %>% unique()
purrr::walk(.x = cidades,.f = build_panel_metro)


fix_panel <- function(metro){
  
  message('Fixing ',metro)
  
  panel <- readr::read_rds(here::here('data','cadunico',paste0('panel_',metro,'.rds'))) %>% data.table::setDT()
  
  panel <- panel[, id := rowidv(nis)]
  
  year_fix <- panel[id == 1]
  year_fix <- year_fix[, year := '2019']
  year_fix <- year_fix[, !c('id')]
  
  panel <- panel[id != 1, !c('id')]
  
  panel <- dplyr::bind_rows(panel, year_fix) %>% 
    data.table::setDT(key = c('cd_fam','nis','year'))
  
  readr::write_rds(panel, here::here('data','cadunico',paste0('panel_',metro,'.rds')), compress = 'gz')
}

cidades <- metros_br$ab_metro %>% unique()
purrr::walk(.x = cidades,.f = fix_panel)
