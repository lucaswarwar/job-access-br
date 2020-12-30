source('setup.R')

#metro <- 'spo'

read_cnefe <- function(metro){
  
  message('Working on ',metro)
  
  state <- metros_br[ab_metro == metro, .(cd_uf)][1]$cd_uf
  
  cnefe <- 
    readr::read_fwf(file = paste0('D:/data/cnefe/',state,'.txt'),
                    col_positions = readr::fwf_widths(
                      widths = c(2,5,2,2,4,1,20,30,60,8,7,
                                 20,10,20,10,20,10,20,10,20,10,20,10,
                                 15,15,60,60,2,40,1,30,3,3,8),
                      col_names = c('UF','CD_MUN','CD_DIST','CD_SUBD','CD_SET','SIT_SET',
                                    'TP_LOGRADOURO','TT_LOGRADOURO','NO_LOGRADOURO','NU_LOGRADOURO','MOD_NU',
                                    'E1','VL1','E2','VL2','E3','VL3','E4','VL4','E5','VL5','E6','VL6','LAT','LON',
                                    'LOCALIDADE','NULO','ESP_END','ID_EST','ID_END','ID_DOM','NU_QD','NU_FACE','CEP')),
                    col_types = cols(.default = col_character(), 
                                     NU_LOGRADOURO = col_double(),
                                     LAT = col_character(),
                                     LON = col_character(),
                                     CEP = col_double()))

  metro_df <- metros_br[ab_metro == metro]
  metro_df <- metro_df[, CD_MUN := as.character(stringr::str_sub(cd_ibge,3,7))]

  cnefe <- data.table::setDT(cnefe)[CD_MUN %in% metro_df$CD_MUN]
  
  cnefe <- cnefe[!is.na(LAT)]
  cnefe <- cnefe[, LAT := stringr::str_trim(gsub(' S','',LAT))]
  cnefe <- cnefe[, LON := stringr::str_trim(gsub(' O','',LON))]
  
  cnefe <- cnefe[, degrees := as.numeric(gsub(" ",".",stringr::str_sub(LAT,1,5)))]
  cnefe <- cnefe[, minutes := as.numeric(gsub("[.]","",stringr::str_sub(LAT,7,8)))]
  cnefe <- cnefe[, seconds := as.numeric(stringr::str_sub(LAT,-4,-1))]
  cnefe <- cnefe[, LAT := -1*(degrees + (minutes/60) + (seconds/3600))]
  
  cnefe <- cnefe[, degrees := as.numeric(gsub(" ",".",stringr::str_sub(LON,1,5)))]
  cnefe <- cnefe[, minutes := as.numeric(gsub("[.]","",stringr::str_sub(LON,7,8)))]
  cnefe <- cnefe[, seconds := as.numeric(stringr::str_sub(LON,-4,-1))]
  cnefe <- cnefe[, LON := -1*(degrees + (minutes/60) + (seconds/3600))]
  
  cnefe <- cnefe[, NO_LOGRADOURO := ifelse(is.na(TT_LOGRADOURO),NO_LOGRADOURO,
                                           paste(TT_LOGRADOURO,NO_LOGRADOURO, sep = ' '))]
  
  cnefe <- cnefe[, NU_LOGRADOURO := ifelse(NU_LOGRADOURO>0,as.character(NU_LOGRADOURO), MOD_NU)]
  cnefe <- cnefe[, CD_IBGE := paste0(UF,CD_MUN)]
  cnefe <- cnefe[, .(CD_IBGE,TP_LOGRADOURO,NO_LOGRADOURO,NU_LOGRADOURO,LOCALIDADE,CEP,LAT,LON)]
  
  readr::write_rds(cnefe, here::here('data-raw','CNEFE',paste0('cnefe_',metro,'.rds')),compress = 'gz')
}

metros <- metros_br$ab_metro %>% unique()
purrr::walk(.x = metros,.f = read_cnefe)


