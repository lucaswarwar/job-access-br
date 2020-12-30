source('setup.R')

metro <- 'for'


# Fix people dataset

fix_people <- function(metro){
  
  message('Working on ', metro)
  
  people <- readr::read_rds(here::here('data',metro,paste0('people_anon_',metro,'.rds')))
  
  # Set key and exclude missing RUA/AV strings (duplicated)
  people <- data.table::setDT(people,key = 'id_pessoa')[stringr::str_sub(address,1,1) != " "]
  people <- people[, address := gsub('00000000000', "",address)] %>% unique()
  
  # Get modes for individual characteristics and replace in original data
  modes <- data.frame(
    id_pessoa = unique(people$id_pessoa),
    race = collapse::fmode(people$race, people$id_pessoa, na.rm = TRUE),
    gend = collapse::fmode(people$gend, people$id_pessoa, na.rm = TRUE),
    dtbirth = collapse::fmode(people$dtbirth, people$id_pessoa, na.rm = TRUE)) %>% 
    data.table::setDT(key = 'id_pessoa')
  
  # Replace and overwrite old dataset
  people <- people[,.(id_pessoa,address,cd_ibge,cep,year)]
  people <- data.table::merge.data.table(people, 
                                         modes,
                                         all.x = TRUE,
                                         by = 'id_pessoa') %>% unique()
  
  readr::write_rds(people,here::here('data',metro,paste0('people_anon_',metro,'.rds')), compress = 'gz')
  
  # Panel
  panel <- readr::read_rds(here::here('data',metro,paste0('panel_anon_',metro,'.rds')))
  panel <- data.table::setDT(panel,key = 'id_pessoa') %>% unique()
  
  panel <- panel[, data_adm := gsub('jan','/01/',data_adm)]
  panel <- panel[, data_adm := gsub('feb','/02/',data_adm)]
  panel <- panel[, data_adm := gsub('mar','/03/',data_adm)]
  panel <- panel[, data_adm := gsub('apr','/04/',data_adm)]
  panel <- panel[, data_adm := gsub('may','/05/',data_adm)]
  panel <- panel[, data_adm := gsub('jun','/06/',data_adm)]
  panel <- panel[, data_adm := gsub('jul','/07/',data_adm)]
  panel <- panel[, data_adm := gsub('aug','/08/',data_adm)]
  panel <- panel[, data_adm := gsub('sep','/09/',data_adm)]
  panel <- panel[, data_adm := gsub('oct','/10/',data_adm)]
  panel <- panel[, data_adm := gsub('nov','/11/',data_adm)]
  panel <- panel[, data_adm := gsub('dec','/12/',data_adm)]
  panel <- panel[, data_adm := gsub('/','-',data_adm)]
  
  panel <- panel[, data_deslig := gsub('jan','/01/',data_deslig)]
  panel <- panel[, data_deslig := gsub('feb','/02/',data_deslig)]
  panel <- panel[, data_deslig := gsub('mar','/03/',data_deslig)]
  panel <- panel[, data_deslig := gsub('apr','/04/',data_deslig)]
  panel <- panel[, data_deslig := gsub('may','/05/',data_deslig)]
  panel <- panel[, data_deslig := gsub('jun','/06/',data_deslig)]
  panel <- panel[, data_deslig := gsub('jul','/07/',data_deslig)]
  panel <- panel[, data_deslig := gsub('aug','/08/',data_deslig)]
  panel <- panel[, data_deslig := gsub('sep','/09/',data_deslig)]
  panel <- panel[, data_deslig := gsub('oct','/10/',data_deslig)]
  panel <- panel[, data_deslig := gsub('nov','/11/',data_deslig)]
  panel <- panel[, data_deslig := gsub('dec','/12/',data_deslig)]
  panel <- panel[, data_deslig := gsub('/','-',data_deslig)]
  
  panel <- panel[,.(id_pessoa,id_estab,data_adm,data_deslig,
                    address,codemun,cep,subs_ibge,
                    temp_empr,salario,horas_contr,rem_med_r,rem_med_sm)]
  
  readr::write_rds(panel,here::here('data',metro,paste0('panel_anon_',metro,'.rds')), compress = 'gz')
  
  people_locations <- people[id_pessoa %in% panel$id_pessoa, .(address, cd_ibge,cep)] %>% unique()
  people_locations <- people_locations[, codemun := as.integer(stringr::str_sub(cd_ibge,1,6))]
  people_locations <- people_locations[, .(address, codemun,cep)] 
  
  firms_locations <- panel[, .(address, codemun, cep)] %>% unique()
  firms_locations <- firms_locations[, cep := as.integer(cep)]
  
  locations <- dplyr::bind_rows(
    people_locations,
    firms_locations) %>% 
    unique()
  
  readr::write_rds(locations,here::here('data',metro,paste0('locations_',metro,'.rds')), compress = 'gz')
  
}

metros <- metros_br$ab_metro %>% unique()
purrr::walk(.x = metros,.f = fix_people)
beepr::beep()
