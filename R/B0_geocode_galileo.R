# B0: Geocode home locations with Galileo

# Load libraries
source('setup.R')

# Function to generate Galileo's input from CADUnico data

input_galileo <- function(metro){
  
  people <- readr::read_rds(here::here('data',metro,paste0('people_anon_',metro,'.rds')))
  
  people <- setDT(people)[,.(tp_logradouro, no_logradouro,nu_logradouro,localidade,cep,cd_ibge)]
  
  people <- merge.data.table(people,
                             metros_br[,.(cd_ibge,name_city,name_uf)],
                             all.x = TRUE,
                             by = 'cd_ibge')[,!c('cd_ibge')]
  
  people <- people[, nu_logradouro := gsub('^0',"",nu_logradouro)]
  people <- people[, nu_logradouro := ifelse(nu_logradouro == '','SN',nu_logradouro)]
  
  people <- people[, Endereco := paste(paste(tp_logradouro,no_logradouro,sep = ' '),nu_logradouro,sep = ', ')]
  
  people <- people %>% dplyr::select(Endereco,
                                     Cidade = name_city,
                                     Bairro = localidade,
                                     CEP = cep,
                                     Estado = name_uf) %>% unique()
  
  return(people)
  
}

people <- purrr::map(.x = c('bho','rjo','spo','rec','for','cur','poa'),.f = input_galileo) %>% rbindlist()

regmet <- metros_br[name_city %in% c('Belo Horizonte','Rio De Janeiro','São Paulo',
                                     'Recife','Fortaleza','Curitiba','Porto Alegre')]

people_rm <- people[Cidade %in% regmet$name_city]

readr::write_rds(people, here::here('data','geocode','input_galileo.rds'))
readr::write_rds(people_rm, here::here('data','geocode','input_galileo_rm.rds'))

input_galileo1 <- people_rm[Cidade == 'São Paulo'] %>% readr::write_delim(delim = ';',here::here('data','geocode','input_galileo_1.csv'))
input_galileo2 <- people_rm[Cidade != 'São Paulo'] %>% readr::write_delim(delim = ';',here::here('data','geocode','input_galileo_2.csv'))
input_galileo3 <- people[Cidade %nin% regmet$name_city] %>% readr::write_delim(delim = ';',here::here('data','geocode','input_galileo_3.csv'))

# Load Galileo Output

input <- data.table::fread(here::here('data','geocode','input_galileo_1.csv'), encoding = 'UTF-8')
output <- data.table::fread(here::here('data','geocode','output_galileo_1.csv'), encoding = 'Latin-1')

janitor::tabyl(output, PrecisionDepth)

# Load people from SP

people <- readr::read_rds(here::here('data','spo','people_anon_spo.rds')) %>% data.table::setDT()

people <- people[cd_ibge == 3550308]

people <- people[, nu_logradouro := gsub('^0',"",nu_logradouro)]
people <- people[, nu_logradouro := ifelse(nu_logradouro == '','SN',nu_logradouro)]

people <- people[, Endereco := paste(paste(tp_logradouro,no_logradouro,sep = ' '),nu_logradouro,sep = ', ')]

people <- data.table::merge.data.table(people[,.(id_pessoa,gend,race,dtbirth,year,Endereco,localidade,cep,cd_ibge)],
                                       output[,.(Endereco,Bairro,Cidade,CEP,Longitude,Latitude,PrecisionDepth)],
                                       all.x = TRUE,
                                       by.x = c('Endereco','localidade','cep'),
                                       by.y = c('Endereco','Bairro','CEP'))

people <- people %>% dplyr::select(id_pessoa,
                                   gend, race, dtbirth,
                                   ano = year, endereco = Endereco,
                                   bairro = localidade,cep,
                                   cd_ibge,city = Cidade,
                                   lon = Longitude,lat = Latitude,
                                   pd = PrecisionDepth) %>% 
  data.table::setDT(key = 'id_pessoa') %>% 
  readr::write_rds(here::here('data','spo','people_spo_geocoded_1.rds'),compress = 'gz')