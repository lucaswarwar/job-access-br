### A3 - Create unified panel accross years

# Load libraries --------

source('setup.R')
#year <- '2018'
metro <- 'for'

get_everyone <- function(metro){
  
  message('Working on, ', metro)
  
  # Get everyone from CAD Unico accross years
  files <- list.files(here::here('data-raw',metro,'CADUnico'))
  
  read_cad <- function(x) {
    
    df <- readr::read_rds(here::here('data-raw',metro,'CADUnico',x))
    df <- data.table::setDT(df, key = 'cpf')[,.(cpf,gend,race,dtbirth,
                                                tp_logradouro,no_logradouro,nu_logradouro,
                                                localidade,cd_ibge,cep,dt_upd)] %>% unique()
    df <- df[!is.na(cpf)]
    
    return(df)
 }  
  
  df_cpf <- purrr::map(.x = files,.f = read_cad) %>% data.table::rbindlist() %>% unique()
  df_cpf <- data.table::setDT(df_cpf, key = 'cpf')
  
  # 1. Padronize address
  # Fix numbers
  df_cpf <- df_cpf[, nu_logradouro := gsub("00000000000000","",nu_logradouro)] %>% unique()
  df_cpf <- df_cpf[, nu_logradouro := gsub("0000000000000","",nu_logradouro)] %>% unique()
  df_cpf <- df_cpf[, nu_logradouro := gsub("000000000000","",nu_logradouro)] %>% unique()
  df_cpf <- df_cpf[, nu_logradouro := gsub("00000000000","",nu_logradouro)] %>% unique()
  df_cpf <- df_cpf[, nu_logradouro := gsub("0000000000","",nu_logradouro)] %>% unique()
  df_cpf <- df_cpf[, nu_logradouro := gsub("000000000","",nu_logradouro)] %>% unique()
  df_cpf <- df_cpf[, nu_logradouro := gsub("00000","",nu_logradouro)] %>% unique()
  
  # Fix type of way
  df_cpf <- df_cpf[, tp_logradouro := data.table::fcase(
    tp_logradouro == "RUA" | tp_logradouro == "10A" |tp_logradouro == "R" | 
    tp_logradouro == "R." | stringr::str_detect(tp_logradouro,"RUA"), "RUA",
    tp_logradouro == "AVENIDA" | tp_logradouro == "AV" |tp_logradouro == "AV." | 
    tp_logradouro == "AVE" | stringr::str_detect(tp_logradouro,"AVENIDA"), "AVENIDA",
    tp_logradouro == 'ALA' | tp_logradouro == 'ALAMEDA','ALAMEDA',
    tp_logradouro == 'BEC' | tp_logradouro == 'BECO','BECO',
    tp_logradouro == 'TRA' | tp_logradouro == 'TRAVESSA','TRAVESSA',
    tp_logradouro == 'DIS' | tp_logradouro == 'DISTRITO','DISTRITO',
    tp_logradouro == 'ACE' | stringr::str_detect(tp_logradouro,"ACESSO"),'ACESSO',
    tp_logradouro == 'BL' | tp_logradouro == 'BLO' | tp_logradouro == 'BLOCO','BLOCO',
    stringr::str_detect(tp_logradouro,"CONJUNTO"),'CONJUNTO',
    tp_logradouro == 'EST' | stringr::str_detect(tp_logradouro,"ESTRADA"),'ESTRADA',
    tp_logradouro == 'SIT' | stringr::str_detect(tp_logradouro,"SITIO"),'SITIO',
    tp_logradouro == 'FAZ' | stringr::str_detect(tp_logradouro,"FAZENDA"),'FAZENDA',
    tp_logradouro == 'CHA' | stringr::str_detect(tp_logradouro,"CHACARA"),'CHACARA',
    tp_logradouro == 'JAR' | tp_logradouro == 'JD' | stringr::str_detect(tp_logradouro,"JARDIM"),'JARDIM',
    tp_logradouro == 'LOT' | stringr::str_detect(tp_logradouro,"LOTEAMENTO"),'LOTEAMENTO',
    tp_logradouro == 'PRA' | tp_logradouro == 'PC' | stringr::str_detect(tp_logradouro,"PRACA"),'PRACA',
    tp_logradouro == 'QUA' | stringr::str_detect(tp_logradouro,"QUADRA"),'QUADRA',
    tp_logradouro == 'RES' | stringr::str_detect(tp_logradouro,"RESIDENCIAL"),'RESIDENCIAL',
    tp_logradouro == 'ROD' | stringr::str_detect(tp_logradouro,"RODOVIA"),'RODOVIA',
    stringr::str_detect(tp_logradouro,"CONDOMINIO"),'CONDOMINIO',
    tp_logradouro == 'VIL' | stringr::str_detect(tp_logradouro,"VILA"),'VILA',
    tp_logradouro == 'VIE' | stringr::str_detect(tp_logradouro,"VIELA"),'VIELA',
    tp_logradouro == 'COL' | stringr::str_detect(tp_logradouro,"COLONIA"),'COLONIA')]
  
  # Padronize street names
  df_cpf <- df_cpf[, no_logradouro := gsub("01","1",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("02","2",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("03","3",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("04","4",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("05","5",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("06","6",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("07","7",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("08","8",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("09","9",no_logradouro)] 
  
  df_cpf <- df_cpf[, no_logradouro := gsub("II","2",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("III","3",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("VII","7",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("VIII","8",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("XII","12",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("XIII","13",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("XIV","14",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("XV","15",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("XVI","16",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("XVII","17",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("XVIII","18",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("XIX","19",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("XXI","21",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("XXII","22",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("XXIII","23",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("XXIV","24",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("XXV","25",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("XXVI","26",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("XXVII","27",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("XXVIII","28",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("XXIX","29",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("XXX","30",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("XX","20",no_logradouro)] 
  
  df_cpf <- df_cpf[, no_logradouro := gsub("^CEL ","CORONEL ",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("^MAJ ","MAJOR ",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("^CAP ","CAPITAO ",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("^TN ","TENENTE ",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("^TEN ","TENENTE ",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("^MAL ","MARECHAL ",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("^GAL ","GENERAL ",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("^DR  ","DOUTOR ",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("^PROF ","PROFESSOR ",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("^ENG ","ENGENHEIRO ",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("^PE ","PADRE ",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("^FR ","FREI ",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("^DES ","DESEMBARGADOR ",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("^VISC ","VISCONDE ",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("^DUQ ","DUQUE ",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("^BR ","BARAO ",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("^DQ ","DUQUE ",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("^COM ","COMENDADOR ",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("^STO ","SANTO ",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("^STA ","SANTA ",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("^S ","SAO ",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("^PRES ","PRESIDENTE ",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("^GOV ","GOVERNADOR ",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("^PREF ","PREFEITO ",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("^SEN ","SENADOR ",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("^DEP ","DEPUTADO ",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("^VER ","VEREADOR ",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("^BRIG ","BRIGADEIRO ",no_logradouro)] 
  df_cpf <- df_cpf[, no_logradouro := gsub("SDO","SEM DENOMINACAO",no_logradouro)] 
  
  df_cpf <- df_cpf[, nu_logradouro := gsub("S/N","SN",nu_logradouro)]
  
  df_cpf <- df_cpf[, dt_upd  := stringr::str_sub(dt_upd,1,4)]
  
  df_cpf <- df_cpf %>% 
    dplyr::group_by(cpf,tp_logradouro,no_logradouro,nu_logradouro,localidade) %>% 
    dplyr::mutate(year = min(as.numeric(dt_upd))) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-dt_upd) %>% 
    data.table::setDT(key = 'cpf') %>% 
    unique()
  
  df_cpf <- df_cpf[!is.na(tp_logradouro)] %>% unique()
  
  df_cpf <- df_cpf %>% 
    dplyr::group_by(cpf) %>% 
    dplyr::mutate(id_pessoa = dplyr::cur_group_id()) %>% 
    dplyr::ungroup() %>% 
    data.table::setDT(key = 'id_pessoa') %>% unique()
  
  # Get modes for individual characteristics and replace in original data
  modes <- data.frame(
    id_pessoa = unique(df_cpf$id_pessoa),
    race = collapse::fmode(df_cpf$race, df_cpf$id_pessoa, na.rm = TRUE),
    gend = collapse::fmode(df_cpf$gend, df_cpf$id_pessoa, na.rm = TRUE),
    dtbirth = collapse::fmode(df_cpf$dtbirth, df_cpf$id_pessoa, na.rm = TRUE)) %>% 
    data.table::setDT(key = 'id_pessoa')
  
  # Replace and overwrite old dataset
  df_cpf <- df_cpf[,.(cpf,id_pessoa,tp_logradouro,no_logradouro,nu_logradouro,localidade,cd_ibge,cep,year)]
  df_cpf <- data.table::merge.data.table(df_cpf, 
                                         modes,
                                         all.x = TRUE,
                                         by = 'id_pessoa') %>% unique()
  
  # Get everyone from RAIS
  files2 <- list.files(here::here('data-raw',metro,'RAIS'))
  
  read_rais <- function(x) {
    
    df <- readr::read_rds(here::here('data-raw',metro,'RAIS',x))
    df <- data.table::setDT(df, key = 'cpf')[,.(id_estab,cpf,grau_instr,data_adm,data_deslig,emp_31dez,
                                                salario,horas_contr,rem_med_r,rem_med_sm,temp_empr,
                                                subs_ibge,address,codemun,cep)] %>% unique()
    
    df <- df[!is.na(cpf) & cpf > 0]
    df <- df[emp_31dez == 0]
    
    return(df)
  }  
  
  df_rais <- purrr::map(.x = files2,.f = read_rais) %>% data.table::rbindlist() %>% unique()
  df_rais <- data.table::setDT(df_rais, key = 'cpf')
  df_rais <- df_rais[cpf %in% df_cpf$cpf,!c('emp_31dez')]
  
  df_rais <- df_rais[, data_adm := gsub('jan','/01/',data_adm)]
  df_rais <- df_rais[, data_adm := gsub('feb','/02/',data_adm)]
  df_rais <- df_rais[, data_adm := gsub('mar','/03/',data_adm)]
  df_rais <- df_rais[, data_adm := gsub('apr','/04/',data_adm)]
  df_rais <- df_rais[, data_adm := gsub('may','/05/',data_adm)]
  df_rais <- df_rais[, data_adm := gsub('jun','/06/',data_adm)]
  df_rais <- df_rais[, data_adm := gsub('jul','/07/',data_adm)]
  df_rais <- df_rais[, data_adm := gsub('aug','/08/',data_adm)]
  df_rais <- df_rais[, data_adm := gsub('sep','/09/',data_adm)]
  df_rais <- df_rais[, data_adm := gsub('oct','/10/',data_adm)]
  df_rais <- df_rais[, data_adm := gsub('nov','/11/',data_adm)]
  df_rais <- df_rais[, data_adm := gsub('dec','/12/',data_adm)]
  df_rais <- df_rais[, data_adm := gsub('/','-',data_adm)]
  
  df_rais <- df_rais[, data_deslig := gsub('jan','/01/',data_deslig)]
  df_rais <- df_rais[, data_deslig := gsub('feb','/02/',data_deslig)]
  df_rais <- df_rais[, data_deslig := gsub('mar','/03/',data_deslig)]
  df_rais <- df_rais[, data_deslig := gsub('apr','/04/',data_deslig)]
  df_rais <- df_rais[, data_deslig := gsub('may','/05/',data_deslig)]
  df_rais <- df_rais[, data_deslig := gsub('jun','/06/',data_deslig)]
  df_rais <- df_rais[, data_deslig := gsub('jul','/07/',data_deslig)]
  df_rais <- df_rais[, data_deslig := gsub('aug','/08/',data_deslig)]
  df_rais <- df_rais[, data_deslig := gsub('sep','/09/',data_deslig)]
  df_rais <- df_rais[, data_deslig := gsub('oct','/10/',data_deslig)]
  df_rais <- df_rais[, data_deslig := gsub('nov','/11/',data_deslig)]
  df_rais <- df_rais[, data_deslig := gsub('dec','/12/',data_deslig)]
  df_rais <- df_rais[, data_deslig := gsub('/','-',data_deslig)]
  
  dates_adm <- stringr::str_split_fixed(df_rais$data_adm, "-",n=3) %>% as.data.frame()
  dates_deslig <- stringr::str_split_fixed(df_rais$data_deslig, "-",n=3) %>% as.data.frame()
  
  dates_adm <- data.table::setDT(dates_adm)[, V1 := ifelse(stringr::str_length(V1) == 1,paste0(0,V1),V1)]
  dates_adm <- dates_adm[, V2 := ifelse(stringr::str_length(V2) == 1,paste0(0,V2),V2)]
  dates_deslig <- data.table::setDT(dates_deslig)[, V1 := ifelse(stringr::str_length(V1) == 1,paste0(0,V1),V1)]
  dates_deslig <- dates_deslig[, V2 := ifelse(stringr::str_length(V2) == 1,paste0(0,V2),V2)]
  
  dates_adm <- dates_adm[, data_adm := data.table::fifelse(
    V3 == '2017',paste(V1,V2,V3,sep = '-'), 
    paste(V2,V1,V3,sep = '-'))]
  
  dates_deslig <- dates_deslig[, data_deslig := data.table::fifelse(
    V3 == '2017',paste(V1,V2,V3,sep = '-'), 
    paste(V2,V1,V3,sep = '-'))]
  
  df_rais <- df_rais[, data_adm := dates_adm$data_adm]
  df_rais <- df_rais[, data_deslig := dates_deslig$data_deslig]
  
  df_rais <- df_rais[, address := gsub(",,",",",address)] 
  df_rais <- df_rais[, address := gsub("^R ","RUA ",address)] 
  df_rais <- df_rais[, address := gsub("^R. ","RUA ",address)] 
  df_rais <- df_rais[, address := gsub("^AV ","AVENIDA ",address)] 
  df_rais <- df_rais[, address := gsub("^AVE ","AVENIDA ",address)] 
  df_rais <- df_rais[, address := gsub("^AV. ","AVENIDA ",address)] 
  df_rais <- df_rais[, address := gsub("^AL ","ALAMEDA ",address)] 
  df_rais <- df_rais[, address := gsub("^AL. ","ALAMEDA ",address)] 
  df_rais <- df_rais[, address := gsub("^PC ","PRACA ",address)] 
  df_rais <- df_rais[, address := gsub("^PC. ","PRACA ",address)] 
  df_rais <- df_rais[, address := gsub("^EST ","ESTRADA ",address)] 
  df_rais <- df_rais[, address := gsub("^EST. ","ESTRADA ",address)] 
  df_rais <- df_rais[, address := gsub("^ESTR ","ESTRADA ",address)] 
  df_rais <- df_rais[, address := gsub("^ESTR. ","ESTRADA ",address)]
  df_rais <- df_rais[, address := gsub("^ROD ","RODOVIA ",address)] 
  df_rais <- df_rais[, address := gsub("^RODOV ","RODOVIA ",address)] 
  df_rais <- df_rais[, address := gsub("^ROD. ","RODOVIA ",address)]
  
  df_rais <- df_rais[, address := gsub("DR. ","DOUTOR ",address)] 
  df_rais <- df_rais[, address := gsub("PROF. ","PROFESSOR ",address)] 
  df_rais <- df_rais[, address := gsub("ENG. ","ENGENHEIRO ",address)] 
  df_rais <- df_rais[, address := gsub("PRES. ","PRESIDENTE ",address)] 
  df_rais <- df_rais[, address := gsub("DR  ","DOUTOR ",address)] 
  df_rais <- df_rais[, address := gsub("PROF ","PROFESSOR ",address)] 
  df_rais <- df_rais[, address := gsub("ENG ","ENGENHEIRO ",address)] 
  df_rais <- df_rais[, address := gsub("PRES ","PRESIDENTE ",address)] 
  
  df_rais <- df_rais[, address := gsub("S/N","SN",address)]
  
  df1 <- stringr::str_split_fixed(df_rais$address, ", ",n=2) %>% as.data.frame()
  df2 <- stringr::str_split_fixed(df1$V2, " - ",n=2) %>% as.data.frame()
  
  df_rais <- df_rais[, logradouro := df1$V1]
  df_rais <- df_rais[, numero := df2$V1]
  df_rais <- df_rais[, localidade := df2$V2]
  
  df_rais <- data.table::merge.data.table(df_rais,
                                          df_cpf[,.(cpf, id_pessoa)],
                                        all.x = TRUE,
                                        by = 'cpf') %>% 
    data.table::setDT(key = 'id_pessoa') %>% unique()
  
  df_rais <- df_rais[, .(id_pessoa,cpf,id_estab,data_adm,data_deslig,grau_instr,
                          salario,horas_contr,rem_med_r,rem_med_sm,temp_empr,
                          subs_ibge,logradouro,numero,localidade,codemun,cep)]
  
  readr::write_rds(df_cpf, here::here('data',metro,paste0('people_',metro,'.rds')), compress = 'gz')
  readr::write_rds(df_rais, here::here('data',metro,paste0('panel_',metro,'.rds')), compress = 'gz')
  readr::write_rds(df_cpf[,!c('cpf')], here::here('data',metro,paste0('people_anon_',metro,'.rds')), compress = 'gz')
  readr::write_rds(df_rais[,!c('cpf')], here::here('data',metro,paste0('panel_anon_',metro,'.rds')), compress = 'gz')
  
}

metros <- metros_br$ab_metro %>% unique()
purrr::walk(.x = metros,.f = get_everyone)


