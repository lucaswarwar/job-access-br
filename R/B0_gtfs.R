# B1: get additional geocoded data from OSM and GTFS from SP metro

source('setup.R')

# Get Locations from GTFS

# Sp Trans
gtfs <- gtfs2gps::read_gtfs(paste0(path_gtfs,'spo.zip'))

routes <- data.table::setDT(gtfs$routes)[route_type == 3,.(route_id,route_long_name)]
trips <- data.table::setDT(gtfs$trips)[route_id %in% routes$route_id,.(route_id,trip_id,trip_headsign,direction_id)]
stoptimes <- data.table::setDT(gtfs$stop_times)[trip_id %in% trips$trip_id,.(trip_id,stop_id,stop_sequence)] %>% unique()
stops <- data.table::setDT(gtfs$stops)[stop_id %in% stoptimes$stop_id,.(stop_name,stop_lat,stop_lon)]

rm(routes,trips,stoptimes)

# EMTU
gtfs <- gtfs2gps::read_gtfs(paste0(path_gtfs,'emtu.zip'))

stops2 <- data.table::setDT(gtfs$stops)[,.(stop_name,stop_lat,stop_lon)]

stops <- dplyr::bind_rows(stops,stops2)
rm(gtfs,stops2)

df <- stringr::str_split_fixed(stops$stop_name,',',n=2) %>% as.data.frame()

stops <- data.table::setDT(stops)[, logradouro := df$V1]
stops <- stops[, numero := df$V2]
stops <- stops[,.(logradouro,numero,stop_lat,stop_lon)]

stops <- stops[, logradouro := stringr::str_to_upper(logradouro)]

stops <- stops[, logradouro := gsub('^R. ','RUA ',logradouro)]
stops <- stops[, logradouro := gsub('^R ','RUA ',logradouro)]
stops <- stops[, logradouro := gsub('^AV. ','AVENIDA ',logradouro)]
stops <- stops[, logradouro := gsub('^AV ','AVENIDA ',logradouro)]
stops <- stops[, logradouro := gsub('^ESTR. ','ESTRADA ',logradouro)]
stops <- stops[, logradouro := gsub('^PÇA. ','PRACA ',logradouro)]
stops <- stops[, logradouro := gsub('^ROD. ','RODOVIA ',logradouro)]
stops <- stops[, logradouro := gsub('^ROD ','RODOVIA ',logradouro)]
stops <- stops[, logradouro := gsub('^AC. ','ACESSO ',logradouro)]
stops <- stops[, logradouro := gsub('^AL. ','ALAMEDA ',logradouro)]

 
stops <- stops[, logradouro := gsub(" DR. "," DOUTOR ",logradouro)] 
stops <- stops[, logradouro := gsub(" PROF. "," PROFESSOR ",logradouro)] 
stops <- stops[, logradouro := gsub(" ENG. "," ENGENHEIRO ",logradouro)] 
stops <- stops[, logradouro := gsub(" PRES. "," PRESIDENTE ",logradouro)] 
stops <- stops[, logradouro := gsub(" PE. "," PADRE ",logradouro)] 
stops <- stops[, logradouro := gsub(" PST. "," PASTOR ",logradouro)] 
stops <- stops[, logradouro := gsub(" DA. "," DONA ",logradouro)] 
stops <- stops[, logradouro := gsub(" CEL. "," CORONEL ",logradouro)] 
stops <- stops[, logradouro := gsub(" MJ. "," MAJOR ",logradouro)]
stops <- stops[, logradouro := gsub(" SG. "," SARGENTO ",logradouro)]
stops <- stops[, logradouro := gsub(" BRIG. "," BRIGADEIRO ",logradouro)] 
stops <- stops[, logradouro := gsub(" TN. "," TENENTE ",logradouro)]
stops <- stops[, logradouro := gsub(" MAL. "," MARECHAL ",logradouro)] 
stops <- stops[, logradouro := gsub(" TEN. "," TENENTE ",logradouro)] 
stops <- stops[, logradouro := gsub(" CAP. "," CAPITAO ",logradouro)] 
stops <- stops[, logradouro := gsub(" GEN. "," GENERAL ",logradouro)] 
stops <- stops[, logradouro := gsub(" CDSSA. "," CONDESSA ",logradouro)] 
stops <- stops[, logradouro := gsub(" MONS. "," MONSENHOR ",logradouro)] 
stops <- stops[, logradouro := gsub(" DEP. "," DEPUTADO ",logradouro)] 
stops <- stops[, logradouro := gsub(" SEN. "," SENADOR ",logradouro)]
stops <- stops[, logradouro := gsub(" GOV. "," GOVERNADOR ",logradouro)]
stops <- stops[, logradouro := gsub(" DES. "," DESEMBARGADOR ",logradouro)] 
stops <- stops[, logradouro := gsub(" VER. "," VEREADOR ",logradouro)] 
stops <- stops[, logradouro := gsub(" S. "," SAO ",logradouro)] 
stops <- stops[, logradouro := gsub(" STO. "," SANTO ",logradouro)] 
stops <- stops[, logradouro := gsub(" NSA. SRA. "," NOSSA SENHORA ",logradouro)] 
stops <- stops[, logradouro := gsub(" STA. "," SANTA ",logradouro)]
stops <- stops[, logradouro := gsub(" FR. "," FREI ",logradouro)] 
stops <- stops[, logradouro := gsub(" BR. "," BARAO ",logradouro)] 
stops <- stops[, logradouro := gsub(" DUQ. "," DUQUE ",logradouro)] 
stops <- stops[, logradouro := gsub(" VISC. "," VISCONDE ",logradouro)] 
stops <- stops[, logradouro := gsub(" MIN. "," MINISTRO ",logradouro)] 
stops <- stops[, logradouro := gsub(" PROFA. "," PROFESSORA ",logradouro)] 
stops <- stops[, logradouro := gsub(" CÔN. "," CONEGO ",logradouro)] 
stops <- stops[, logradouro := gsub(" BSA. "," BARONESA ",logradouro)] 
stops <- stops[, logradouro := gsub(" CB. "," CABO ",logradouro)] 
stops <- stops[, logradouro := gsub(" SD. "," SOLDADO ",logradouro)] 
stops <- stops[, logradouro := gsub(" PRINC. "," PRINCIPE ",logradouro)] 
stops <- stops[, logradouro := gsub(" MTO. "," MAESTRO ",logradouro)] 
stops <- stops[, logradouro := gsub(" CTE. "," COMANDANTE ",logradouro)] 
stops <- stops[, logradouro := gsub(" IMPZ. "," IMPERATRIZ ",logradouro)] 

stops <- stops[, logradouro := gsub("Ç","C",logradouro)] 
stops <- stops[, logradouro := gsub("Ã","A",logradouro)] 
stops <- stops[, logradouro := gsub("Á","A",logradouro)] 
stops <- stops[, logradouro := gsub("À","A",logradouro)] 
stops <- stops[, logradouro := gsub("Â","A",logradouro)] 
stops <- stops[, logradouro := gsub("Ä","A",logradouro)] 
stops <- stops[, logradouro := gsub("É","E",logradouro)] 
stops <- stops[, logradouro := gsub("Ê","E",logradouro)] 
stops <- stops[, logradouro := gsub("È","E",logradouro)] 
stops <- stops[, logradouro := gsub("Ë","E",logradouro)] 
stops <- stops[, logradouro := gsub("Í","I",logradouro)] 
stops <- stops[, logradouro := gsub("Ì","I",logradouro)] 
stops <- stops[, logradouro := gsub("Î","I",logradouro)] 
stops <- stops[, logradouro := gsub("Ï","I",logradouro)] 
stops <- stops[, logradouro := gsub("Ó","O",logradouro)] 
stops <- stops[, logradouro := gsub("Ò","O",logradouro)] 
stops <- stops[, logradouro := gsub("Ô","O",logradouro)] 
stops <- stops[, logradouro := gsub("Ö","O",logradouro)] 
stops <- stops[, logradouro := gsub("Õ","O",logradouro)] 
stops <- stops[, logradouro := gsub("Ú","U",logradouro)] 
stops <- stops[, logradouro := gsub("Û","U",logradouro)] 
stops <- stops[, logradouro := gsub("Ù","U",logradouro)] 
stops <- stops[, logradouro := gsub("Ü","U",logradouro)] 

readr::write_rds(stops, here::here('geocode','GTFS','gtfs_spo.rds'))

# bho
stops <- data.table::fread(paste0(path_gtfs,'bho/stops.txt'))
stops <- stops[stringr::str_detect(stop_name, 'Estacao ') == FALSE, .(stop_name,stop_lat,stop_lon)]

stops <- stops[, stop_name := stringr::str_to_upper(stop_name)]
stops <- stops[, stop_name := gsub('^AVE ','AVENIDA ',stop_name)]
stops <- stops[, stop_name := gsub('^EST ','ESTRADA ',stop_name)]
stops <- stops[, stop_name := gsub('^PCA ','PRACA ',stop_name)]
stops <- stops[, stop_name := gsub('^ROD ','RODOVIA ',stop_name)]

readr::write_rds(stops, here::here('geocode','GTFS','gtfs_bho.rds'))

# for
stops <- readr::read_csv('https://dados.fortaleza.ce.gov.br/dataset/ef775a23-3b43-4a56-9478-94607fd050e3/resource/856d23f7-e39c-436a-97b2-8fc314e4f154/download/stops.txt')

df <- stringr::str_split_fixed(stops$stop_name,',',n=2) %>% as.data.frame()

stops <- data.table::setDT(stops)[, logradouro := df$V1]
stops <- stops[, numero := df$V2]
stops <- stops[,.(logradouro,numero,stop_lat,stop_lon)]

stops <- stops[, logradouro := gsub(" DR "," DOUTOR ",logradouro)] 
stops <- stops[, logradouro := gsub(" DOR "," DOUTOR ",logradouro)] 
stops <- stops[, logradouro := gsub(" PRO "," PROFESSOR ",logradouro)] 
stops <- stops[, logradouro := gsub(" ENG "," ENGENHEIRO ",logradouro)] 
stops <- stops[, logradouro := gsub(" PRS "," PRESIDENTE ",logradouro)] 
stops <- stops[, logradouro := gsub(" PDE "," PADRE ",logradouro)] 
stops <- stops[, logradouro := gsub(" PST "," PASTOR ",logradouro)] 
stops <- stops[, logradouro := gsub(" DNA "," DONA ",logradouro)] 
stops <- stops[, logradouro := gsub(" CEL "," CORONEL ",logradouro)] 
stops <- stops[, logradouro := gsub(" MAJ "," MAJOR ",logradouro)]
stops <- stops[, logradouro := gsub(" SGT "," SARGENTO ",logradouro)]
stops <- stops[, logradouro := gsub(" BRIG "," BRIGADEIRO ",logradouro)] 
stops <- stops[, logradouro := gsub(" TN "," TENENTE ",logradouro)]
stops <- stops[, logradouro := gsub(" MAL "," MARECHAL ",logradouro)] 
stops <- stops[, logradouro := gsub(" TEN "," TENENTE ",logradouro)] 
stops <- stops[, logradouro := gsub(" CAP "," CAPITAO ",logradouro)] 
stops <- stops[, logradouro := gsub(" GAL "," GENERAL ",logradouro)] 
stops <- stops[, logradouro := gsub(" CDSSA "," CONDESSA ",logradouro)] 
stops <- stops[, logradouro := gsub(" MON "," MONSENHOR ",logradouro)] 
stops <- stops[, logradouro := gsub(" DEP "," DEPUTADO ",logradouro)] 
stops <- stops[, logradouro := gsub(" SEN "," SENADOR ",logradouro)]
stops <- stops[, logradouro := gsub(" GOV "," GOVERNADOR ",logradouro)]
stops <- stops[, logradouro := gsub(" DES "," DESEMBARGADOR ",logradouro)] 
stops <- stops[, logradouro := gsub(" VER "," VEREADOR ",logradouro)] 
stops <- stops[, logradouro := gsub(" S "," SAO ",logradouro)] 
stops <- stops[, logradouro := gsub(" STO "," SANTO ",logradouro)] 
stops <- stops[, logradouro := gsub(" SRA "," SENHORA ",logradouro)] 
stops <- stops[, logradouro := gsub(" NSA "," NOSSA ",logradouro)] 
stops <- stops[, logradouro := gsub(" STA "," SANTA ",logradouro)]
stops <- stops[, logradouro := gsub(" FRE "," FREI ",logradouro)] 
stops <- stops[, logradouro := gsub(" BAR "," BARAO ",logradouro)] 
stops <- stops[, logradouro := gsub(" DUQ "," DUQUE ",logradouro)] 
stops <- stops[, logradouro := gsub(" VISC "," VISCONDE ",logradouro)] 
stops <- stops[, logradouro := gsub(" MIN "," MINISTRO ",logradouro)] 
stops <- stops[, logradouro := gsub(" ALM "," ALMIRANTE ",logradouro)] 
stops <- stops[, logradouro := gsub(" CON "," CONEGO ",logradouro)] 
stops <- stops[, logradouro := gsub(" BSA "," BARONESA ",logradouro)] 
stops <- stops[, logradouro := gsub(" CB "," CABO ",logradouro)] 
stops <- stops[, logradouro := gsub(" JOR "," JORNALISTA ",logradouro)] 
stops <- stops[, logradouro := gsub(" PRINC "," PRINCIPE ",logradouro)] 
stops <- stops[, logradouro := gsub(" MTO "," MAESTRO ",logradouro)] 
stops <- stops[, logradouro := gsub(" MIT "," MISTER ",logradouro)] 
stops <- stops[, logradouro := gsub(" REV "," REVERENDO ",logradouro)] 

readr::write_rds(stops, here::here('geocode','GTFS','gtfs_for.rds'))

# poa
stops <- data.table::fread(paste0(path_gtfs,'poa/stops.txt'))
stops <- stops[,.(stop_name,stop_lat,stop_lon)]

stops <- stops[, stop_name := gsub("DR "," DOUTOR ",stop_name)] 
stops <- stops[, stop_name := gsub("PROF "," PROFESSOR ",stop_name)] 
stops <- stops[, stop_name := gsub("ENG "," ENGENHEIRO ",stop_name)] 

stops <- stops[, stop_name := gsub("CEL "," CORONEL ",stop_name)] 

stops <- stops[, stop_name := gsub("TEN "," TENENTE ",stop_name)] 
stops <- stops[, stop_name := gsub("CAP "," CAPITAO ",stop_name)] 
stops <- stops[, stop_name := gsub("GEN "," GENERAL ",stop_name)] 
stops <- stops[, stop_name := gsub("JD ","JARDIM ",stop_name)] 
stops <- stops[, stop_name := gsub("MON "," MONSENHOR ",stop_name)] 
stops <- stops[, stop_name := gsub("DEP "," DEPUTADO ",stop_name)] 
stops <- stops[, stop_name := gsub("SEN "," SENADOR ",stop_name)]
stops <- stops[, stop_name := gsub("GOV "," GOVERNADOR ",stop_name)]
readr::write_rds(stops, here::here('geocode','GTFS','gtfs_poa.rds'))

# rjo
stops <- readr::read_csv(paste0(path_gtfs,'rjo/stops.txt')) %>% data.table::setDT()

stops <- stops[,.(stop_name,stop_lat,stop_lon)]
stops <- stops[, stop_name := stringr::str_to_upper(stop_name)]

df <- stringr::str_split_fixed(stops$stop_name,' PRÓXIMO AO ',n=2) %>% as.data.frame()

stops <- data.table::setDT(stops)[, logradouro := df$V1]
stops <- stops[, numero := df$V2]
stops <- stops[,.(logradouro,numero,stop_lat,stop_lon)]

stops <- stops[, logradouro := gsub("Ç","C",logradouro)] 
stops <- stops[, logradouro := gsub("Ã","A",logradouro)] 
stops <- stops[, logradouro := gsub("Á","A",logradouro)] 
stops <- stops[, logradouro := gsub("À","A",logradouro)] 
stops <- stops[, logradouro := gsub("Â","A",logradouro)] 
stops <- stops[, logradouro := gsub("Ä","A",logradouro)] 
stops <- stops[, logradouro := gsub("É","E",logradouro)] 
stops <- stops[, logradouro := gsub("Ê","E",logradouro)] 
stops <- stops[, logradouro := gsub("È","E",logradouro)] 
stops <- stops[, logradouro := gsub("Ë","E",logradouro)] 
stops <- stops[, logradouro := gsub("Í","I",logradouro)] 
stops <- stops[, logradouro := gsub("Ì","I",logradouro)] 
stops <- stops[, logradouro := gsub("Î","I",logradouro)] 
stops <- stops[, logradouro := gsub("Ï","I",logradouro)] 
stops <- stops[, logradouro := gsub("Ó","O",logradouro)] 
stops <- stops[, logradouro := gsub("Ò","O",logradouro)] 
stops <- stops[, logradouro := gsub("Ô","O",logradouro)] 
stops <- stops[, logradouro := gsub("Ö","O",logradouro)] 
stops <- stops[, logradouro := gsub("Õ","O",logradouro)] 
stops <- stops[, logradouro := gsub("Ú","U",logradouro)] 
stops <- stops[, logradouro := gsub("Û","U",logradouro)] 
stops <- stops[, logradouro := gsub("Ù","U",logradouro)] 
stops <- stops[, logradouro := gsub("Ü","U",logradouro)] 

readr::write_rds(stops, here::here('geocode','GTFS','gtfs_rjo.rds'))

# cur
stops <- readr::read_csv(paste0(path_gtfs,'cur/stops.txt')) %>% data.table::setDT()

stops <- stops[,.(stop_name,stop_lat,stop_lon)]
stops <- stops[, stop_name := stringr::str_to_upper(stop_name)]

df1 <- stringr::str_split_fixed(stops$stop_name,',',n=2) %>% as.data.frame()
df2 <- stringr::str_split_fixed(df1$V2,' - ',n=2) %>% as.data.frame()

stops <- stops[, logradouro := df1$V1]
stops <- stops[, numero := df2$V1]
stops <- stops[, localidade := df2$V2]
stops <- stops[,.(logradouro,numero,localidade,stop_lat,stop_lon)]

stops <- stops[, logradouro := gsub('^R. ','RUA ',logradouro)]
stops <- stops[, logradouro := gsub('^R ','RUA ',logradouro)]
stops <- stops[, logradouro := gsub('^AV. ','AVENIDA ',logradouro)]
stops <- stops[, logradouro := gsub('^AV ','AVENIDA ',logradouro)]
stops <- stops[, logradouro := gsub('^ESTR. ','ESTRADA ',logradouro)]
stops <- stops[, logradouro := gsub('^PÇA. ','PRACA ',logradouro)]
stops <- stops[, logradouro := gsub('^ROD. ','RODOVIA ',logradouro)]
stops <- stops[, logradouro := gsub('^ROD ','RODOVIA ',logradouro)]
stops <- stops[, logradouro := gsub('^AC. ','ACESSO ',logradouro)]
stops <- stops[, logradouro := gsub('^AL. ','ALAMEDA ',logradouro)]

stops <- stops[, logradouro := gsub(" DR. "," DOUTOR ",logradouro)] 
stops <- stops[, logradouro := gsub(" PROF. "," PROFESSOR ",logradouro)] 
stops <- stops[, logradouro := gsub(" ENG. "," ENGENHEIRO ",logradouro)] 
stops <- stops[, logradouro := gsub(" PRES. "," PRESIDENTE ",logradouro)] 
stops <- stops[, logradouro := gsub(" CEL. "," CORONEL ",logradouro)] 
stops <- stops[, logradouro := gsub(" BRIG. "," BRIGADEIRO ",logradouro)] 
stops <- stops[, logradouro := gsub(" TN. "," TENENTE ",logradouro)]
stops <- stops[, logradouro := gsub(" MAL. "," MARECHAL ",logradouro)] 
stops <- stops[, logradouro := gsub(" MAJ. "," MAJOR ",logradouro)] 
stops <- stops[, logradouro := gsub(" TEN. "," TENENTE ",logradouro)] 
stops <- stops[, logradouro := gsub(" CAP. "," CAPITAO ",logradouro)] 
stops <- stops[, logradouro := gsub(" GEN. "," GENERAL ",logradouro)] 
stops <- stops[, logradouro := gsub(" DEP. "," DEPUTADO ",logradouro)] 
stops <- stops[, logradouro := gsub(" SEN. "," SENADOR ",logradouro)]
stops <- stops[, logradouro := gsub(" GOV. "," GOVERNADOR ",logradouro)]
stops <- stops[, logradouro := gsub(" DES. "," DESEMBARGADOR ",logradouro)] 
stops <- stops[, logradouro := gsub(" VER. "," VEREADOR ",logradouro)] 
stops <- stops[, logradouro := gsub(" NSA. SRA. "," NOSSA SENHORA ",logradouro)] 
stops <- stops[, logradouro := gsub(" VISC. "," VISCONDE ",logradouro)] 
stops <- stops[, logradouro := gsub("ESTAÇÃO TUBO "," ",logradouro)] 
stops <- stops[, logradouro := gsub(" PROFª. "," PROFESSORA ",logradouro)] 
stops <- stops[, logradouro := gsub(" PROFA. "," PROFESSORA ",logradouro)] 
stops <- stops[, logradouro := gsub(" ALM. "," ALMIRANTE ",logradouro)] 
stops <- stops[, logradouro := gsub(" JORN. "," JORNALISTA ",logradouro)] 
stops <- stops[, logradouro := gsub(" SRA. "," SENHORA ",logradouro)] 

stops <- stops[, logradouro := gsub("Ç","C",logradouro)] 
stops <- stops[, logradouro := gsub("Ã","A",logradouro)] 
stops <- stops[, logradouro := gsub("Á","A",logradouro)] 
stops <- stops[, logradouro := gsub("À","A",logradouro)] 
stops <- stops[, logradouro := gsub("Â","A",logradouro)] 
stops <- stops[, logradouro := gsub("Ä","A",logradouro)] 
stops <- stops[, logradouro := gsub("É","E",logradouro)] 
stops <- stops[, logradouro := gsub("Ê","E",logradouro)] 
stops <- stops[, logradouro := gsub("È","E",logradouro)] 
stops <- stops[, logradouro := gsub("Ë","E",logradouro)] 
stops <- stops[, logradouro := gsub("Í","I",logradouro)] 
stops <- stops[, logradouro := gsub("Ì","I",logradouro)] 
stops <- stops[, logradouro := gsub("Î","I",logradouro)] 
stops <- stops[, logradouro := gsub("Ï","I",logradouro)] 
stops <- stops[, logradouro := gsub("Ó","O",logradouro)] 
stops <- stops[, logradouro := gsub("Ò","O",logradouro)] 
stops <- stops[, logradouro := gsub("Ô","O",logradouro)] 
stops <- stops[, logradouro := gsub("Ö","O",logradouro)] 
stops <- stops[, logradouro := gsub("Õ","O",logradouro)] 
stops <- stops[, logradouro := gsub("Ú","U",logradouro)] 
stops <- stops[, logradouro := gsub("Û","U",logradouro)] 
stops <- stops[, logradouro := gsub("Ù","U",logradouro)] 
stops <- stops[, logradouro := gsub("Ü","U",logradouro)] 

readr::write_rds(stops, here::here('geocode','GTFS','gtfs_sal.rds'))

# sal
stops <- readr::read_csv(paste0(path_gtfs,'bsb/stops.txt')) %>% data.table::setDT()

df <- stringr::str_split_fixed(stops$stop_name,' - ',n=2) %>% as.data.frame()

stops <- data.table::setDT(stops)[, logradouro := df$V1]
stops <- stops[, localidade := df$V2]
stops <- stops[,.(logradouro,localidade,stop_lat,stop_lon)]

gtfs <- gtfs2gps::read_gtfs(paste0(path_gtfs,'rec.zip'))
