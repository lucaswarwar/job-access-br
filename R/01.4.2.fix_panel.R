


source('setup.R')

panel <- readr::read_rds(here::here('data','panel','panel_final_anon.rds')) %>% data.table::setDT()

# Fix dt_nasc
panel <- panel[, dt_nasc := data.table::fifelse(!is.na(dt_nasc), dt_nasc,data_nasc)]

dt_nasc <- panel[,.(id_pessoa,dt_nasc)] %>% unique()
dt_nasc <- dt_nasc[dt_nasc != ""]
dt_nasc <- dt_nasc[, date := lubridate::as_date(dt_nasc)]

dt_nasc_ok <- dt_nasc[!is.na(date)]

dt_nasc <- dt_nasc[id_pessoa %nin% dt_nasc_ok$id_pessoa]

dt_nasc <- dt_nasc[, year := stringr::str_sub(dt_nasc,-4,-1)]
dt_nasc <- dt_nasc[year != '1899']

dt_nasc <- dt_nasc[, day := stringr::str_sub(dt_nasc,1,2)]
dt_nasc <- dt_nasc[, day := data.table::fifelse(stringr::str_sub(day,-1,-1)=="/",paste0(0,day),day)]
dt_nasc <- dt_nasc[, day := gsub('/','',day)]

dt_nasc <- dt_nasc[, month := stringr::str_sub(dt_nasc,3,5)]
dt_nasc <- dt_nasc[, month := gsub('/','',month)]

dt_nasc <- dt_nasc[, month := gsub('jan','01',month)]
dt_nasc <- dt_nasc[, month := gsub('feb','02',month)]
dt_nasc <- dt_nasc[, month := gsub('mar','03',month)]
dt_nasc <- dt_nasc[, month := gsub('apr','04',month)]
dt_nasc <- dt_nasc[, month := gsub('may','05',month)]
dt_nasc <- dt_nasc[, month := gsub('jun','06',month)]
dt_nasc <- dt_nasc[, month := gsub('jul','07',month)]
dt_nasc <- dt_nasc[, month := gsub('aug','08',month)]
dt_nasc <- dt_nasc[, month := gsub('sep','09',month)]
dt_nasc <- dt_nasc[, month := gsub('oct','10',month)]
dt_nasc <- dt_nasc[, month := gsub('nov','11',month)]
dt_nasc <- dt_nasc[, month := gsub('dec','12',month)]

dt_nasc <- dt_nasc[, month := data.table::fifelse(as.numeric(month) > 12 & stringr::str_sub(month,-1,-1) == '1',
                                                  paste0(0,stringr::str_sub(month,-2,-2)),
                                                  month)]

dt_nasc <- dt_nasc[, date := data.table::fifelse(as.numeric(month) > 12,
                                                 paste(year,day,month,sep = '-'),
                                                 paste(year,month,day,sep = '-'))]

dt_nasc <- dt_nasc[,.(id_pessoa,date)] %>% unique()

dt_nasc <- dt_nasc %>% group_by(id_pessoa) %>% 
                       mutate(n=n())

dt_nasc_ok2 <- dt_nasc %>% dplyr::filter(n==1) %>% setDT()
dt_nasc_ok2 <- dt_nasc_ok2[,date:=lubridate::as_date(date)]
dt_nasc_ok2 <- dt_nasc_ok2[,.(id_pessoa,date)]

dt_nasc_ok <- dplyr::bind_rows(dt_nasc_ok[,.(id_pessoa,date)],dt_nasc_ok2) %>% setDT(key='id_pessoa')
rm(dt_nasc_ok2)

dt_nasc <- dt_nasc %>% dplyr::filter(n>1) %>% setDT()
dt_nasc <- dt_nasc[,.(id_pessoa,date)]
dt_nasc <- dt_nasc[, date := lubridate::as_date(date)]
dt_nasc <- dt_nasc[, n := rank(date, ties.method="first"), by = 'id_pessoa'][n == 1]
dt_nasc <- dt_nasc[,.(id_pessoa,date)]

dt_nasc_ok <- dplyr::bind_rows(dt_nasc_ok,dt_nasc) %>% setDT(key='id_pessoa')

panel <- data.table::merge.data.table(panel,
                                      dt_nasc_ok,
                                      all.x = TRUE,
                                      by = 'id_pessoa')