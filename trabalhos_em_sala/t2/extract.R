


classes <- c('', 'CATIVO', 'RESIDENCIAL', 'INDUSTRIAL', 'COMERCIAL', 'OUTROS')

consumo <- list()
for (cl in classes){
  if (stringr::str_length(cl)>0){
    sheet <- sprintf('CONSUMO %s POR UF', cl)
    ind = cl
  } else {
    sheet <- 'CONSUMO POR UF'
    ind = 'TOTAL'
  }
  consumo[[ind]] <- readxl::read_xls(path.expand("~/Documents/fgv/TimeSeries/T1/dados.xls"),
                                     sheet = sheet,
                                     range = 'A8:HC34',
                                     col_names = F)
}

consumo$RESIDENCIAL%>% 
  filter(`...1` == 'Espírito Santo') %>% 
  select(-`...1`) %>% t %>% 
  ts(start=c(2004,1), end = c(2021,6), frequency=12) -> res

consumo$RESIDENCIAL%>% 
  filter(`...1` == 'Minas Gerais') %>% 
  select(-`...1`) %>% t %>% 
  ts(start=c(2004,1), end = c(2021,6), frequency=12) -> residencial.mg

consumo$RESIDENCIAL%>% 
  filter(`...1` == 'São Paulo') %>% 
  select(-`...1`) %>% t %>% 
  ts(start=c(2004,1), end = c(2021,6), frequency=12) -> residencial.sp

consumo$RESIDENCIAL%>% 
  filter(`...1` == 'Bahia') %>% 
  select(-`...1`) %>% t %>% 
  ts(start=c(2004,1), end = c(2021,6), frequency=12) -> residencial.bh



