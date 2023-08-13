library(tidyverse)
library(vroom)
library(numbersBR)

arquivos <- fs::dir_ls(glob = "*.csv", path = "dados/dados_covid/")

dados_covid <- vroom(arquivos, locale = locale(encoding = "UTF-8"))

range(dados_covid$paciente_dataNascimento, na.rm = T)
range(dados_covid$vacina_dataAplicacao)

data_aplicacao <- dados_covid %>% 
                    filter(vacina_dataAplicacao > "2023-07-10")

paciente_idade <- dados_covid %>% 
                    filter(paciente_idade > 115)



tab_resumo <- dados_covid %>% 
                filter(paciente_dataNascimento < "1908-01-01")

tab_resumo_paciente <- dados_covid %>% 
                      group_by(paciente_id) %>% 
                      summarise(total = n()) %>% 
                      filter(total > 6)

casos_suspeitos11 <- dados_covid %>% 
                    select(vacina_lote) %>% 
                    filter(nchar(vacina_lote) == 11) %>% 
                    filter(str_detect(vacina_lote, pattern = regex("[:digit:]{11}")))

casos_suspeitos11$validado <- numbersBR::is.valid(CPF(as.numeric(casos_suspeitos11$vacina_lote)))

casos_suspeitos14 <- dados_covid %>% 
                        select(vacina_lote) %>% 
                        filter(nchar(vacina_lote) == 14) %>% 
                        filter(str_detect(vacina_lote, pattern = regex("[:digit:]{3}[:punct:][:digit:]{3}"))) %>% 
                        mutate(vacina_lote = str_remove_all(vacina_lote, pattern = regex("[:punct:]")))

casos_suspeitos14$vacina_lote <- numbersBR::is.valid(CPF(as.numeric(casos_suspeitos14$vacina_lote)))


saveRDS(tab_resumo_doses, file = "dados/tab_doses_vacinas.RDS")
saveRDS(tab_resumo_fabricante, file = "dados/tab_fabricantes_vacinas.RDS")
saveRDS(tab_resumo_paciente, file = "dados/tab_pacientes_doses.RDS")
saveRDS(paciente_idade, file = "dados/tab_paciente_idade.RDS")
saveRDS(data_aplicacao, file = "dados/data_aplicacao.RDS")
