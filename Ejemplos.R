#install.packages("eph")                 # Versión oficial CRAN
#devtools::install_github("holatam/eph") # Versión en desarrollo. Instalar previamente "devtools"
#install.packages("tidyverse")           # Si no instalaron aún tidyverse
#?eph                                    # Acceder a documentación del paquete

library(eph)
library(tidyverse)

##########################Descarga de bases#################
base_eph_2019_t4 <- get_microdata(year = 2019,
                                 trimester = 4,
                                 type = "individual")
rm(base_eph_2019_t4)

variables_interes <- c(
  "ANO4","TRIMESTRE","CODUSU","NRO_HOGAR","COMPONENTE",
  "AGLOMERADO","REGION","ESTADO","CAT_OCUP",
  "CH04","CH06","NIVEL_ED","PP04B_COD","PP04D_COD","P21","ITF",
  "PONDERA","PONDIIO","PONDIH")

base_eph_select<- get_microdata(year = 2019,
                                trimester = 4,
                                vars = variables_interes,
                                destfile = "ind_2019_t4_select.rds")

########Etiquetado y clasificación de variables###############
base_trabajo <- base_eph_select %>% 
  organize_labels()


#df_caes <-  eph::caes
#df_cno <- eph::CNO

base_trabajo <- base_trabajo %>% 
  organize_cno()

base_trabajo <- base_trabajo %>% 
  organize_caes()

############Tabulados########################
calculate_tabulates(base = base_trabajo,
                    x = "JERARQUIA",
                    y = "CH04",
                    weights = "PONDERA",
                    add.percentage = "col")

nivel_ed_rama <- calculate_tabulates(base = base_trabajo,
                    x = "caes_seccion_label",
                    y = "NIVEL_ED",
                    weights = "PONDERA",add.percentage = "row")

############ Espacio para preguntas ######################


############## Map Agglomerates #################
tasas <- base_trabajo %>%
  group_by(AGLOMERADO) %>%
  summarise(tasa_actividad = sum(PONDERA[ESTADO==1])/sum(PONDERA)) 


map_agglomerates(.data = tasas,
                 agglomerates = "AGLOMERADO",
                 indicator = tasa_actividad)

############## Pobreza ##########################
valoriz_canastas_trimestral <- get_poverty_lines(regional = T)

base_eph_select <- calculate_poverty(
  base = base_eph_select,
  basket = valoriz_canastas_trimestral)

rm(base_eph_select)
rm(base_trabajo)
############## Descarga de multiples bases #######
bases_2018_2019<- get_microdata(year = 2018:2019,
                                trimester = 1:4,
                                destfile = "bases_2018_2019.rds")

bases_2018_2019 <- bases_2018_2019 %>%
  select(microdata) %>%
  unnest(cols = c(microdata)) 


bases_2018_2019 <- calculate_poverty(
  base = bases_2018_2019,
  basket = valoriz_canastas_trimestral)

############## Paneles ###########################
pool_trimestral <- organize_panels(
  bases = bases_2018_2019,
  variables = variables_interes,
  window = "trimestral")

pool_trimestral <- pool_trimestral %>% 
  organize_labels()

calculate_tabulates(base = pool_trimestral,
                    x = "ESTADO",
                    y = "ESTADO_t1",
                    weights = "PONDERA",
                    add.percentage = "row")

################ Dataframes accesorios########################
# eph::caes
# eph::CNO
# eph::adulto_equivalente
# eph::canastas_reg_example
# eph::centroides_aglomerados
# eph::diccionario_aglomerados
# eph::diccionario_regiones
# eph::toybase_individual_2016_04
 