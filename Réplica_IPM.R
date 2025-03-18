--------------------------------------------------------------------------------
# REPLICACIÓN del ÍNDICE DE POBREZA MULTIDIMENSIONAL (IPM) A DICIEMBRE DE 2023
--------------------------------------------------------------------------------

# Fuente: Adaptado según la Ficha Técnica del Instituto Nacional de Esatadísticas
#        y Censos (INEC, 2024)
# Adaptado por: Ec. Fernando Yunga
  
# 1. Cargar las siguientes librerías o instalarlas en el caso de ser necesario:
  
library(haven)
library(tidyverse)
library(srvyr)
library(data.table)
library(writexl)

# 2. Importar la bases de personas (enemdu_persona_aaaa_12) y vivienda (enemdu_viv_hog_aaaa_12)
# Formato SPSS (.sav):

per <- read_sav("/enemdu_persona_2023_12.sav") #Agregar el directorio correspondiente
viv<- read_sav("/enemdu_vivienda_hogar_2023_12.sav") #Agregar el directorio correspondiente

# 3. Unión de base de personas y hogares(vivienda) y creación de variables (PEA, PEI Y PET)
#    PEA=Población Económicamente Activa
#    PEI=Población Económicamente Inactiva
#    PET=Población con Edad de Trabajar

viv <- bind_cols(
  viv %>% select("id_hogar"),
  viv %>% select(-intersect(names(per), names(viv))))

bdd <- left_join(per, viv, by = "id_hogar", keep = FALSE) %>%
  mutate(
    pea = if_else(condact >= 1 & condact <= 8, 1, NA_real_),
    pei = if_else(condact == 9, 1, NA_real_),
    pet = if_else(condact >= 1 & condact <= 9, 1, NA_real_))

# **************** A. OBTENCIÓN DE DIMENSIONES E INDICADORES ********************

## DIMENSIÓN 1: EDUCACIÓN-------------------------------------------------------

### 1. Inasistencia a educación básica y bachillerato
# Población de referencia: Niños (5-14 años) y Adolescentes (15-17 años) 
# Privación:
# (5-14 años): No asiste a centro de educación básica.
# (15-17 años): Completó educación básica y no asiste al bachillerato.

bdd <- bdd %>% mutate(
  asist_basica = # a) Asistencia a educación básica
    case_when(
      between(p03, 5, 14) & (p07 == 1) &
        ((p10a == 1 | p10a == 3) | (p10a == 4 & between(p10b, 0, 6)) |
           (p10a == 5 & between(p10b, 0, 9)) |
           (p10a == 6 & between(p10b, 0, 2))) ~ 1,
      inrange(p03, 5, 14) ~ 0,
      T ~ NA_real_
    ),
  asist_bach = # b) Asistencia a educación bachillerato
    case_when(
      between(p03, 15, 17) & (p07 == 1) &
        ((p10a == 5 & p10b == 10) | (p10a == 7 & inrange(p10b, 0, 2)) |
           (p10a == 6 & inrange(p10b, 3, 5))) ~ 1,
      inrange(p03, 15, 17) ~ 0,
      T ~ NA_real_
    ),
  dim1_ind1 = # c) Cálculo de la privación
    case_when(
      inrange(p03, 5, 17) & p10a < 8 ~ 1,
      T ~ NA_real_
    ),
  dim1_ind1 =
    case_when(
      inrange(p03, 5, 17) & (asist_basica == 1 |
                               asist_bach == 1 | p10a >= 8) ~ 0,
      T ~ dim1_ind1
    )
)

### 2. No acceso a educación superior por razones económicas
# Población de referencia: Jóvenes (18-29 años) bachilleres.
# Privación:
# No asiste a un establecimiento de educación superior por falta de recursos económicos.

bdd <- bdd %>% mutate(
  dim1_ind2 =
    case_when(
      inrange(p03, 18, 29) & (p07 >= 1 & p07 <= 2) &
        ((p10a == 7 & p10b >= 3) | (p10a == 6 & p10b >= 6)) |
        inrange(p10a, 8, 10) ~ 0,
      T ~ NA_real_
    ),
  dim1_ind2 =
    case_when(
      inrange(p03, 18, 29) & p07 == 2 &
        ((p10a == 7 & p10b >= 3) | (p10a == 6 & p10b >= 6) |
           (inrange(p10a, 8, 9))) & p09 == 3 ~ 1,
      p03 < 18 | p03 > 29 ~ NA_real_,
      T ~ dim1_ind2
    )
)

### 3. Logro educativo incompleto
# Población de referencia: Personas de 18 a 64 años.
# Privación:
# Personas con menos de 10 años de escolaridad que no asisten a centros de educación formal.
#### a) Años promedio de escolaridad

bdd <- bdd %>% mutate(
  escol =
    case_when(
      p10a == 1 ~ 0,
      inrange(p10b, 0, 3) & p10a == 2 ~ 2 * p10b,
      inrange(p10b, 4, 10) & p10a == 2 ~ 3 + p10b,
      p10a == 3 ~ 1,
      p10a == 4 ~ 1 + p10b,
      p10a == 5 ~ p10b,
      p10a == 6 ~ 7 + p10b,
      p10a == 7 ~ 10 + p10b,
      p10a == 8 | p10a == 9 ~ 13 + p10b,
      p10a == 10 ~ 18 + p10b,
      T ~ NA_real_
    )
)
#### b) Cálculo de la privación
bdd <- bdd %>% mutate(
  dim1_ind3 =
    if_else(inrange(p03, 18, 64) & ((escol < 10 & p07 == 1) | (escol >= 10)), 0, NA_real_),
  dim1_ind3 = case_when(
    inrange(p03, 18, 64) & escol < 10 & p07 == 2 ~ 1,
    T ~ dim1_ind3
  )
)

## DIMENSIÓN 2: TRABAJO Y SEGURIDAD SOCIAL--------------------------------------

### 1. Empleo infantil y adolescente
# Población de referencia: Niños (5-14 años) y Adolescentes (15-17 años)
# Privación:
# Niños (5-14 años): Ocupado en la semana de referencia
# Adolescentes (15-17 años):
# Ocupado en la semana de referencia y cumple una de las siguientes condiciones:
# i) Recibe una remuneración inferior al Salario Básico Unificado
# ii) No asisten a clases
# iii) Trabaja más de 30 horas.

bdd <- bdd %>% mutate(
  horas = if_else(empleo == 1, 0, NA_real_),
  horas = if_else(pea == 1 & (p20 == 1 | (p20 == 2 & p21 <= 11)), p24, horas)
)
bdd <- bdd %>% mutate(
  across(
    where(is.double),
    ~ if_else(as.numeric(.x) == 999, NA_real_, as.numeric(.x))
  ),
  across(
    where(is.factor),
    ~ if_else(as.numeric(.x) == 999, NA_real_, as.numeric(.x))
  )
)
bdd$hh <- rowSums(select(bdd, starts_with("p51")), na.rm = TRUE)
bdd <- bdd %>% mutate(
  hh = if_else(hh <= 0, NA_real_, hh),
  horas = if_else(pea == 1 & p20 == 2 & p21 == 12 & p22 == 1, hh, horas),
  dim2_ind1 = case_when(
    ((p20 == 1 | inrange(p21, 1, 11) | p22 == 1) & inrange(p03, 5, 14)) |
      (inrange(condact, 2, 6) & inrange(p03, 15, 17)) ~ 1,
    p03 >= 18 ~ NA_real_,
    T ~ 0
  ),
  dim2_ind1 = if_else(condact == 1 & inrange(p03, 15, 17) & (p07 == 2 | horas > 30), 1, dim2_ind1)
)

### 2. Desempleo o empleo inadecuado
# Población de referencia: Población de 18 años y más.
# Privación: Personas en condición de desempleo o empleo inadecuado.
bdd <- bdd %>%
  mutate(
    dim2_ind2 =
      case_when(
        inrange(condact, 2, 8) & inrange(p03, 18, 98) ~ 1,
        p03 < 18 | p03 == 99 |
          (p20) & (is.na(p21) & is.na(p22) & is.na(p32) & is.na(p34) & is.na(p35)) ~ NA_real_,
        T ~ 0
      )
  )

### 3. No contribución al sistema de pensiones
# Población de referencia: Personas de 15 años y más.
# Privación:
# Ocupados que no contribuyen al sistema de seguridad social. Se excluye a ocupados de 65 años y más,
# que no aportan, pero reciben pensión por jubilación.
# Desocupados e inactivos mayores de 65 años que:
# No recibe pensión por jubilación, BDH y Bono Joaquín Gallegos Lara (BJGL).

bdd <- bdd %>% mutate(
  dim2_ind3 = # Personas ocupadas
    case_when(
      empleo == 1 & p03 >= 65 & p72a == 1 ~ 0,
      (empleo == 1 & inrange(p05a, 5, 10) & inrange(p05b, 5, 10)) ~ 1,
      T ~ 0
    )
)

bdd <- bdd %>% mutate(
  dim2_ind3 = # Personas desocupadas o PEI mayores de 65 años
    case_when(
      ((desempleo == 1 | pei == 1) & p03 >= 65 & p72a == 2) ~ 1,
      T ~ dim2_ind3
    ),
  dim2_ind3 =
    if_else((p03 >= 65 & p72a == 2 & p75 == 1) | (pet == 1 & p77 == 1), 0, dim2_ind3),
  dim2_ind3 =
    if_else(pet == 0 | inrange(p03, 0, 14), NA_real_, dim2_ind3)
)

# DIMENSIÓN 3: SALUD, AGUA Y ALIMENTACIÓN --------------------------------------

bdd <- bdd %>%
  mutate(
    dim3_ind1 = # Pobreza extrema por ingresos: Ingreso per cápita familiar
      epobreza, # es inferior al de la línea de pobreza extrema.
    dim3_ind2 = # Sin servicio de agua por red pública: Viviendas que obtienen
      case_when(
        vi10 != 1 ~ 1, # el agua por un medio distinto al de la red pública
        is.na(vi10) ~ NA_real_,
        T ~ 0
      )
  )

# DIMENSIÓN 4: HABITAT, VIVIENDA Y AMBIENTE SANO -------------------------------

## 1. Hacinamiento 
# Población de referencia: Toda la población.
# Privación:
# El número de personas por dormitorio exclusivo para dormir mayor a tres.

bdd <- bdd %>% mutate(vi07 = if_else(vi07 == 0, 1, vi07))
bdd <- bdd %>%
  group_by(id_hogar) %>%
  mutate(
    hsize =
      if_else(!is.na(p01), 1, NA_real_),
    hsize =
      sum(hsize)
  ) %>%
  ungroup() %>%
  mutate(dim4_ind1 = if_else((hsize / vi07) > 3, 1, 0))

## 2. Déficit habitacional
# Población de referencia: Toda la población
# Privación:
# Se consideran en déficit habitacional las personas cuya vivienda, debido a los materiales
# o estado de sus paredes, piso y techo, son consideradas en déficit cualitativo o cuantitativo .
### Material y condición del techo, pared, piso

bdd <- bdd %>% mutate(
  techo =
    case_when(
      (vi03a == 1 & inrange(vi03b, 1, 2)) |
        (vi03b == 1 & inrange(vi03a, 2, 4)) ~ 1,
      (vi03a == 1 & vi03b == 3) |
        vi03b == 2 & inrange(vi03a, 2, 4) ~ 2,
      (vi03b == 3 & inrange(vi03a, 2, 4)) |
        (vi03a == 5 | vi03a == 6) ~ 3
    ),
  pared =
    case_when(
      (vi05a == 1 & inrange(vi05b, 1, 2)) |
        (vi05a == 2 & vi05b == 1) ~ 1,
      (vi05a == 1 & vi05b == 3) | (vi05a == 2 & vi05b == 2) |
        (inrange(vi05a, 3, 5) & inrange(vi05b, 1, 2)) ~ 2,
      (vi05b == 3 & inrange(vi05a, 2, 5)) |
        (vi05a == 6 | vi05a == 7) ~ 3
    ),
  piso =
    case_when(
      (vi04a <= 3 & vi04b == 1) |
        (vi04a <= 3 & vi04b == 2) |
        (vi04b == 1 & inrange(vi04a, 4, 5)) ~ 1,
      (vi04a <= 3 & vi04b == 3) |
        (vi04b == 2 & inrange(vi04a, 4, 5)) |
        (vi04a == 6 & vi04b == 1) ~ 2,
      (vi04b == 3 & inrange(vi04a, 4, 6)) |
        (vi04a == 6 & vi04b == 2) |
        (vi04a == 7 | vi04a == 8) ~ 3
    )
)
### Tipología de Vivienda
bdd <- bdd %>% mutate(
  tipviv1 =
    case_when(
      (techo == 1 & pared == 1 & inrange(piso, 1, 2)) |
        (techo == 1 & pared == 2 & piso == 1) ~ 1,
      (techo == 1 & pared == 1 & piso == 3) |
        (techo == 1 & pared == 2 & inrange(piso, 2, 3)) |
        (techo == 1 & pared == 3 & inrange(piso, 1, 2)) |
        (techo == 2 & pared == 1 & inrange(piso, 1, 2)) |
        (techo == 2 & pared == 2 & inrange(piso, 1, 2)) |
        (techo == 3 & pared == 1 & inrange(piso, 1, 2)) |
        (techo == 3 & pared == 2 & piso == 1) ~ 2,
      (techo == 1 & pared == 3 & piso == 3) |
        (techo == 2 & pared == 1 & piso == 3) |
        (techo == 2 & pared == 2 & piso == 3) |
        (techo == 2 & pared == 3 & inrange(piso, 1, 3)) |
        (techo == 3 & pared == 1 & piso == 3) |
        (techo == 3 & pared == 2 & inrange(piso, 2, 3)) |
        (techo == 3 & pared == 3 & inrange(piso, 1, 3)) ~ 3
    )
)

### Personas que habitan en viviendas con déficit habitacional
bdd <- bdd %>% mutate(
  dim4_ind2 =
    case_when(
      tipviv1 == 3 | tipviv1 == 2 ~ 1,
      tipviv1 == 1 ~ 0,
      T ~ NA_real_
    )
)

## 3. Sin saneamiento de excretas
# Población de referencia: Toda la población.
# Privación:
# Área urbana: Vivienda con inodoro conectado a pozo séptico, pozo ciego,
# letrina o no tiene ningún servicio higiénico.
# Área rural: Vivienda con inodoro conectado a pozo ciego, letrina o no tiene ningún servicio higiénico.

bdd <- bdd %>% mutate(
  dim4_ind3 =
    case_when(
      (area == 1 & inrange(vi09, 2, 5)) |
        (area == 2 & inrange(vi09, 3, 5)) ~ 1,
      is.na(vi09) | is.na(area) ~ NA_real_,
      T ~ 0
    )
)

## 4. Sin servicio de recolección de basura
# Población de referencia: Toda la población.
# Privación: El hogar no cuenta con servicio municipal para eliminación de basura.

bdd <- bdd %>% mutate(
  dim4_ind4 =
    case_when(
      vi13 == 1 | inrange(vi13, 3, 5) ~ 1,
      is.na(vi13) ~ NA_real_,
      T ~ 0
    )
)

# ************** B. IDENTIFICACIÓN Y AGREGACIÓN DE PRIVACIONES *****************

bdd <- bind_cols(
  bdd %>% rename_at(vars(starts_with("dim")), ~ paste0(., "_p")),
  bdd %>% mutate(across(starts_with("dim"), ~ if_else(is.na(.x), 0, .x))) %>%
    group_by(id_hogar) %>%
    transmute(across(starts_with("dim"), ~ max(.x, na.rm = TRUE), .names = "{.col}_h")) %>%
    ungroup() %>% select(-starts_with("id_h"))
)
bdd2 <- bdd

## Tratamiento de valores extremos:
# Se excluyen hogares de la muestra si no pueden ser evaluados - Missing data

### 1. Educación
bdd <- bdd %>%
  mutate(
    miss_dim1_ind1 = if_else(inrange(p03, 5, 17), as.numeric(is.na(p07) + is.na(p10a)), NA_real_),
    miss_dim1_ind2 = if_else(inrange(p03, 18, 29), as.numeric(is.na(p07) + is.na(p10a)), NA_real_),
    miss_dim1_ind3 = if_else(inrange(p03, 18, 64), as.numeric(is.na(p07) + is.na(escol)), NA_real_)
  )
### 2. Trabajo y seguridad social
bdd <- bdd %>%
  mutate(
    miss_dim2_ind1 =
      case_when(
        is.na(p20) & is.na(p21) & is.na(p22) & inrange(p03, 5, 14) ~ 1,
        T ~ NA_real_
      ),
    miss_dim2_ind1 =
      case_when(
        is.na(condact) & inrange(p03, 15, 17) ~ 1,
        condact == 1 & inrange(p03, 15, 17) & (is.na(p07) | is.na(horas)) ~ 0,
        T ~ miss_dim2_ind1
      ),
    miss_dim2_ind2 =
      if_else(inrange(p03, 18, 98), as.numeric(is.na(condact)), NA_real_),
    miss_dim2_ind2 =
      case_when(
        is.na(p20) & is.na(p21) & is.na(p22) & is.na(p32) & is.na(p34) &
          is.na(p35) & inrange(p03, 18, 98) ~ 1,
        T ~ miss_dim2_ind2
      ),
    miss_dim2_ind3 =
      if_else(inrange(p03, 15, 98),
              as.numeric(is.na(p72a) + is.na(p75) + is.na(p77) + is.na(p05a) + is.na(p05b)),
              NA_real_
      )
  )
### 3. Salud, agua y alimentación
bdd <- bdd %>% mutate(
  miss_dim3_ind1 = as.numeric(is.na(epobreza)),
  miss_dim3_ind2 = as.numeric(is.na(vi10))
)
### 4. Hábitat, vivienda y ambiente sano
bdd <- bdd %>% mutate(
  miss_dim4_ind1 = as.numeric(is.na(hsize) + is.na(vi07)),
  miss_dim4_ind2 = as.numeric(is.na(vi03a) + is.na(vi03b) +
                                is.na(vi04a) + is.na(vi04b) +
                                is.na(vi05a) + is.na(vi05b)),
  miss_dim4_ind3 = as.numeric(is.na(vi09)),
  miss_dim4_ind4 = as.numeric(is.na(vi13))
)
bdd$miss_total <- rowSums(select(bdd, starts_with("miss_dim")), na.rm = TRUE)
bdd <- bdd %>% mutate(miss_total = if_else(inrange(miss_total, 1, max(miss_total)), 1, miss_total))
bdd <- bdd %>%
  select(-starts_with("miss_dim")) %>%
  filter(p03 != 99) %>%
  group_by(id_hogar) %>%
  mutate(miss_total = max(miss_total, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(miss_total != 1)

# ******************** C. ESTRUCTURA DE PONDERACIÓN ****************************

# Cada dimensión recibe un peso del 25% (1/4)
# y dentro de cada dimensión el peso se distribuye proporcionalmente

pesos <- data.frame(
    1 / 4 * 1 / 3, 1 / 4 * 1 / 3, 1 / 4 * 1 / 3,
    1 / 4 * 1 / 3, 1 / 4 * 1 / 3, 1 / 4 * 1 / 3,
    1 / 4 * 1 / 2, 1 / 4 * 1 / 2,
    1 / 4 * 1 / 4, 1 / 4 * 1 / 4, 1 / 4 * 1 / 4, 1 / 4 * 1 / 4)

colnames(pesos) <- c(paste0("wdim", rbind(1:12)))
bdd <- cbind(bdd, pesos)
n <- 1
c <- c(names(bdd %>% select(ends_with("_h"))))
dim <- numeric(12)
for (x in c) {
  dim[n] <- bdd %>% transmute(x = !!sym(x) * !!sym(paste0("wdim", n)))
  n <- n + 1
}
dim <- dim %>% as.data.frame()
colnames(dim) <- c(paste0("wp_", c))
bdd <- cbind(bdd, dim)
rm(dim, pesos)
bdd$ci <- rowSums(select(bdd, starts_with("wp_")), na.rm = TRUE)

# ******** D. LÍNEAS DE POBREZA MULTIDIMENSIONAL Y COMPONENTES DEL IPM *********

bddf <- bdd %>% mutate(
  TPM = # Tasa de Pobreza Multidimensional (k>=1/3)
    if_else(ci >= 4 / 12, 1, 0),
  TPEM = # Tasa de Pobreza Extrema Multidimensional (k>=1/2)
    if_else(ci >= 6 / 12, 1, 0),
  A = # Porcentaje Promedio de Privaciones (k>=1/3)
    if_else(TPM == 1, ci, NA_real_),
  IPM = # Índice de Pobreza Multidimensional (k>=1/3)
    if_else(TPM == 1, ci, 0))


# ************************ E. UNIÓN DE BASES DE DATOS **************************

bdd <- merge(per, bdd2 %>% select(ends_with("_p"), id_persona), by = "id_persona", all.x = TRUE)
bdd <- merge(bdd, bddf %>% select(TPM, TPEM, A, IPM,id_persona), by = "id_persona", all.x = TRUE)
bdd <- bdd %>% arrange(across(starts_with("id_")))

# ********************* F. CÁLCULO DEL IPM DE FORMA DESAGREGADA *****************

# Desagregaciones
bdd <- bdd %>% mutate(
  Nacional = 1,
  Área = area,
  Sexo = p02,
  Rangos_edad =
    case_when(
      !is.na(p03) & p03 != 99 & p03 < 15 ~ 1,
      !is.na(p03) & p03 != 99 & p03 >= 15 & p03 <= 24 ~ 2,
      !is.na(p03) & p03 != 99 & p03 >= 25 & p03 <= 34 ~ 3,
      !is.na(p03) & p03 != 99 & p03 >= 35 & p03 <= 44 ~ 4,
      !is.na(p03) & p03 != 99 & p03 >= 45 & p03 <= 64 ~ 5,
      !is.na(p03) & p03 != 99 & p03 >= 65 ~ 6,
      T ~ NA_real_
    ),
  Autoidentificación =
    case_when(
      p15 == 1 ~ 1,
      p15 == 2 | p15 == 3 | p15 == 4 ~ 2,
      p15 == 5 ~ 3,
      p15 == 6 ~ 4,
      p15 == 7 ~ 5,
      T ~ NA_real_
    )
)

Desagregaciones <- c("Nacional", "Área", "Sexo", "Rangos_edad", "Autoidentificación")
# Diseño Muestral
bdd <- bdd %>%
  as_survey_design(
    ids = upm,
    strata = estrato,
    weights = fexp,
    nest = T
  )
options(survey.lonely.psu = "certainty")
Pobr_Multi <- Desagregaciones %>%
  map_dfr(
    ~ bind_rows((bdd %>%
                   filter(!is.na(!!sym(.x))) %>%
                   group_by(!!sym(.x)) %>%
                   summarize(
                     TPM = survey_mean(TPM, na.rm = TRUE),
                     TPEM = survey_mean(TPEM, na.rm = TRUE),
                     A = survey_mean(A, na.rm = TRUE),
                     IPM = survey_mean(IPM, na.rm = TRUE)))) %>%
      mutate(Indicador = .x, Categorías = as.character(!!sym(.x))) %>%
      select(-.x)
  )
# NOTA: Para obtener los indicadores: TPM, TPEM, y A. 
# Únicamente en la línea del código 512, cambiar IPM por cualquiera de los indicadores.

Pobr_Multi <- Pobr_Multi %>%
  mutate(
    Categorías =
      case_when(
        Indicador == "Nacional" ~ "Nacional",
        Indicador == "Área" & Categorías == 1 ~ "Urbana",
        Indicador == "Área" & Categorías == 2 ~ "Rural",
        Indicador == "Sexo" & Categorías == 1 ~ "Hombre",
        Indicador == "Sexo" & Categorías == 2 ~ "Mujer",
        Indicador == "Rangos_edad" & Categorías == 1 ~ "Menor de 15 años",
        Indicador == "Rangos_edad" & Categorías == 2 ~ "15 a 24 años",
        Indicador == "Rangos_edad" & Categorías == 3 ~ "25 a 34 años",
        Indicador == "Rangos_edad" & Categorías == 4 ~ "35 a 44 años",
        Indicador == "Rangos_edad" & Categorías == 5 ~ "45 a 64 años",
        Indicador == "Rangos_edad" & Categorías == 6 ~ "65 y más",
        Indicador == "Autoidentificación" & Categorías == 1 ~ "Indígena",
        Indicador == "Autoidentificación" & Categorías == 2 ~ "Afroecuatoriano/a",
        Indicador == "Autoidentificación" & Categorías == 3 ~ "Montubio/a",
        Indicador == "Autoidentificación" & Categorías == 4 ~ "Mestizo/a",
        Indicador == "Autoidentificación" & Categorías == 5 ~ "Blanco/a",
        T ~ NA_character_
      )
  ) %>%
  select("Indicador", "Categorías", "IPM", "TPM", "TPEM", "A") %>%
  mutate(across(where(is.numeric), ~ .x * 100))
view(Pobr_Multi)

# *********************G. CóDIGO ADICIONAL O EXTRA *****************************

# Borrar algunos elementos en el caso de ser necesario
rm(bdd, bdd2, bddf, per, viv, n, x, c, Desagregaciones)

# Guardar Pobr_Multi como un archivo tipo R.data o en Excel.
save(IPM,file='Pobr_Multi.Rdata')
write_xlsx(IPM, "Pobr_Multi.xlsx")

