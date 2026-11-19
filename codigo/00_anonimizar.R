# Asigna un código de participante (P01, P02, ...) a cada persona y genera
# los datos crudos que usa el informe.
#
# Entrada (solo local, no se versiona): datos/originales/
#   - base_datos_encuesta.csv       (encuesta de Google Forms)
#   - base_datos_encuesta_excel.xlsx (hoja "Diccionario")
#   - base_votaciones_juego.xlsx     (hojas "votaciones" y "diccionario")
# Salida (se versiona): datos/
#   - encuesta.csv, juego.csv, diccionario_encuesta.csv, diccionario_juego.csv
# Además guarda la correspondencia correo -> código en datos/originales/,
# que tampoco se versiona.
#
# Regla del código: el correo se pasa a minúscula y sin espacios externos;
# los códigos se numeran por orden de primera aparición en la encuesta y
# después en el juego. La misma persona recibe el mismo código en ambos.

library(dplyr)
library(readr)
library(readxl)
library(here)

orig <- here("datos/originales")
dir.create(here("datos"), showWarnings = FALSE, recursive = TRUE)

encuesta <- read_csv(file.path(orig, "base_datos_encuesta.csv"),
                     show_col_types = FALSE)
juego <- read_excel(file.path(orig, "base_votaciones_juego.xlsx"),
                    sheet = "votaciones")
dic_encuesta <- read_excel(file.path(orig, "base_datos_encuesta_excel.xlsx"),
                           sheet = "Diccionario")

normalizar <- function(x) tolower(trimws(x))

claves <- unique(c(normalizar(encuesta$email), normalizar(juego$identificador)))
correspondencia <- tibble(
  clave = claves,
  id = sprintf("P%02d", seq_along(claves))
)

encuesta_anon <- encuesta |>
  mutate(clave = normalizar(email)) |>
  left_join(correspondencia, by = "clave") |>
  select(id, everything(), -email, -clave)

juego_anon <- juego |>
  mutate(clave = normalizar(identificador)) |>
  left_join(correspondencia, by = "clave") |>
  select(id, everything(), -identificador, -clave)

stopifnot(!anyNA(encuesta_anon$id), !anyNA(juego_anon$id))

# Diccionario de la encuesta: la fila del correo pasa a describir el código.
dic_encuesta <- dic_encuesta |>
  mutate(across(everything(), as.character))
fila_id <- dic_encuesta$variable == "email"
dic_encuesta$variable[fila_id] <- "id"
dic_encuesta$tipo[fila_id] <- "Texto nominal"
dic_encuesta[fila_id, 3] <- "Código de participante (P01, P02, ...)"
dic_encuesta[fila_id, -(1:3)] <- NA

# Diccionario del juego: con los nombres de las columnas actuales.
dic_juego <- tibble(
  variable = c("id", "ronda1_eleccion", "ronda1_seguridad",
               "ronda2_eleccion", "ronda2_seguridad"),
  descripcion = c("Código de participante (P01, P02, ...), el mismo que en la encuesta",
                  "Actividad elegida antes de mostrar resultados",
                  "Seguridad de la elección antes de mostrar resultados",
                  "Actividad elegida luego de conocer los resultados del grupo",
                  "Seguridad de la elección de la segunda ronda"),
  tipo = c("Texto nominal", "Categórica nominal", "Ordinal",
           "Categórica nominal", "Ordinal"),
  codificacion = c("P + número de dos cifras",
                   "1=Lógica; 2=Mímica; 3=Dibujo; 4=Memoria",
                   "1=Mínima; 2; 3; 4; 5=Máxima",
                   "1=Lógica; 2=Mímica; 3=Dibujo; 4=Memoria",
                   "1=Mínima; 2; 3; 4; 5=Máxima"),
  faltantes = c("No admite", "Vacío sin respuesta", "Vacío sin respuesta",
                "Vacío sin respuesta", "Vacío sin respuesta")
)

write_csv(encuesta_anon, here("datos/encuesta.csv"), na = "")
write_csv(juego_anon, here("datos/juego.csv"), na = "")
write_csv(dic_encuesta, here("datos/diccionario_encuesta.csv"), na = "")
write_csv(dic_juego, here("datos/diccionario_juego.csv"), na = "")
write_csv(correspondencia, file.path(orig, "correspondencia_codigos.csv"))
