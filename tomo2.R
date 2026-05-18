# =========================================
# OCR PDF ESCANEADO - WINDOWS ROBUSTO
# PDF -> imagen 400 DPI -> OCR catalán
# Paralelizado con 4 cores
# =========================================

library(magick)
library(tesseract)
library(pdftools)
library(parallel)

# ----------------------------
# CONFIGURACIÓN
# ----------------------------

# Usa un nombre simple para evitar problemas con acentos, apóstrofos o símbolos
pdf_file <- normalizePath("pdf_aplec/Aplech_Tom_II_1925.pdf", mustWork = TRUE)

# Número total de páginas
n_pages <- pdf_info(pdf_file)$pages

# Fijamos 4 cores
n_cores <- 7

# Archivos de salida
output_csv <- "raw_text/rondalles_ocr_tomo2.csv"
output_txt <- "raw_text/rondalles_ocr_tomo2.txt"

# Comprobar que Tesseract tiene instalado el catalán
if (!"cat" %in% tesseract_info()$available) {
  stop(
    "No está instalado el idioma catalán de Tesseract.\n",
    "Ejecuta primero:\n",
    "tesseract_download('cat')"
  )
}

# ----------------------------
# FUNCIÓN OCR POR PÁGINA
# ----------------------------

procesar_ocr <- function(page, pdf_file) {
  
  tryCatch({
    
    # Crear el motor de Tesseract dentro del worker
    # Esto es importante en Windows
    tess <- tesseract::tesseract(
      language = "cat",
      options = list(
        tessedit_pageseg_mode = 6
      )
    )
    
    # Convertir la página del PDF a imagen a 400 DPI
    img <- magick::image_read_pdf(
      path = pdf_file,
      pages = page,
      density = 400
    )
    
    # Preprocesado de imagen
    img <- magick::image_convert(img, colorspace = "gray")
    img <- magick::image_deskew(img, threshold = 40)
    img <- magick::image_trim(img)
    
    # Guardar imagen temporal como PNG
    # Esto evita incompatibilidades entre magick::image_ocr() y tesseract
    tmp <- tempfile(fileext = ".png")
    magick::image_write(img, path = tmp, format = "png")
    
    # OCR con tesseract::ocr()
    txt <- tesseract::ocr(tmp, engine = tess)
    
    # Borrar archivo temporal
    unlink(tmp)
    
    # Devolver resultado
    data.frame(
      pagina = page,
      texto = txt,
      error = NA_character_,
      stringsAsFactors = FALSE
    )
    
  }, error = function(e) {
    
    data.frame(
      pagina = page,
      texto = NA_character_,
      error = conditionMessage(e),
      stringsAsFactors = FALSE
    )
  })
}

# ----------------------------
# PRUEBA CON LA PRIMERA PÁGINA
# ----------------------------

cat("Probando OCR con la página 1...\n")

prueba <- procesar_ocr(1, pdf_file)

cat("Resultado de prueba:\n")
cat(substr(prueba$texto, 1, 1000), "\n\n")

if (!is.na(prueba$error)) {
  stop(
    "La prueba con la página 1 ha fallado:\n",
    prueba$error
  )
}

cat("Prueba correcta. Empezando OCR completo...\n")

# ----------------------------
# CREAR CLUSTER WINDOWS
# ----------------------------

cl <- makeCluster(n_cores)

# Asegurar que el cluster se cierra aunque haya error
on.exit({
  try(stopCluster(cl), silent = TRUE)
}, add = TRUE)

# Cargar librerías en cada worker
clusterEvalQ(cl, {
  library(magick)
  library(tesseract)
  library(pdftools)
})

# Exportar objetos necesarios a los workers
clusterExport(
  cl,
  c("pdf_file", "procesar_ocr"),
  envir = environment()
)

# ----------------------------
# EJECUCIÓN OCR EN PARALELO
# ----------------------------

resultado <- parLapply(cl, 1:n_pages, function(p) {
  procesar_ocr(p, pdf_file)
})

# Cerrar cluster
stopCluster(cl)

# ----------------------------
# RECONSTRUIR DATAFRAME
# ----------------------------

df <- do.call(rbind, resultado)
df <- df[order(df$pagina), ]

# ----------------------------
# GUARDAR RESULTADOS
# ----------------------------

# CSV estructurado
write.csv(
  df,
  output_csv,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# TXT con separador de páginas
txt_final <- paste0(
  "=== Página ", df$pagina, " ===\n",
  ifelse(is.na(df$texto), "", df$texto),
  "\n"
)

writeLines(
  txt_final,
  con = output_txt,
  useBytes = TRUE
)

# ----------------------------
# RESUMEN FINAL
# ----------------------------

cat("\nOCR completado.\n")
cat("PDF:", pdf_file, "\n")
cat("Páginas totales:", n_pages, "\n")
cat("Cores usados:", n_cores, "\n")
cat("Páginas procesadas:", nrow(df), "\n")
cat("Páginas con error:", sum(!is.na(df$error)), "\n")
cat("Archivo CSV:", output_csv, "\n")
cat("Archivo TXT:", output_txt, "\n")

# Mostrar errores, si los hay
if (sum(!is.na(df$error)) > 0) {
  cat("\nPáginas con error:\n")
  print(df[df$error != "" & !is.na(df$error), c("pagina", "error")])
}

