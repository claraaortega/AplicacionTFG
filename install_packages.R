paquetes <- c("shiny", "shinydashboard", "ggplot2", "DT", "gtools", "visNetwork")

nuevos <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]

if(length(nuevos)) install.packages(nuevos)
