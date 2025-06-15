library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(gtools)
library(visNetwork)

# Funciones

minimax <- function(nodo, es_max) {
  # Si el nodo es una hoja, se devuelve (nodo terminal)
  if (is.numeric(nodo)) {
    return(nodo)
  }
  
  # Evaluar hijos
  valores <- sapply(nodo, function(hijo) minimax(hijo, !es_max))
  
  if (es_max) {
    return(max(valores))
  } else {
    return(min(valores))
  }
}

alfabeta <- function(nodo, profundidad, es_max, alpha, beta) {

  if (is.numeric(nodo)) {
    return(nodo)
  }
  
  if (es_max) {
    valor <- -Inf
    for (hijo in nodo) {
      valor <- max(valor, alfabeta(hijo, profundidad + 1, FALSE, alpha, beta))
      alpha <- max(alpha, valor)
      if (beta <= alpha) {
        break  # poda beta
      }
    }
    return(valor)
  } else {
    valor <- Inf
    for (hijo in nodo) {
      valor <- min(valor, alfabeta(hijo, profundidad + 1, TRUE, alpha, beta))
      beta <- min(beta, valor)
      if (beta <= alpha) {
        break  # poda alfa
      }
    }
    return(valor)
  }
}


# Interfaz de usuario
ui <- dashboardPage(
  dashboardHeader(title = "Teoría de Juegos e IA"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio", tabName = "inicio", icon = icon("home")),
      menuItem("Teoría Clásica", tabName = "teoria", icon = icon("chess"),
               menuSubItem("Juegos Estáticos", tabName = "estaticos"),
               menuSubItem("Juegos Dinámicos", tabName = "dinamicos")),
      menuItem("Ejemplos Clásicos", tabName = "ejemplos", icon = icon("gamepad")),
      menuItem("IA y Juegos", tabName = "ia", icon = icon("robot")),
      menuItem("Valor de Shapley", tabName = "shapley", icon = icon("handshake"))
    )
  ),
  dashboardBody(
    tabItems(
      # Pestaña de inicio
      tabItem(tabName = "inicio",
              h2("Aplicación de Teoría de Juegos e IA"),
              p("Esta aplicación interactiva está basada en el Trabajo de Fin de Grado 'Teoría de juegos y aplicaciones en la IA' de Clara Ortega Sevilla."),
              p("Explora los conceptos fundamentales de la teoría de juegos y su aplicación en inteligencia artificial a través de simulaciones interactivas."),
              h3("Instrucciones:"),
              tags$ul(
                tags$li("Usa el menú lateral para navegar entre las diferentes secciones"),
                tags$li("En 'Teoría Clásica' puedes explorar juegos estáticos y dinámicos"),
                tags$li("En 'Ejemplos Clásicos' encontrarás implementaciones interactivas de juegos conocidos"),
                tags$li("La sección 'IA y Juegos' muestra algoritmos de búsqueda con adversario"),
                tags$li("'Valor de Shapley' te permite experimentar con juegos cooperativos")
              ),
              hr(),
              h4("Resumen del TFG:"),
              p("El trabajo explora los fundamentos matemáticos de la teoría de juegos, desde conceptos básicos hasta aplicaciones en inteligencia artificial, incluyendo:"),
              tags$ul(
                tags$li("Teoría de juegos clásica (estáticos, dinámicos, cooperativos)"),
                tags$li("Equilibrio de Nash y conceptos de solución"),
                tags$li("Algoritmos de búsqueda con adversario (Minimax, poda alfa-beta)"),
                tags$li("Aplicaciones en IA y el valor de Shapley para juegos cooperativos")
              )
      ),
      
      # Pestaña de juegos estáticos
      tabItem(tabName = "estaticos",
              h2("Juegos Estáticos - Forma Estratégica"),
              fluidRow(
                box(width = 6, title = "Configurar Juego",
                    numericInput("num_strategies1", "Número de estrategias Jugador 1:", 2, min = 2, max = 5),
                    numericInput("num_strategies2", "Número de estrategias Jugador 2:", 2, min = 2, max = 5),
                    actionButton("generate_matrix", "Generar Matriz de Pagos")
                ),
                box(width = 6, title = "Instrucciones",
                    p("1. Define el número de estrategias para cada jugador"),
                    p("2. Completa la matriz de pagos que aparecerá abajo"),
                    p("3. Haz clic en 'Analizar Juego' para calcular equilibrios y estrategias dominantes"),
                    p("Las valores de la matriz deben introducirse con el siguiente formato: ''4,2'' (dos números separados por una coma)")
                )
              ),
              fluidRow(
                box(width = 12, title = "Matriz de Pagos",
                    uiOutput("matrix_input"),
                    actionButton("analyze_game", "Analizar Juego")
                )
              ),
              fluidRow(
                box(width = 12, title = "Resultados del Análisis",
                    uiOutput("nash_equilibrium"),
                    verbatimTextOutput("game_analysis"),
                    plotOutput("nash_plot")
                )
              )
      ),
      
      # Pestaña de juegos dinámicos
      tabItem(tabName = "dinamicos",
              h2("Juegos Dinámicos - Forma Extensiva"),
              fluidRow(
                box(width = 6, title = "Configurar Árbol de Juego",
                    selectInput("game_type", "Tipo de juego:",
                                choices = c("Entronque", "Dilema del Prisionero Secuencial", "Personalizado")),
                    conditionalPanel(
                      condition = "input.game_type == 'Personalizado'",
                      numericInput("num_players", "Número de jugadores:", 2, min = 2, max = 4),
                      numericInput("tree_depth", "Profundidad del árbol:", 2, min = 1, max = 4)
                    )
                ),
                box(width = 6, title = "Instrucciones",
                    p("1. Selecciona un tipo de juego predefinido o crea uno personalizado"),
                    p("2. Construye el árbol de decisiones"),
                    p("3. Usa inducción hacia atrás para resolver el juego"),
                    p("4. Visualiza el equilibrio perfecto en subjuegos")
                )
              ),
              fluidRow(
                box(width = 12, title = "Visualización del Árbol de Juego",
                    plotOutput("game_tree", height = "600px"),
                    actionButton("steps", "Siguiente Paso"),
                    verbatimTextOutput("explanation")
                )
              )
      ),
      
      # Pestaña de ejemplos clásicos
      tabItem(tabName = "ejemplos",
              h2("Ejemplos Clásicos Interactivos"),
              tabsetPanel(
                tabPanel("Dilema del Prisionero",
                         fluidRow(
                           box(width = 6, title = "Configuración",
                               selectInput("pd_choice1", "Jugador 1 (Tú):", 
                                           choices = c("Confesar", "Callar")),
                               selectInput("pd_choice2", "Jugador 2 (Oponente):", 
                                           choices = c("Confesar", "Callar")),
                               actionButton("pd_play", "Jugar")
                           ),
                           box(width = 6, title = "Resultado",
                               tableOutput("pd_matrix"),
                               verbatimTextOutput("pd_result"),
                               plotOutput("pd_equilibrium")
                           )
                         ),
                         box(width = 12, title = "Análisis",
                             p("El dilema del prisionero muestra por qué dos individuos podrían no cooperar, incluso si parece que es en su mejor interés hacerlo."),
                             p("Equilibrio de Nash: (Confesar, Confesar)"),
                             p("Este resultado es Pareto-ineficiente, ya que ambos jugadores podrían obtener mejores resultados si ambos callaran.")
                         )
                ),
                tabPanel("Piedra, Papel o Tijera",
                         fluidRow(
                           box(width = 4, title = "Jugar",
                               selectInput("rps_choice", "Tu elección:", 
                                           choices = c("Piedra", "Papel", "Tijera")),
                               actionButton("rps_play", "Jugar"),
                               verbatimTextOutput("rps_result")
                           ),
                           box(width = 4, title = "Historial",
                               tableOutput("rps_history")
                           ),
                           box(width = 4, title = "Estadísticas",
                               plotOutput("rps_stats"),
                               plotOutput("rps_stats_2")
                           )
                         ),
                         box(width = 12, title = "Análisis",
                             p("Juego simétrico de suma cero con equilibrio de Nash en estrategias mixtas (1/3, 1/3, 1/3)."),
                             p("La estrategia óptima es elegir aleatoriamente con igual probabilidad cada opción.")
                         )
                ),
                tabPanel("Oligopolio de Cournot",
                         fluidRow(
                           box(width = 6, title = "Configuración",
                               sliderInput("cournot_firms", "Número de empresas:", 2, 10, 2),
                               numericInput("cournot_a", "Parámetro a (demanda máxima):", 100),
                               numericInput("cournot_b", "Parámetro b (pendiente demanda):", 1),
                               numericInput("cournot_c", "Costo marginal c:", 10)
                           ),
                           box(width = 6, title = "Resultados",
                               plotOutput("cournot_plot"),
                               verbatimTextOutput("cournot_eq")
                           )
                         ),
                         box(width = 12, title = "Análisis",
                             p("Modelo de competencia imperfecta donde empresas eligen cantidades a producir simultáneamente."),
                             p("El equilibrio de Nash se alcanza cuando cada empresa produce q* = (a-c)/(b(n+1))"),
                             p("A medida que aumenta el número de empresas, la producción total se acerca al nivel competitivo.")
                         )
                ),
                tabPanel("Juego de Nim",
                         fluidRow(
                           box(width = 4, title = "Configuración",
                               numericInput("nim_matches", "Número inicial de cerillas:", 15, min = 5, max = 30),
                               selectInput("nim_first", "Quién juega primero:", 
                                           choices = c("Tú", "Computadora")),
                               actionButton("nim_start", "Comenzar Juego")
                           ),
                           box(width = 4, title = "Juego",
                               uiOutput("nim_matches_display"),
                               conditionalPanel(
                                 condition = "input.nim_start > 0 && output.nim_game_active",
                                 numericInput("nim_take", "Cuántas cerillas tomar (1-3):", 1, min = 1, max = 3),
                                 actionButton("nim_move", "Mover")
                               ),
                               verbatimTextOutput("nim_result")
                           ),
                           box(width = 4, title = "Estrategia",
                               p("Juego de suma cero con estrategia ganadora conocida."),
                               p("Para ganar, deja siempre un múltiplo de 4 cerillas al oponente."),
                               p("La IA usa esta estrategia cuando juega primero.")
                           )
                         )
                )
              )
      ),
      
      # Pestaña de IA y juegos
      tabItem(tabName = "ia",
              h2("Inteligencia Artificial y Búsqueda con Adversario"),
              tabsetPanel(
                tabPanel("Algoritmo Minimax",
                         fluidRow(
                           box(width = 6, title = "Configurar Árbol",
                               numericInput("num_nodos", "Número total de nodos:", value = 3, min = 1),
                               uiOutput("inputs_hijos"),  # Inputs dinámicos para hijos y pagos
                               actionButton("construir", "Construir Árbol")
                           ),
                           box(width = 6, title = "Instrucciones",
                               p("1. Introduce el número de nodos que quieres que tenga el árbol"),
                               p("2. En cada nodo, señala cuáles son sus hijos mediante su ID (1, 2, 3...) separados por una coma si hay más de uno"),
                               p("3. Solo los nodos terminales necesitan un valor para el pago (y no tendrán nodos hijo)"),
                               p("4. Observa tu árbol y el resultado minimax")
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "Visualización del Árbol",
                               visNetworkOutput("arbol", height = "600px")
                              )
                         ),
                         fluidRow(
                           box(width = 12, title = "Resultado de la aplicación del algoritmo al árbol",
                               uiOutput("result_minimax")
                           )
                         )
                ),
                tabPanel("Poda Alfa-Beta",
                         fluidRow(
                           box(width = 6, title = "Configurar Árbol",
                               numericInput("num_nodos", "Número total de nodos:", value = 3, min = 1),
                               uiOutput("inputs_hijos_poda"),  # Inputs dinámicos para hijos y pagos
                               actionButton("construir_poda", "Construir Árbol")
                           ),
                           box(width = 6, title = "Comparación con Minimax",
                               p("La poda alfa-beta es una optimización del algoritmo minimax que elimina ramas que no necesitan ser evaluadas."),
                               p("Produce exactamente el mismo resultado que minimax pero de forma más eficiente."),
                               p("Observa cómo se 'podan' ramas completas durante la exploración."),
                               
                               p("Instrucciones de uso:"),
                               p("1. Introduce el número de nodos que quieres que tenga el árbol"),
                               p("2. En cada nodo, señala cuáles son sus hijos mediante su ID (1, 2, 3...) separados por una coma si hay más de uno"),
                               p("3. Solo los nodos terminales necesitan un valor para el pago (y no tendrán nodos hijo)"),
                               p("4. Observa tu árbol y el resultado tras la poda")
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "Visualización del Árbol",
                               visNetworkOutput("arbol_poda", height = "600px")
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "Resultado de la aplicación del algoritmo al árbol",
                               uiOutput("result_alfabeta")
                           )
                         )
                )
              )
      ),
      
      # Pestaña de valor de Shapley
      tabItem(tabName = "shapley",
              h2("Valor de Shapley para Juegos Cooperativos"),
              fluidRow(
                box(width = 6, title = "Configurar Juego",
                    numericInput("shapley_players", "Número de jugadores:", 3, min = 2, max = 5),
                    uiOutput("shapley_coalitions_input"),
                    actionButton("shapley_calculate", "Calcular Valor de Shapley")
                ),
                box(width = 6, title = "Instrucciones",
                    p("1. Selecciona el número de jugadores"),
                    p("2. Define el valor para cada coalición posible"),
                    p("3. Calcula el valor de Shapley para cada jugador"),
                    p("El valor de Shapley representa una distribución justa de las ganancias totales.")
                )
              ),
              fluidRow(
                box(width = 12, title = "Resultados",
                    verbatimTextOutput("shapley_result"),
                    plotOutput("shapley_plot")
                )
              ),
              box(width = 12, title = "Explicación",
                  p("El valor de Shapley es una solución para juegos cooperativos que asigna pagos a los jugadores según su contribución marginal esperada."),
                  p("Cumple cuatro axiomas:"),
                  tags$ol(
                    tags$li("Eficiencia: La suma de los pagos es igual al valor de la gran coalición"),
                    tags$li("Simetría: Jugadores con la misma contribución reciben lo mismo"),
                    tags$li("Jugador nulo: Jugadores que no contribuyen reciben cero"),
                    tags$li("Aditividad: El valor de la suma de juegos es la suma de los valores")
                  )
              )
      )
    )
  )
)

# Servidor
server <- function(input, output, session) {
  
  # Juegos estáticos - matriz de pagos
  game_matrix <- reactiveVal(NULL)
  
  observeEvent(input$generate_matrix, {
    # Validar
    if (input$num_strategies1 < 1 || input$num_strategies2 < 1 || !is.integer(input$num_strategies1) || !is.integer(input$num_strategies2) ) {
      showNotification(paste("Error: el número de estrategias introducido ha de ser un entero positivo y al menos 1"), type = "error")
      return()
    }
    
    n1 <- input$num_strategies1
    n2 <- input$num_strategies2
    
    mat <- matrix("", nrow = n1, ncol = n2)
    rownames(mat) <- paste("Estrategia", 1:n1, "J1")
    colnames(mat) <- paste("Estrategia", 1:n2, "J2")
    
    game_matrix(mat)
  })
  
  
  output$matrix_input <- renderUI({
    
    mat <- game_matrix()
    if (is.null(mat)) return()
    
    n1 <- nrow(mat)
    n2 <- ncol(mat)
    
    inputs <- lapply(1:n1, function(i) {
      fluidRow(
        lapply(1:n2, function(j) {
          column(3, textInput(paste0("cell_", i, "_", j), 
                              label = paste(rownames(mat)[i], "-", colnames(mat)[j])))
        })
      )
    })
    
    do.call(tagList, inputs)
    
  })
  
  strategies_j1 <- reactiveVal(NULL)
  strategies_j2 <- reactiveVal(NULL)
  
  equilibrium<-eventReactive(input$analyze_game,{
    mat<- game_matrix()
    if (is.null(mat)) return("Matriz no generada")
    max_j1<- -Inf
    max_j2<- -Inf
    valor_max_j1<- NULL
    valor_max_j2<- NULL
    max_strategies_j1 <- strategies_j1()
    max_strategies_j2 <- strategies_j2()
    
    for(i in 1:nrow(mat)){
      for(j in 1:ncol(mat)){
        valor <- input[[paste0("cell_", i, "_", j)]]
        numeros <- as.numeric(strsplit(valor, ",")[[1]])  
        if(numeros[2] > max_j2){
          max_j2<- numeros[2]
          valor_max_j2<- valor
        }
      }
      max_strategies_j2<- append(max_strategies_j2, valor_max_j2)
      max_j2<- -Inf 
      valor_max_j2<- NULL
    }
    
    for(j in 1:ncol(mat)){
      for(i in 1:nrow(mat)){
        valor <- input[[paste0("cell_", i, "_", j)]]
        numeros <- as.numeric(strsplit(valor, ",")[[1]])  
        if(numeros[1] > max_j1){
          max_j1<- numeros[1]
          valor_max_j1<- valor
        }
      }
      max_strategies_j1<- append(max_strategies_j1, valor_max_j1)
      max_j1<- -Inf
      valor_max_j1<-NULL
    }
    
    equilibrio_nash<- NULL
    for(i in 1:length(max_strategies_j1)){
      for(j in 1:length(max_strategies_j2)){
        if(max_strategies_j1[i] == max_strategies_j2[j]){
          equilibrio_nash<- max_strategies_j1[i]
          break
        }
      }
    }
    
    strategies_j1(max_strategies_j1)
    strategies_j2(max_strategies_j2)
  
    if(length(equilibrio_nash)>0){
      paste("Equilibrio Nash: ", equilibrio_nash)
    }
    else{
      "Este juego no presenta un equilibrio de Nash en estrategias puras"
    }
  
  })
  
  output$nash_equilibrium <- renderText({
    equilibrium()
  })
  
  output$nash_plot<- renderPlot({
    req(input$analyze_game)
    points_j1<- strategies_j1()
    points_j2<- strategies_j2()
  
    convert_to_point <- function(p) {
      as.numeric(strsplit(p, ",")[[1]])
    }
    
    points_j1 <- lapply(strategies_j1(), convert_to_point)
    points_j2 <- lapply(strategies_j2(), convert_to_point)
    
    x_j1 <- sapply(points_j1, function(p) p[1])
    y_j1 <- sapply(points_j1, function(p) p[2])
    
    x_j2 <- sapply(points_j2, function(p) p[1])
    y_j2 <- sapply(points_j2, function(p) p[2])
    
    
    range_x <- range(c(x_j1, x_j2))
    range_y <- range(c(y_j1, y_j2))
    xlim <- c(range_x[1] - 1, range_x[2] + 1)
    ylim <- c(range_y[1] - 1, range_y[2] + 1)
    
    plot(x_j1, y_j1, 
         xlab = "Eje X", ylab = "Eje Y", 
         main = "Mejores opciones para cada jugador",
         pch = 19, col = "blue", cex = 4,
         xlim = xlim,
         ylim = ylim
         )
    grid()
    
    points(x_j2, y_j2, pch = 17, col = "red", cex = 3)
    
    legend("topright",
           legend = c("Opciones óptimas jugador 1", "Opciones óptimas jugador 2"),
           col = c("blue", "red"),
           pch = c(19, 17),
           pt.cex = 2.5,
           bty = "n")
    
  })
  
  
  # Juegos dinámicos - árbol de juego
  output$game_tree <- renderPlot({
    if (input$game_type == "Entronque") {
      plot(1, type = "n", xlim = c(0, 10), ylim = c(0, 10), xlab = "", ylab = "", axes = FALSE)
      text(5, 8, "Jugador 1", cex = 1.5)
      segments(5, 8, 3, 6)
      segments(5, 8, 7, 6)
      text(3, 6, "Jugador 2", cex = 1.5)
      text(7, 6, "Jugador 2", cex = 1.5)
      segments(3, 6, 2, 4)
      segments(3, 6, 4, 4)
      segments(7, 6, 6, 4)
      segments(7, 6, 8, 4)
      text(2, 4, "(3,2)", cex = 1.2)
      text(4, 4, "(1,1)", cex = 1.2)
      text(6, 4, "(0,0)", cex = 1.2)
      text(8, 4, "(2,3)", cex = 1.2)
      
    } else if (input$game_type == "Dilema del Prisionero Secuencial") {
      plot(1, type = "n", xlim = c(0, 10), ylim = c(0, 10), xlab = "", ylab = "", axes = FALSE)
      text(5, 8, "Jugador 1", cex = 1.5)
      segments(5, 8, 3, 6)
      segments(5, 8, 7, 6)
      text(3, 6, "Confesar", cex = 1.2, pos = 2)
      text(7, 6, "Callar", cex = 1.2, pos = 4)
      segments(3, 6, 2, 4)
      segments(3, 6, 4, 4)
      segments(7, 6, 6, 4)
      segments(7, 6, 8, 4)
      text(2, 4, "Jugador 2\nConfesar\n(-4,-4)", cex = 1.2)
      text(4, 4, "Jugador 2\nCallar\n(-5,0)", cex = 1.2)
      text(6, 4, "Jugador 2\nConfesar\n(0,-5)", cex = 1.2)
      text(8, 4, "Jugador 2\nCallar\n(-1,-1)", cex = 1.2)
      
    } else{
      
      # Validar
      if (input$tree_depth < 2 || input$num_players < 1 || !is.integer(input$tree_depth) ||  !is.integer(input$num_players)) {
        showNotification(paste("Los valores introducidos deben ser números enteros positivos. Además, mínimo debe haber un jugador y profundidad 2"), type = "error")
        return()
      }
      
      tree_depth <- input$tree_depth
      num_nodos <- (2^tree_depth) - 1
      num_hojas <- 2^(tree_depth-1)
      num_players <- input$num_players
  
      plot(1, type = "n", xlim = c(0, 2^tree_depth), ylim = c(0, tree_depth + 1),
           xlab = "", ylab = "", axes = FALSE)
      # Posiciones de nodos
      nodo_pos <- data.frame(id = 1:num_nodos, x = NA, y = NA)
      
      # Coordenadas en cada nodo según nivel y posición
      nodo_id <- 1
      for (nivel in 1:tree_depth) {
        num_nodos_nivel <- 2^(nivel - 1)
        espaciado <- 2^((tree_depth - nivel) + 1)
        y <- tree_depth - nivel + 1
        x_start <- espaciado / 2
        
        for (i in 0:(num_nodos_nivel - 1)) {
          x <- x_start + i * espaciado
          nodo_pos[nodo_id, ] <- c(nodo_id, x, y)
          nodo_id <- nodo_id + 1
        }
      }
      
      # Aristas
      for (i in 1:(num_nodos - num_hojas)) {
        padre <- nodo_pos[i, ]
        hijo_izq <- nodo_pos[2 * i, ]
        hijo_der <- nodo_pos[2 * i + 1, ]
        
        segments(padre$x, padre$y, hijo_izq$x, hijo_izq$y)
        segments(padre$x, padre$y, hijo_der$x, hijo_der$y)
        
        # Etiquetas de acciones (esto es decorativo en realidad)
        text((padre$x + hijo_izq$x) / 2, (padre$y + hijo_izq$y) / 2, "Alternativa 1", cex = 0.8, pos = 2)
        text((padre$x + hijo_der$x) / 2, (padre$y + hijo_der$y) / 2, "Alternativa 2", cex = 0.8, pos = 4)
      }
      
      # Nodos
      for (i in 1:num_nodos) {
        nodo <- nodo_pos[i, ]
        nivel <- floor(log2(nodo$id)) + 1
        
        if (i <= (num_nodos - num_hojas)) {
          jugador_num <- ((nivel - 1) %% num_players) + 1
          jugador <- paste("Jugador", jugador_num)
          
          text(nodo$x, nodo$y, jugador, cex = 1.2)
          #text(nodo$x, nodo$y, paste("Jugador", ifelse(nivel %% 2 == 1, " 1", " 2")), cex = 1.2)
        } else {
          random_num <- sample(1:100, num_players, replace = TRUE)
          label <- paste0("(", paste(random_num, collapse = ", "), ")")
          text(nodo$x, nodo$y, label, cex = 1)
        }
      }
      
    }
    
  })
  
  steps <- reactiveVal(0)
  observeEvent(input$steps, {
    if(input$game_type == "Entronque"){
      step <- steps() + 1
      steps(step)
      
      # Actualizar explicación según el paso
      explanations <- c(
        "1. Comenzamos con los nodos terminales (hojas) que tienen valores asignados.",
        "2. Opción óptima primer subárbol: para el jugador 2, la opción de mayor ventaja es (3,2).",
        "3. Opción óptima segundo subárbol: para el jugador 2, la opción de mayor ventaja es (2,3).",
        "4. Se propaga la utilidad hacia arriba.Se eliminan los nodos hoja.",
        "5. Opción óptima para el subárbol restante: para el jugador 1, la opción de mayor ventaja es (3,2).",
        "6. El equilibrio de Nash perfecto en subjuegos es (3,2)."
      )
      
      output$explanation <- renderText({
        if (step <= length(explanations)){
          explanations[step]
        }
        else{
          steps(0)
          "Algoritmo completado."
        } 
      })
    }
    else if(input$game_type == "Dilema del Prisionero Secuencial"){
      step <- steps() + 1
      steps(step)
      
      # Actualizar explicación según el paso
      explanations <- c(
        "1. Comenzamos con los nodos terminales (hojas) que tienen valores asignados.",
        "2. Opción óptima primer subárbol: para el jugador 2, la opción de mayor ventaja es Callar (-5,0).",
        "3. Opción óptima segundo subárbol: para el jugador 2, la opción de mayor ventaja es Callar (-1,-1).",
        "4. Se propaga la utilidad hacia arriba.Se eliminan los nodos hoja.",
        "5. Opción óptima para el subárbol restante: para el jugador 1, la opción de mayor ventaja es Callar (-1, -1).",
        "6. El equilibrio de Nash perfecto en subjuegos es (-1,-1)."
      )
      
      output$explanation <- renderText({
        if (step <= length(explanations)){
          explanations[step] 
        }
        else {
          steps(0)
          "Algoritmo completado."
        }
      })
    }
    else{
      step <- steps() + 1
      steps(step)
      
      # Actualizar explicación según el paso
      explanations <- c(
        "1. Comenzamos con los nodos terminales (hojas) que tienen valores asignados.",
        "2. Opción óptima para cada subárbol: tupla con el número mayor en la posición que coincide con el jugador del nodo padre.",
        "3. Se propaga hacia arriba la utilidad. Se eliminan los nodos hoja.",
        "4. Se repite el mismo proceso con el siguiente jugador y los siguientes nodos hijo. ",
        "5. El equilibrio de Nash perfecto en subjuegos es el valor final del nodo raíz."
      )
      
      output$explanation <- renderText({
        if (step <= length(explanations)){
          explanations[step] 
        }
        else {
          steps(0)
          "Algoritmo completado."
        }
      })
    }
  })
  
  
  # Ejemplos clásicos - Dilema del Prisionero
  output$pd_matrix <- renderTable({
    data.frame(
      Jugador2 = c("Confesar", "Callar"),
      Confesar = c("(-4, -4)", "(-5, 0)"),
      Callar = c("(0, -5)", "(-1, -1)"),
      stringsAsFactors = FALSE
    )
  }, rownames = TRUE)
  
  observeEvent(input$pd_play, {
    choice1 <- input$pd_choice1
    choice2 <- input$pd_choice2
    
    if (choice1 == "Confesar" && choice2 == "Confesar") {
      result <- "Resultado: Ambos confiesan (-4, -4)\nEste es el equilibrio de Nash."
    } else if (choice1 == "Confesar" && choice2 == "Callar") {
      result <- "Resultado: Tú confiesas, oponente calla (0, -5)\nTú sales libre, oponente recibe máxima pena."
    } else if (choice1 == "Callar" && choice2 == "Confesar") {
      result <- "Resultado: Tú callas, oponente confiesa (-5, 0)\nOponente sale libre, tú recibes máxima pena."
    } else {
      result <- "Resultado: Ambos callan (-1, -1)\nResultado Pareto-óptimo pero no equilibrio."
    }
    
    output$pd_result <- renderText(result)
  })
  
  # Ejemplos clásicos - Piedra, Papel o Tijera
  rps_history <- reactiveVal(data.frame(
    Jugada = integer(),
    Tu_Eleccion = character(),
    IA_Eleccion = character(),
    Resultado = character(),
    stringsAsFactors = FALSE
  ))
  
  results <- reactiveVal(c(Piedra = 0, Papel = 0, Tijera = 0))
  ganancias<- reactiveVal(c(Ganas = 0, Pierdes = 0, Empate = 0))
  
  observeEvent(input$rps_play, {
    player_choice <- input$rps_choice
    ia_choice <- sample(c("Piedra", "Papel", "Tijera"), 1)
    
    if (player_choice == ia_choice) {
      result <- "Empate"
    } else if ((player_choice == "Piedra" && ia_choice == "Tijera") ||
               (player_choice == "Papel" && ia_choice == "Piedra") ||
               (player_choice == "Tijera" && ia_choice == "Papel")) {
      result <- "Ganas"
    } else {
      result <- "Pierdes"
    }
    
    new_row <- data.frame(
      Jugada = nrow(rps_history()) + 1,
      Tu_Eleccion = player_choice,
      IA_Eleccion = ia_choice,
      Resultado = result,
      stringsAsFactors = FALSE
    )
    
    rps_history(rbind(rps_history(), new_row))
    
    output$rps_result <- renderText({
      paste("Tú elegiste:", player_choice, "\nIA eligió:", ia_choice, "\nResultado:", result)
    })
    
    valores_actuales <- results() 
    valores_actuales[ia_choice] <- valores_actuales[ia_choice] + 1
    results(valores_actuales)
    
    valores_ganancias<- ganancias()
    valores_ganancias[result]<- valores_ganancias[result]+1
    ganancias(valores_ganancias)
    
  })
  
  output$rps_stats <- renderPlot({
    # Estadísticas de "Piedra, Papel o Tijera"
  
    barplot(
      results(),
      col = c("orange", "purple", "green"),
      main = "Frecuencia de elecciones de la IA",
      ylab = "Número total"
    )
    
    
  })
  
  output$rps_stats_2 <- renderPlot({
    # Estadísticas de "Empate, Perder, Ganar"
  
    barplot(
      ganancias(),
      col = c("red", "black", "gray"),
      main = "Frecuencia de resultados",
      ylab = "Número total"
    )
  
  })
 
  output$rps_history <- renderTable({
    rps_history()
  })
  
  # Ejemplos clásicos - Oligopolio de Cournot
  observe({
    n <- input$cournot_firms
    a <- input$cournot_a
    b <- input$cournot_b
    c_val <- input$cournot_c
    
    q_star <- (a - c_val) / (b * (n + 1))
    Q_star <- n * q_star
    P_star <- a - b * Q_star
    profit <- (P_star - c_val) * q_star
    
    output$cournot_plot <- renderPlot({
      q_values <- seq(0, (a - c_val)/b, length.out = 100)
      demand <- a - b * q_values
      marginal_revenue <- a - 2 * b * q_values
      
      df <- data.frame(Quantity = q_values, Demand = demand, MR = marginal_revenue)
      
      ggplot(df, aes(x = Quantity)) +
        geom_line(aes(y = Demand, color = "Demanda"), size = 1.5) +
        geom_line(aes(y = MR, color = "Ingreso Marginal"), size = 1.5) +
        geom_hline(yintercept = c_val, linetype = "dashed", color = "red") +
        geom_vline(xintercept = Q_star, linetype = "dotted") +
        geom_point(aes(x = Q_star, y = P_star), size = 3, color = "blue") +
        labs(title = "Equilibrio de Cournot",
             x = "Cantidad total (Q)", y = "Precio",
             color = "Curvas") +
        theme_minimal() +
        annotate("text", x = Q_star, y = P_star + 5, 
                 label = paste0("(", round(Q_star,1), ", ", round(P_star,1), ")"))
    })
    
    output$cournot_eq <- renderText({
      paste("Equilibrio de Nash:\n",
            "Cantidad por empresa (q*):", round(q_star, 2), "\n",
            "Cantidad total (Q*):", round(Q_star, 2), "\n",
            "Precio (P*):", round(P_star, 2), "\n",
            "Beneficio por empresa:", round(profit, 2))
    })
  })
  
  # Ejemplos clásicos - Juego de Nim
  nim_matches_left <- reactiveVal(0)
  nim_game_active <- reactiveVal(FALSE)
  
  observeEvent(input$nim_start, {
    # Validar
    if (input$nim_matches < 1) {
      showNotification(paste("El número de cerillas para comenzar el juego debe ser mínimo 1"), type = "error")
      return()
    }
    
    nim_matches_left(input$nim_matches)
    nim_game_active(TRUE)
  })
  
  output$nim_matches_display <- renderUI({
    if (nim_game_active()) {
      box(title = "Cerillas restantes", width = 12,
          h3(nim_matches_left()),
          style = "text-align: center; font-size: 24px;")
    }
  })
  
  output$nim_game_active <- reactive({
    nim_game_active()
  })
  outputOptions(output, "nim_game_active", suspendWhenHidden = FALSE)
  
  # Si la IA empieza, se ejecuta 
  
  observeEvent(input$nim_start, {
    
    current_matches <- nim_matches_left()
    
    # Si la IA debe comenzar y aún no ha jugado
    if (input$nim_first == "Computadora" && current_matches == input$nim_matches) {
      ia_takes <- ifelse(current_matches %% 4 == 0, sample(1:3, 1), current_matches %% 4)
      current_matches <- current_matches - ia_takes
      nim_matches_left(current_matches)
      
      if (current_matches == 0) {
        output$nim_result <- renderText(paste("IA comenzó y tomó", ia_takes, "cerillas. ¡La IA ganó!"))
        nim_game_active(FALSE)
        return()
      } else {
        output$nim_result <- renderText(paste("IA comenzó y tomó", ia_takes, "cerillas. Tu turno."))
        return()
      }
    }
  })
  
  observeEvent(input$nim_move, {
    
    player_takes <- input$nim_take
    current_matches <- nim_matches_left()
    
    if (player_takes < 1 || player_takes > 3 || player_takes > current_matches) {
      output$nim_result <- renderText("Movimiento inválido. Debes tomar entre 1 y 3 cerillas.")
      return()
    }
    
    new_matches <- current_matches - player_takes
    nim_matches_left(new_matches)
    
    if (new_matches == 0) {
      output$nim_result <- renderText("¡Ganaste! Tomaste la última cerilla.")
      nim_game_active(FALSE)
      return()
    }
    
    # Turno de la IA
    ia_takes <- ifelse(new_matches %% 4 == 0, sample(1:3, 1), new_matches %% 4)
    new_matches <- new_matches - ia_takes
    nim_matches_left(new_matches)
    
    if (new_matches == 0) {
      output$nim_result <- renderText(paste("IA tomó", ia_takes, "cerillas. ¡La IA ganó!"))
      nim_game_active(FALSE)
    } else {
      output$nim_result <- renderText(paste("Tú tomaste", player_takes, "cerillas. IA tomó", ia_takes, "cerillas."))
    }
  })
  
  # IA y juegos - Algoritmo Minimax y poda alfa-beta
  
  # Generar inputs para cada nodo
  output$inputs_hijos <- renderUI({
    req(input$num_nodos)
    lapply(1:input$num_nodos, function(nodo) {
      wellPanel(
        h4(paste("Nodo", nodo)),
        textInput(paste0("hijos_", nodo), "Nodos hijos (IDs separados por coma):", ""),
        textInput(paste0("pago_", nodo), "Pago si es terminal (dejar vacío si no es hoja):", "")
      )
    })
  })
  
  # Función recursiva para construir el árbol desde nodo raíz
  construir_arbol <- function(nodo_id, input) {
    hijos_input <- input[[paste0("hijos_", nodo_id)]]
    pago_input <- input[[paste0("pago_", nodo_id)]]
    
    # Si no tiene hijos y tiene pago, es una hoja
    if ((is.null(hijos_input) || trimws(hijos_input) == "") && nzchar(pago_input)) {
      return(as.numeric(pago_input))
    }
    
    # Si tiene hijos, construir cada subárbol recursivamente
    hijos <- strsplit(hijos_input, ",")[[1]] |> trimws() |> as.integer()
    hijos <- hijos[!is.na(hijos)]
    
    subarboles <- lapply(hijos, function(h) construir_arbol(h, input))
    return(subarboles)
  }
  
  # Construir el árbol al hacer clic en el botón
  observeEvent(input$construir, {
    req(input$num_nodos)
    
    nodos <- data.frame(id = 1:input$num_nodos, label = paste("Nodo", 1:input$num_nodos))
    aristas <- data.frame(from = integer(), to = integer())
    
    # Procesar cada nodo
    for (nodo in 1:input$num_nodos) {
      hijos <- strsplit(input[[paste0("hijos_", nodo)]], ",")[[1]] |> trimws() |> as.integer()
      pago <- input[[paste0("pago_", nodo)]]
      
      # Validar
      if ((length(hijos) > 0 && any(is.na(hijos))) || !is.integer(hijos) || any(hijos > input$num_nodos)) {
        showNotification(paste("Error en Nodo", nodo, ": IDs de hijos inválidos"), type = "error")
        return()
      }
      
      # Si tiene hijos, crear aristas
      if (length(hijos) > 0) {
        aristas <- rbind(aristas, data.frame(from = nodo, to = hijos))
      }
      
      # Si no tiene hijos y tiene pago, es nodo terminal
      if (length(hijos) == 0 && nzchar(pago)) {
        nodos$label[nodos$id == nodo] <- paste("(Pago:", pago, ")")
      }
      
    }
    
    # Construir árbol desde la raíz (nodo 1)
    lista_arbol <- construir_arbol(1, input)
    
    # Visualizar
    output$arbol <- renderVisNetwork({
      visNetwork(nodos, aristas) %>%
        visHierarchicalLayout(direction = "UD", sortMethod = "directed") %>%
        visEdges(arrows = "to") %>%
        visNodes(shape = "box", color = list(background = "lightblue"))
    })
    
    
    # Ejecutar minimax
    output$result_minimax <- renderUI({
      valor <- minimax(lista_arbol, es_max = TRUE)
      p(paste("Valor esperado del juego (estrategia minimax):", valor))
    })
    
  })
  
  # Poda alfa-beta
  
  output$inputs_hijos_poda <- renderUI({
    req(input$num_nodos)
    lapply(1:input$num_nodos, function(nodo) {
      wellPanel(
        h4(paste("Nodo", nodo)),
        textInput(paste0("hijos_", nodo), "Nodos hijos (IDs separados por coma):", ""),
        textInput(paste0("pago_", nodo), "Pago si es terminal (dejar vacío si no es hoja):", "")
      )
    })
  })

  # Construir el árbol al hacer clic en el botón
  observeEvent(input$construir_poda, {
    req(input$num_nodos)
    
    nodos <- data.frame(id = 1:input$num_nodos, label = paste("Nodo", 1:input$num_nodos))
    aristas <- data.frame(from = integer(), to = integer())
    
    # Procesar cada nodo
    for (nodo in 1:input$num_nodos) {
      hijos <- strsplit(input[[paste0("hijos_", nodo)]], ",")[[1]] |> trimws() |> as.integer()
      pago <- input[[paste0("pago_", nodo)]]
      
      # Validar
      if ((length(hijos) > 0 && any(is.na(hijos))) || !is.integer(hijos) || any(hijos > input$num_nodos)) {
        showNotification(paste("Error en Nodo", nodo, ": IDs de hijos inválidos"), type = "error")
        return()
      }
      
      # Si tiene hijos, crear aristas
      if (length(hijos) > 0) {
        aristas <- rbind(aristas, data.frame(from = nodo, to = hijos))
      }
      
      # Si no tiene hijos y tiene pago, es nodo terminal
      if (length(hijos) == 0 && nzchar(pago)) {
        nodos$label[nodos$id == nodo] <- paste("(Pago:", pago, ")")
      }
      
    }
    
    # Construir árbol desde la raíz (nodo 1)
    lista_arbol_poda <- construir_arbol(1, input)
    
    # Visualizar
    output$arbol_poda <- renderVisNetwork({
      visNetwork(nodos, aristas) %>%
        visHierarchicalLayout(direction = "UD", sortMethod = "directed") %>%
        visEdges(arrows = "to") %>%
        visNodes(shape = "box", color = list(background = "lightblue"))
    })
    
    output$result_alfabeta <- renderUI({
      inicio <- Sys.time()
      valor <- alfabeta(lista_arbol_poda, 0, TRUE, -Inf, Inf)
      fin <- Sys.time()
      
      inicio_minimax <- Sys.time()
      valor_minimax <- minimax(lista_arbol_poda, es_max = TRUE)
      fin_minimax <- Sys.time()
      
      tiempo_transcurrido <- fin - inicio
      tiempo_transcurrido_minimax <- fin_minimax - inicio_minimax
      
      HTML(paste0("Valor esperado del juego (estrategia poda):", valor, "<br>",
              "\nTiempo de ejecución con la estrategia de poda: ", tiempo_transcurrido, "<br>",
              "\nTiempo de ejecución con la estrategia minimax: ", tiempo_transcurrido_minimax))
    })
    
  })
  
  # Valor de Shapley
  output$shapley_coalitions_input <- renderUI({
    n <- input$shapley_players
    players <- LETTERS[1:n]
    all_coalitions <- unlist(lapply(1:n, function(k) combn(players, k, simplify = FALSE)), recursive = FALSE)
    
    inputs <- lapply(all_coalitions, function(co) {
      coalition_name <- paste(co, collapse = ",")
      numericInput(paste0("coal_", coalition_name), 
                   label = paste("Valor para coalición", coalition_name), 
                   value = 0)
    })
    
    do.call(tagList, inputs)
  })
  
  observeEvent(input$shapley_calculate, {
    n <- input$shapley_players
    players <- LETTERS[1:n]
    
    # Obtener valores de todas las coaliciones
    all_coalitions <- unlist(lapply(1:n, function(k) combn(players, k, simplify = FALSE)), recursive = FALSE)
    coalition_values <- sapply(all_coalitions, function(co) {
      coalition_name <- paste(co, collapse = ",")
      input[[paste0("coal_", coalition_name)]]
    })
    
    names(coalition_values) <- sapply(all_coalitions, function(co) paste(co, collapse = ","))
    
    # Calcular valor de Shapley para cada jugador
    shapley_values <- sapply(players, function(p) {
      coalitions_without_p <- all_coalitions[sapply(all_coalitions, function(co) !(p %in% co))]
      
      sum(sapply(coalitions_without_p, function(co) {
        s <- length(co)
        co_with_p <- c(co, p)
        co_with_p_name <- paste(sort(co_with_p), collapse = ",")
        co_name <- paste(sort(co), collapse = ",")
        
        marginal_contribution <- coalition_values[co_with_p_name] - coalition_values[co_name]
        factorial(s) * factorial(n - s - 1) / factorial(n) * marginal_contribution
      }))
    })
    
    output$shapley_result <- renderText({
      paste("Valores de Shapley:\n", 
            paste(players, ": ", round(shapley_values, 2), collapse = "\n"))
    })
    
    output$shapley_plot <- renderPlot({
      df <- data.frame(Player = players, Value = shapley_values)
      ggplot(df, aes(x = Player, y = Value, fill = Player)) +
        geom_bar(stat = "identity") +
        labs(title = "Distribución del Valor de Shapley", y = "Valor") +
        theme_minimal()
    })
  })
}

# Ejecutar la aplicación
shinyApp(ui, server)