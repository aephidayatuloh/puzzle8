library(shiny)
library(shinydashboard)

# Fungsi untuk membuat puzzle awal yang dapat diselesaikan
create_solvable_puzzle <- function() {
  # Puzzle solved
  solved <- c(1, 2, 3, 4, 5, 6, 7, 8, 0)
  
  # Acak puzzle dengan langkah-langkah valid
  puzzle <- solved
  moves <- 0
  blank_pos <- 9
  
  while(moves < 100) {
    # Tentukan kemungkinan gerakan
    possible_moves <- c()
    if(blank_pos %% 3 != 1) possible_moves <- c(possible_moves, blank_pos - 1)  # Kiri
    if(blank_pos %% 3 != 0) possible_moves <- c(possible_moves, blank_pos + 1)  # Kanan
    if(blank_pos > 3) possible_moves <- c(possible_moves, blank_pos - 3)        # Atas
    if(blank_pos < 7) possible_moves <- c(possible_moves, blank_pos + 3)        # Bawah
    
    # Pilih gerakan acak
    move <- sample(possible_moves, 1)
    
    # Tukar posisi
    temp <- puzzle[blank_pos]
    puzzle[blank_pos] <- puzzle[move]
    puzzle[move] <- temp
    
    blank_pos <- move
    moves <- moves + 1
  }
  
  return(puzzle)
}

# Fungsi untuk mengecek apakah puzzle sudah selesai
is_solved <- function(puzzle) {
  target <- c(1, 2, 3, 4, 5, 6, 7, 8, 0)
  return(identical(puzzle, target))
}

# Fungsi untuk mendapatkan posisi kotak kosong
get_blank_position <- function(puzzle) {
  return(which(puzzle == 0))
}

# Fungsi untuk memindahkan kotak - dimodifikasi untuk return status pergerakan
move_tile <- function(puzzle, tile_position) {
  blank_pos <- get_blank_position(puzzle)
  
  # Cek apakah gerakan valid
  valid_moves <- c()
  if(blank_pos %% 3 != 1) valid_moves <- c(valid_moves, blank_pos - 1)  # Kiri
  if(blank_pos %% 3 != 0) valid_moves <- c(valid_moves, blank_pos + 1)  # Kanan
  if(blank_pos > 3) valid_moves <- c(valid_moves, blank_pos - 3)        # Atas
  if(blank_pos < 7) valid_moves <- c(valid_moves, blank_pos + 3)        # Bawah
  
  # Jika gerakan valid, lakukan pertukaran
  if(tile_position %in% valid_moves) {
    # Lakukan pertukaran
    temp <- puzzle[blank_pos]
    puzzle[blank_pos] <- puzzle[tile_position]
    puzzle[tile_position] <- temp
    return(list(puzzle = puzzle, moved = TRUE))  # Return dengan status moved TRUE
  }
  
  # Jika tidak valid, return puzzle tanpa perubahan dan status moved FALSE
  return(list(puzzle = puzzle, moved = FALSE))
}

# Fungsi untuk menghitung jarak Manhattan antara dua posisi
manhattan_distance <- function(pos1, pos2) {
  row1 <- (pos1 - 1) %/% 3 + 1
  col1 <- (pos1 - 1) %% 3 + 1
  row2 <- (pos2 - 1) %/% 3 + 1
  col2 <- (pos2 - 1) %% 3 + 1
  return(abs(row1 - row2) + abs(col1 - col2))
}

# Fungsi untuk menghitung heuristik (jumlah jarak Manhattan semua kotak ke posisi target)
calculate_heuristic <- function(puzzle) {
  target_positions <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)  # posisi target untuk angka 1-8, 0 di posisi 9
  heuristic <- 0
  
  for(i in 1:8) {
    current_pos <- which(puzzle == i)
    target_pos <- i
    heuristic <- heuristic + manhattan_distance(current_pos, target_pos)
  }
  
  return(heuristic)
}

# Fungsi BFS untuk mencari solusi optimal
solve_puzzle_bfs <- function(initial_puzzle) {
  # Jika sudah solved, return 0
  if(is_solved(initial_puzzle)) {
    return(0)
  }
  
  # Queue untuk menyimpan state yang akan dieksplorasi
  # Setiap elemen: list(puzzle = ..., depth = ...)
  queue <- list(list(puzzle = initial_puzzle, depth = 0))
  
  # Set untuk menyimpan state yang sudah dikunjungi
  visited <- new.env(hash = TRUE)
  visited[[paste(initial_puzzle, collapse = ",")]] <- TRUE
  
  while(length(queue) > 0) {
    # Ambil elemen pertama dari queue
    current <- queue[[1]]
    queue <- queue[-1]
    
    # Dapatkan posisi blank
    blank_pos <- get_blank_position(current$puzzle)
    
    # Dapatkan semua gerakan valid
    valid_moves <- c()
    if(blank_pos %% 3 != 1) valid_moves <- c(valid_moves, blank_pos - 1)  # Kiri
    if(blank_pos %% 3 != 0) valid_moves <- c(valid_moves, blank_pos + 1)  # Kanan
    if(blank_pos > 3) valid_moves <- c(valid_moves, blank_pos - 3)        # Atas
    if(blank_pos < 7) valid_moves <- c(valid_moves, blank_pos + 3)        # Bawah
    
    # Untuk setiap gerakan valid
    for(move in valid_moves) {
      # Buat salinan puzzle dan coba gerakan
      new_puzzle <- current$puzzle
      temp <- new_puzzle[blank_pos]
      new_puzzle[blank_pos] <- new_puzzle[move]
      new_puzzle[move] <- temp
      
      # Cek apakah sudah mencapai target
      if(is_solved(new_puzzle)) {
        return(current$depth + 1)
      }
      
      # Buat key untuk state baru
      state_key <- paste(new_puzzle, collapse = ",")
      
      # Jika belum dikunjungi
      if(is.null(visited[[state_key]])) {
        # Tandai sebagai sudah dikunjungi
        visited[[state_key]] <- TRUE
        
        # Tambahkan ke queue
        queue <- c(queue, list(list(puzzle = new_puzzle, depth = current$depth + 1)))
      }
    }
  }
  
  # Jika tidak ditemukan solusi (seharusnya tidak terjadi untuk puzzle yang solvable)
  return(-1)
}

# Fungsi untuk mendapatkan hint (kotak terbaik untuk dipindahkan)
get_hint <- function(puzzle) {
  blank_pos <- get_blank_position(puzzle)
  valid_moves <- c()
  
  # Dapatkan semua gerakan valid
  if(blank_pos %% 3 != 1) valid_moves <- c(valid_moves, blank_pos - 1)  # Kiri
  if(blank_pos %% 3 != 0) valid_moves <- c(valid_moves, blank_pos + 1)  # Kanan
  if(blank_pos > 3) valid_moves <- c(valid_moves, blank_pos - 3)        # Atas
  if(blank_pos < 7) valid_moves <- c(valid_moves, blank_pos + 3)        # Bawah
  
  # Hitung heuristik untuk setiap kemungkinan gerakan
  best_move <- NULL
  best_heuristic <- Inf
  
  for(move in valid_moves) {
    # Buat salinan puzzle dan coba gerakan
    temp_puzzle <- puzzle
    temp <- temp_puzzle[blank_pos]
    temp_puzzle[blank_pos] <- temp_puzzle[move]
    temp_puzzle[move] <- temp
    
    # Hitung heuristik setelah gerakan
    heuristic <- calculate_heuristic(temp_puzzle)
    
    if(heuristic < best_heuristic) {
      best_heuristic <- heuristic
      best_move <- move
    }
  }
  
  return(best_move)
}

# UI
ui <- fluidPage(
  titlePanel("Puzzle 8 - Susun Angka 1-8"),
  
  # Loading screen
  uiOutput("loading_screen"),
  
  # Main content (akan disembunyikan saat loading)
  uiOutput("main_content")
)

# Server
server <- function(input, output, session) {
  # Inisialisasi state permainan
  game_state <- reactiveValues(
    puzzle = create_solvable_puzzle(),
    initial_puzzle = NULL,
    moves = 0,
    game_won = FALSE,
    hints_used = 0,
    hint_position = NULL,
    optimal_steps = NULL,
    initialized = FALSE
  )
  
  # Proses inisialisasi awal dengan perhitungan solusi
  observe({
    if(!game_state$initialized) {
      # Hitung langkah optimal
      optimal_steps <- solve_puzzle_bfs(game_state$puzzle)
      game_state$optimal_steps <- optimal_steps
      game_state$initial_puzzle <- game_state$puzzle
      game_state$initialized <- TRUE
    }
  })
  
  # Render loading screen
  output$loading_screen <- renderUI({
    if(!game_state$initialized) {
      div(
        style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; 
                 background-color: rgba(0,0,0,0.8); z-index: 9999; display: flex; 
                 justify-content: center; align-items: center; flex-direction: column;",
        h2("Menghitung Solusi Optimal...", style = "color: white;"),
        tags$img(src = "data:image/gif;base64,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......", 
             style = "width: 50px; height: 50px; margin-top: 20px;"),
        p("Mohon tunggu, sedang menghitung solusi optimal...", style = "color: white; margin-top: 20px;")
      )
    }
  })
  
  # Render main content
  output$main_content <- renderUI({
    if(game_state$initialized) {
      fluidRow(
        column(5,
          wellPanel(
            tags$table(
              style = "margin: 0 auto; border-collapse: separate; border-spacing: 25px 0; text-align: center;",
              tags$tr(
                tags$td("Langkah"),
                tags$td("Posisi Kotak Kosong"),
                tags$td("Hint Digunakan"),
                tags$td("Langkah Optimal")
              ),
              tags$tr(
                tags$td(textOutput("move_count")), 
                tags$td(textOutput("game_status")), 
                tags$td(textOutput("hint_count")),
                tags$td(textOutput("optimal_steps"))
              )
            ), 
            # Grid 3x3 menggunakan table untuk memastikan layout yang benar
            tags$table(
              style = "margin: 0 auto; border-collapse: separate; border-spacing: 5px;",
              tags$tr(
                tags$td(uiOutput("tile_1")),
                tags$td(uiOutput("tile_2")),
                tags$td(uiOutput("tile_3"))
              ),
              tags$tr(
                tags$td(uiOutput("tile_4")),
                tags$td(uiOutput("tile_5")),
                tags$td(uiOutput("tile_6"))
              ),
              tags$tr(
                tags$td(uiOutput("tile_7")),
                tags$td(uiOutput("tile_8")),
                tags$td(uiOutput("tile_9"))
              ),
              tags$tr(
                tags$td(actionButton("new_game", "New Game", class = "btn-success btn-block")),
                tags$td(actionButton("reset_game", "Reset", class = "btn-warning btn-block")),
                tags$td(actionButton("hint_button", "Hint", class = "btn-info btn-block"))
              )
            ),
            br(),
            uiOutput('game_state'),
            br(),
            uiOutput('hint_message')
          )
        ),
        column(7,
          wellPanel(
            h3("Cara Bermain"),
            p("Tujuan: Susun angka 1-8 secara berurutan dengan kotak kosong di pojok kanan bawah."),
            tags$ul(
              tags$li("Hanya angka yang bersebelahan langsung dengan kotak kosong yang dapat dipindahkan"),
              tags$li("Anda bisa memindahkan angka ke atas, bawah, kiri, atau kanan ke kotak kosong"),
              tags$li("Klik pada angka yang ingin Anda pindahkan (harus bersebelahan dengan kotak kosong)")
            ),
            br(),
            h4("Petunjuk:"),
            p("Klik kotak angka yang bersebelahan dengan kotak kosong untuk memindahkannya."),
            p("Susun angka 1-8 secara berurutan dengan kotak kosong di pojok kanan bawah.")
          )
        )
      )
    }
  })
  
  # Tangani klik pada kotak puzzle (melalui observer global)
  observe({
    # Cek semua kemungkinan input tile
    for(i in 1:9) {
      local({
        tile_id <- i
        input_name <- paste0("tile_", tile_id)
        
        observeEvent(input[[input_name]], {
          if(game_state$initialized && !game_state$game_won) {
            # Panggil move_tile dan dapatkan hasil dengan status pergerakan
            result <- move_tile(game_state$puzzle, tile_id)
            
            # Hanya update puzzle dan increment moves jika pergerakan valid
            if(result$moved) {
              game_state$puzzle <- result$puzzle
              game_state$moves <- game_state$moves + 1
              game_state$hint_position <- NULL  # Reset hint saat pemain bergerak
              
              # Cek apakah puzzle sudah selesai
              if(is_solved(game_state$puzzle)) {
                game_state$game_won <- TRUE
              }
            }
          }
        })
      })
    }
  })
  
  # Tombol permainan baru
  observeEvent(input$new_game, {
    if(game_state$initialized) {
      new_puzzle <- create_solvable_puzzle()
      game_state$puzzle <- new_puzzle
      game_state$initial_puzzle <- new_puzzle  # Update initial puzzle
      game_state$moves <- 0
      game_state$game_won <- FALSE
      game_state$hints_used <- 0
      game_state$hint_position <- NULL
      
      # Hitung langkah optimal untuk puzzle baru
      game_state$initialized <- FALSE  # Set ke false untuk menampilkan loading
      # Gunakan isolate untuk menghindari reactivity
      isolate({
        optimal_steps <- solve_puzzle_bfs(new_puzzle)
        game_state$optimal_steps <- optimal_steps
        game_state$initialized <- TRUE
      })
    }
  })
  
  # Tombol reset
  observeEvent(input$reset_game, {
    if(game_state$initialized && !is.null(game_state$initial_puzzle)) {
      game_state$puzzle <- game_state$initial_puzzle
      game_state$moves <- 0
      game_state$game_won <- FALSE
      game_state$hint_position <- NULL
    }
  })
  
  # Tombol hint
  observeEvent(input$hint_button, {
    if(game_state$initialized && !game_state$game_won && game_state$hints_used < 300) {
      hint_pos <- get_hint(game_state$puzzle)
      game_state$hint_position <- hint_pos
      game_state$hints_used <- game_state$hints_used + 1
    }
  })
  
  # Render setiap tile secara individual
  for(i in 1:9) {
    local({
      tile_index <- i
      
      output[[paste0("tile_", tile_index)]] <- renderUI({
        if(game_state$initialized) {
          value <- game_state$puzzle[tile_index]
          
          # Tentukan style berdasarkan apakah ini adalah hint
          is_hint <- !is.null(game_state$hint_position) && game_state$hint_position == tile_index
          hint_style <- if(is_hint) "box-shadow: 0 0 10px 3px orange; border: 3px solid orange;" else ""
          size <- 200
          
          if(value == 0) {
            # Kotak kosong
            div(
              style = paste0("width: ", size, "px; height: ", size, "px; border: 2px solid #333; 
                       background-color: #f0f0f0; display: flex; align-items: center; 
                       justify-content: center; font-size: 24px; font-weight: bold;", hint_style),
              ""
            )
          } else {
            # Kotak dengan angka
            actionButton(
              paste0("tile_", tile_index),
              label = as.character(value),
              style = paste0("width: ", size, "px; height: ", size, "px; border: 2px solid #333; 
                       background-color: #4CAF50; color: white; font-size: 24px; 
                       font-weight: bold;", hint_style)
            )
          }
        }
      })
    })
  }
  
  # Status permainan
  output$game_status <- renderText({
    if(game_state$initialized) {
      if(game_state$game_won) {
        return(paste("Selesai dalam", game_state$moves, "langkah!"))
      } else {
        blank_pos <- get_blank_position(game_state$puzzle)
        return(blank_pos)
      }
    }
  })
  
  # Jumlah langkah
  output$move_count <- renderText({
    if(game_state$initialized) {
      game_state$moves
    }
  })
  
  # Jumlah hint yang digunakan
  output$hint_count <- renderText({
    if(game_state$initialized) {
      paste(game_state$hints_used, "/ 300")
    }
  })
  
  # Langkah optimal
  output$optimal_steps <- renderText({
    if(game_state$initialized && !is.null(game_state$optimal_steps)) {
      game_state$optimal_steps
    }
  })
  
  # Pesan hint
  output$hint_message <- renderUI({
    if(game_state$initialized) {
      if(!is.null(game_state$hint_position) && game_state$hints_used <= 300 && !game_state$game_won) {
        div(
          style = "text-align: center; color: orange; font-size: 16px; font-weight: bold;",
          icon("lightbulb"),
          paste("Hint: Klik angka", game_state$puzzle[game_state$hint_position], "untuk bergerak ke arah solusi!")
        )
      } else if(game_state$hints_used >= 300 && !game_state$game_won) {
        div(
          style = "text-align: center; color: red; font-size: 16px; font-weight: bold;",
          "Anda telah menggunakan semua hint (3/3)"
        )
      }
    }
  })
  
  # Status kemenangan
  output$game_won <- reactive({
    return(game_state$game_won)
  })
  
  # Untuk conditionalPanel
  output$game_state <- renderUI({
    if(game_state$initialized) {
      req(game_state$game_won)
      if(game_state$game_won){
        div(
          style = "text-align: center; color: green; font-size: 20px; font-weight: bold;",
          icon("trophy"),
          "Selamat! Anda Menyelesaikan Puzzle!"
        )
      }
    }
  })
}

# Jalankan aplikasi
shinyApp(ui = ui, server = server)