library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(leaflet)
library(geosphere)
library(DT)
library(tidyr)
library(shinyjs)
library(future)
library(future.apply)
library(shinycssloaders)
library(promises)

# Set up parallel processing (multisession works on most platforms)
plan(multisession)

# --------------------------------------------------------------------
# Helper funkcije za izračun obilježja putanje

# Funkcija za računanje detalja fraktalne dimenzije korištenjem metode "divider" (linijski postupak)
compute_fractal_dimension_details <- function(track) {
  n <- nrow(track)
  if(n < 3) return(list(dimension = NA, eps_values = NA, L_eps = NA))
  
  # Izračun ukupne duljine putanje (suma udaljenosti između susjednih točaka)
  total_length <- sum(sapply(2:n, function(i) geosphere::distHaversine(
    c(track$longitude[i-1], track$latitude[i-1]),
    c(track$longitude[i], track$latitude[i])
  )))
  
  if(total_length < 1000) return(list(dimension = NA, eps_values = NA, L_eps = NA))
  
  # Definiranje raspona mjerilaca (epsilon) – koristimo logaritamski razmak
  min_eps <- 50      # minimalna vrijednost u metrima
  max_eps <- total_length / 2
  eps_values <- exp(seq(log(min_eps), log(max_eps), length.out = 10))
  
  L_eps <- numeric(length(eps_values))
  
  # Za svaki epsilon, "prošetaj" putanjom i broj koraka koji prelaze epsilon
  for(j in seq_along(eps_values)){
    eps <- eps_values[j]
    count <- 0
    cumulative <- 0
    for(i in 2:n) {
      d <- geosphere::distHaversine(
        c(track$longitude[i-1], track$latitude[i-1]),
        c(track$longitude[i], track$latitude[i])
      )
      cumulative <- cumulative + d
      if(cumulative >= eps) {
        count <- count + 1
        cumulative <- 0
      }
    }
    L_eps[j] <- count * eps
  }
  
  valid <- L_eps > 0
  if(sum(valid) < 2) return(list(dimension = NA, eps_values = eps_values, L_eps = L_eps))
  fit <- lm(log(L_eps[valid]) ~ log(eps_values[valid]))
  slope <- coef(fit)[2]
  # Prema formuli: log(L(eps)) = const + (1-D)*log(eps)  ⟹ D = 1 - slope
  fractal_dim <- 1 - slope
  
  return(list(dimension = fractal_dim, eps_values = eps_values, L_eps = L_eps))
}

# Pojednostavljena funkcija koja vraća samo fraktalnu dimenziju
compute_fractal_dimension <- function(track) {
  details <- compute_fractal_dimension_details(track)
  return(details$dimension)
}

# Funkcija za račun statističkih obilježja putanje zrakoplova
compute_metrics <- function(track) {
  # Osiguravamo da su podaci sortirani po vremenu
  track <- track[order(track$time), ]
  
  # Trajanje putovanja (u sekundama)
  duration <- as.numeric(max(track$time) - min(track$time))
  
  # Ukupna duljina putanje (suma udaljenosti između susjednih točaka u metrima)
  total_length <- sum(sapply(2:nrow(track), function(i) {
    geosphere::distHaversine(
      c(track$longitude[i-1], track$latitude[i-1]),
      c(track$longitude[i], track$latitude[i])
    )
  }))
  
  # Pravocrtnost: omjer udaljenosti "ravne linije" (između prve i zadnje točke) i ukupne duljine putanje
  straight_distance <- geosphere::distHaversine(
    c(track$longitude[1], track$latitude[1]),
    c(track$longitude[nrow(track)], track$latitude[nrow(track)])
  )
  straightness <- ifelse(total_length > 0, straight_distance / total_length, NA)
  
  # Srednja brzina gibanja (u m/s)
  mean_velocity <- ifelse(duration > 0, total_length / duration, NA)
  
  # Difuzijska udaljenost: RMS udaljenost svih točaka od početne točke
  distances_from_start <- sapply(1:nrow(track), function(i) {
    geosphere::distHaversine(
      c(track$longitude[1], track$latitude[1]),
      c(track$longitude[i], track$latitude[i])
    )
  })
  diffusion_distance <- sqrt(mean(distances_from_start^2))
  
  # Fraktalna dimenzija
  fd_details <- compute_fractal_dimension_details(track)
  fractal_dim <- fd_details$dimension
  
  # Sastavljamo rezultat kao data.frame
  metrics <- data.frame(
    Metric = c("Difuzijska udaljenost (m)", "Pravocrtnost", "Trajanje (s)", "Srednja brzina (m/s)", "Fraktalna dimenzija"),
    Value = c(diffusion_distance, straightness, duration, mean_velocity, fractal_dim)
  )
  return(metrics)
}

# --------------------------------------------------------------------
# Funkcije za dohvaćanje podataka

get_track_data <- function(icao24, time, username = NULL, password = NULL) {
  base_url <- "https://opensky-network.org/api/tracks/all"
  response <- GET(base_url, 
                  query = list(icao24 = icao24, time = time),
                  authenticate(username, password, type = "basic"))
  if(response$status_code != 200) {
    stop(paste("Error fetching track data: Status", response$status_code, content(response, as = "text")))
  }
  track_data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  return(track_data)
}

get_flight_data <- function(departure_airport, flight_datetime, username = NULL, password = NULL) {
  base_url <- "https://opensky-network.org/api/flights/departure"
  # Definiramo vremenski prozor: 30 minuta prije i poslije zadanog vremena.
  begin_time <- as.numeric(flight_datetime) - 1800
  end_time <- as.numeric(flight_datetime) + 1800
  
  full_url <- httr::modify_url(base_url, query = list(
    airport = departure_airport,
    begin = begin_time,
    end = end_time
  ))
  
  print(paste("Requesting data for", departure_airport, "from", begin_time, "to", end_time))
  print(paste("Full request URL:", full_url))
  
  response <- GET(base_url, 
                  query = list(
                    airport = departure_airport,
                    begin = begin_time,
                    end = end_time
                  ),
                  authenticate(username, password, type = "basic"))
  print(paste("Status Code:", response$status_code))
  if (response$status_code == 200) {
    flights <- fromJSON(content(response, as = "text"))
    if (length(flights) == 0) {
      stop("Nema podataka za zadane parametre.")
    }
    flights <- as.data.frame(flights)
    flights <- na.omit(flights)
    return(flights)
  } else {
    stop(paste("Greška pri dohvaćanju podataka. Status kod:", response$status_code, 
               content(response, as = "text")))
  }
}

format_flight_data <- function(flights) {
  flights <- flights %>% 
    left_join(airport_data, by = c("estDepartureAirport" = "icao_code")) %>% 
    rename(
      depLatitude = latitude,
      depLongitude = longitude
    ) %>% 
    left_join(airport_data, by = c("estArrivalAirport" = "icao_code")) %>% 
    rename(
      arrLatitude = latitude,
      arrLongitude = longitude
    ) %>% 
    drop_na() %>% 
    mutate(
      firstSeen = format(as.POSIXct(firstSeen, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S"),
      lastSeen = format(as.POSIXct(lastSeen, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S"),
      estDepartureAirportHorizDistance = estDepartureAirportHorizDistance / 1000,
      estArrivalAirportHorizDistance = estArrivalAirportHorizDistance / 1000,
      estDepartureAirportVertDistance = estDepartureAirportVertDistance / 1000,
      estArrivalAirportVertDistance = estArrivalAirportVertDistance / 1000
    ) %>% 
    select(
      icao24,
      callsign,
      firstSeen,
      estDepartureAirport,
      depLatitude,
      depLongitude,
      estArrivalAirport,
      arrLatitude,
      arrLongitude,
      lastSeen,
      estDepartureAirportHorizDistance,
      estArrivalAirportHorizDistance,
      estDepartureAirportVertDistance,
      estArrivalAirportVertDistance,
      departureAirportCandidatesCount,
      arrivalAirportCandidatesCount
    ) %>% 
    rename(
      "ICAO zrakoplova (jedinstveni kod Međ. org. civ. zrakoplovstva)" = icao24,
      "Pozivni znak leta" = callsign,
      "Zračna luka polaska" = estDepartureAirport,
      "Širina zračne luke polaska" = depLatitude,
      "Dužina zračne luke polaska" = depLongitude,
      "Zračna luka dolaska" = estArrivalAirport,
      "Širina zračne luke dolaska" = arrLatitude,
      "Dužina zračne luke dolaska" = arrLongitude,
      "Prvi put detektirano" = firstSeen,
      "Posljednji put detektirano" = lastSeen,
      "Horizontalna udaljenost polaska (km)" = estDepartureAirportHorizDistance,
      "Horizontalna udaljenost dolaska (km)" = estArrivalAirportHorizDistance,
      "Vertikalna udaljenost polaska (km)" = estDepartureAirportVertDistance,
      "Vertikalna udaljenost dolaska (km)" = estArrivalAirportVertDistance,
      "Broj kandidata za zračnu luku polaska" = departureAirportCandidatesCount,
      "Broj kandidata za zračnu luku dolaska" = arrivalAirportCandidatesCount
    )
  
  return(flights)
}

# --------------------------------------------------------------------
# Učitavanje statičkih podataka (airports, airlines)

airport_data <- read.csv(file.path(getwd(), "airports.csv"), stringsAsFactors = FALSE)
colnames(airport_data) <- c("id", "name", "city", "country", "iata_code", 
                            "icao_code", "latitude", "longitude", "altitude", 
                            "timezone", "dst", "tz_database_time_zone", 
                            "type", "source")
airport_data <- airport_data[, c("name", "city", "icao_code", "latitude", "longitude")]

airlines_data <- read.csv(file.path(getwd(), "airlines.csv"), header = TRUE, stringsAsFactors = FALSE)
colnames(airlines_data) <- c("id", "name", "alias", "IATA", "ICAO", "callsign", "country", "active")
airlines_data <- airlines_data[, c("ICAO", "name")]

# --------------------------------------------------------------------
# Definicija grafičkog korisničkog sučelja (UI)

ui <- navbarPage(
  "Dohvat i analiza letova",
  useShinyjs(),
  # Prvi tab – dohvat podataka, prikaz karte i tablice
  tabPanel("Podaci",
           sidebarLayout(
             sidebarPanel(
               textInput("flight_number", "Broj leta (opcionalno):", value = ""),
               # Removed separate search fields; using selectize with built-in search
               selectizeInput(
                 inputId = "airline",
                 label   = "Odaberi aviokompaniju:",
                 choices = NULL,
                 options = list(
                   placeholder = 'Počni tipkati za pretraživanje…',
                   allowEmptyOption = FALSE),
                 selected = NULL
               ),
               selectizeInput(
                 inputId = "airport",
                 label   = "Odaberi zračnu luku:",
                 choices = NULL,
                 options = list(
                   placeholder = 'Počni tipkati za pretraživanje…',
                   allowEmptyOption = FALSE),
                 selected = NULL
               ),
               dateInput("flight_date", "Datum leta:", value = Sys.Date(), max = Sys.Date()),
               textInput("departure_time", "Sat polijetanja (HH:MM):", value = "12:00"),
               textInput("username", "Korisničko ime (opcionalno):", value = "filipparis11"),
               passwordInput("password", "Lozinka (opcionalno):", value = "filipparis11"),
               actionButton("fetch", "Dohvati podatke"),
               downloadButton("downloadData", "Download Data"),
               width = 3
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Karta letova",
                          hidden(
                            div(
                              id = "map-spinner",
                              style = "
                             position:absolute;
                             top:0; left:0;
                             width:100%; height:100%;
                             background: rgba(255,255,255,0.7);
                             z-index:1000;
                             display:flex;
                             align-items:center;
                             justify-content:center;
                             ",
                              icon("spinner", class = "fa-spin fa-3x fa-fw")
                            )
                          ),
                          # map container hidden until data arrives
                            hidden(
                              div(
                                id = "map-container", style = "position:relative; height:600px;",
                                leafletOutput("map", height = "600px")
                                )
                              )
                          ),
                 tabPanel("Tablica podataka",
                          # spinner overlay (initially hidden)
                          hidden(
                            div(
                              id = "table-spinner",
                              style = "
                               position: absolute;
                               top: 0; left: 0;
                               width: 100%; height: 100%;
                               background: rgba(255,255,255,0.7);
                               z-index: 1000;
                               display: flex;
                               align-items: center;
                               justify-content: center;
                             ",
                              icon("spinner", class = "fa-spin fa-3x fa-fw")
                              )
                            ),
                          # actual table container (initially hidden)
                            hidden(
                              div(
                                id = "table-container",
                                 DTOutput("flight_table")
                                )
                              )
                          )
               ),
               textOutput("status_message")
             )
           )
  ),
  
  # Drugi tab – analiza odabranog leta
  tabPanel("Analiza leta",
           fluidPage(
             h3("Analiza odabranog leta"),
             textOutput("selected_flight_info"),
             br(),
             h4("Statistička obilježja putanje"),
             tableOutput("metrics_table"),
             br(),
             h4("Analiza fraktalne dimenzije"),
             plotOutput("fractal_plot"),
             br(),
             h4("Putanja odabranog leta"),
             withSpinner(
               leafletOutput("selected_map", height = "500px"),
               type  = 4,
               color = "#2C3E50"
               )
           )
  )
)

# --------------------------------------------------------------------
# Definicija server logike

server <- function(input, output, session) {
  rv <- reactiveValues(data = NULL)
  
  flight_table_proxy <- dataTableProxy("flight_table")
  
  disable("downloadData")
  
  updateSelectizeInput(
    session,
    "airline",
    choices = setNames(airlines_data$ICAO, airlines_data$name),
    selected = character(0),
    server = TRUE
  )
  updateSelectizeInput(
    session,
    "airport",
    choices = setNames(
      airport_data$icao_code,
      paste0(airport_data$name, " (", airport_data$city, ")")
    ),
    selected = character(0),
    server = TRUE
  )
  
  # Download handler: include track coordinates for each flight when exporting
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("flight_data_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv", sep = "")
    },
    content = function(file) {
      # Prepare export: include all columns plus a collapsed coordinate string
      export_df <- rv$data %>%
        rowwise() %>%
        mutate(
          track_coords = if (!is.null(track)) {
            paste0(
              "[",
              paste(paste(track$latitude, track$longitude, sep = ","), collapse = ";"),
              "]"
            )
          } else {
            NA_character_
          }
        ) %>%
        ungroup() %>%
        select(-track)
      
      write.csv(export_df, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$fetch, {
    # clear old notifications and start departure-phase spinner
    removeNotification(id = "notif_dep"); removeNotification(id = "notif_trk")
    #–– map spinners
    hide("map-container")
    show("map-spinner")
    #–– table spinners
    hide("table-container")
    show("table-spinner")
    showNotification("Dohvaćam podatke o polascima…", id = "notif_dep", type = "message", duration = NULL)
    output$status_message <- renderText({ "Dohvaćam podatke o polascima…" })
    
    flight_datetime <- as.POSIXct(paste(input$flight_date, input$departure_time),
                                  format = "%Y-%m-%d %H:%M",
                                  tz = "UTC")
    if(is.na(flight_datetime)) {
      output$status_message <- renderText({ "Neispravan format vremena. Koristite HH:MM." })
      return()
    }
    
    tryCatch({
      flights <- get_flight_data(
        departure_airport = input$airport,
        flight_datetime = flight_datetime,
        username = input$username,
        password = input$password
      )
      
      # departure-phase done: hide spinner, notify success
       removeNotification(id = "notif_dep")
       hide("map-spinner")
       showNotification("Podaci o polascima dohvaćeni!", type = "message", duration = 2)
       output$status_message <- renderText({ "Podaci o polascima dohvaćeni, dohvaćam tragove…" })
      
      flights <- flights %>%
        mutate(trim_callsign = trimws(callsign)) %>%
        mutate(airline_prefix = sub("^(\\D+).*", "\\1", trim_callsign))
      
      if(nchar(input$flight_number) > 0) {
        flights <- flights %>% filter(trim_callsign == input$flight_number)
      }
      
      flights <- flights %>% filter(airline_prefix == input$airline)
      
      if(nrow(flights) == 0) {
        stop("Nema podataka za zadane parametre: provjerite broj leta (ako je uneseno), aviokompaniju, zračnu luku polaska, datum i vrijeme polijetanja.")
      }
      
      formatted_flights <- format_flight_data(flights)
      
      user <- input$username
      pass <- input$password
      total_requests <- nrow(formatted_flights)
      
      showNotification("Dohvaćam podatke o tragovima…", id = "notif_trk", type = "message", duration = NULL)
      # track-phase spinner
      show("map-spinner")
      track_results <- future_lapply(seq_len(total_requests), function(i, flights, user, pass, total_requests) {
        library(httr)
        library(jsonlite)
        library(lubridate)
        
        print(paste("Fetching track data request", i, "of", total_requests))
        flight <- flights[i, ]
        t1 <- as.numeric(as.POSIXct(flight$`Prvi put detektirano`, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
        t2 <- as.numeric(as.POSIXct(flight$`Posljednji put detektirano`, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
        mid_time <- round((t1 + t2) / 2)
        
        track_data <- tryCatch({
          get_track_data(
            icao24 = flight$`ICAO zrakoplova (jedinstveni kod Međ. org. civ. zrakoplovstva)`,
            time = mid_time,
            username = user,
            password = pass
          )
        }, error = function(e) NULL)
        
        if(!is.null(track_data) && !is.null(track_data$path) && length(track_data$path) > 0) {
          if(is.matrix(track_data$path)) {
            track_df <- as.data.frame(track_data$path)
          } else {
            track_df <- as.data.frame(do.call(rbind, track_data$path))
          }
          colnames(track_df) <- c("time", "latitude", "longitude", "baro_altitude", "true_track", "on_ground")
          return(track_df)
        } else {
          return(NULL)
        }
      },
      flights = formatted_flights,
      user = user,
      pass = pass,
      total_requests = total_requests
      )
      
      formatted_flights$track <- track_results
      
      # Filtriranje letova: isključiti ako nema track podataka ili zadnja točka nije dovoljno blizu dolazne luke.
      valid_flights <- formatted_flights %>%
        rowwise() %>%
        filter({
          tol <- 3000  # tolerancija u metrima (mijenjajte po potrebi)
          if(is.null(track) || nrow(track) == 0) {
            FALSE
          } else {
            arrival_lon <- as.numeric(`Dužina zračne luke dolaska`)
            arrival_lat <- as.numeric(`Širina zračne luke dolaska`)
            last_point <- tail(track, 1)
            d <- geosphere::distHaversine(c(last_point$longitude, last_point$latitude),
                                          c(arrival_lon, arrival_lat))
            d <= tol
          }
        }) %>% ungroup()
      
      rv$data <- valid_flights
      
      # and *then* update the existing DT via the proxy:
      replaceData(
        flight_table_proxy,
        rv$data %>% select(-track),
        resetPaging = TRUE
      )
      
      # track-phase done: hide spinner, notify success
      removeNotification(id = "notif_trk")
      hide("map-spinner")
      show("map-container")
      hide("table-spinner")
      show("table-container")
      showNotification("Podaci o tragovima uspješno dohvaćeni!", type = "message", duration = 2)
      output$status_message <- renderText({ "Podaci uspješno dohvaćeni!" })
      enable("downloadData")
      
    }, error = function(e) {
      hide("map-spinner")
      hide("map-container")
      hide("table-spinner")
      hide("table-container")
      output$status_message <- renderText({ paste("Greška prilikom dohvaćanja podataka:", e$message) })
    })
  })
  
  output$flight_table <- renderDT({
    req(rv$data)
    datatable(rv$data %>% select(-track), 
              selection = "single", 
              options = list(scrollX = TRUE, autoWidth = TRUE))
  })
  
  output$map <- renderLeaflet({
    # Base map with world‐wrapping enabled
    m <- leaflet(options = leafletOptions(worldCopyJump = TRUE)) %>% 
      addTiles()
    
    # If there's no flight data, just return the empty map
    if (is.null(rv$data) || nrow(rv$data) == 0) {
      return(m)
    }
    
    # Coerce the airport coords to numeric
    df <- rv$data %>%
      mutate_at(vars(
        `Širina zračne luke polaska`,
        `Dužina zračne luke polaska`,
        `Širina zračne luke dolaska`,
        `Dužina zračne luke dolaska`
      ), as.numeric) %>%
      filter(
        !is.na(`Širina zračne luke polaska`),
        !is.na(`Dužina zračne luke polaska`),
        !is.na(`Širina zračne luke dolaska`),
        !is.na(`Dužina zračne luke dolaska`)
      )
    
    # Add departure (blue) and arrival (red) markers
    m <- m %>%
      addCircleMarkers(
        lng     = df$`Dužina zračne luke polaska`,
        lat     = df$`Širina zračne luke polaska`,
        color   = "blue", radius = 5,
        popup   = paste0("<strong>Polazak:</strong> ", df$`Zračna luka polaska`)
      ) %>%
      addCircleMarkers(
        lng     = df$`Dužina zračne luke dolaska`,
        lat     = df$`Širina zračne luke dolaska`,
        color   = "red",  radius = 5,
        popup   = paste0("<strong>Dolazak:</strong> ", df$`Zračna luka dolaska`)
      )
    
    # Draw each track (noClip = TRUE so it spans map edges)
    for (i in seq_len(nrow(df))) {
      trk <- df$track[[i]]
      if (is.data.frame(trk) && nrow(trk) >= 2) {
        m <- m %>%
          addPolylines(
            lng     = trk$longitude,
            lat     = trk$latitude,
            color   = "purple",
            weight  = 3,
            options = pathOptions(noClip = TRUE)
          )
      }
    }
    
    m
  })
  
  # ----------------------------------------------------------------
  # Reaktivne varijable i izlazi za analizu odabranog leta
  
  selected_flight <- reactive({
    req(rv$data)
    s <- input$flight_table_rows_selected
    if(length(s) == 0) return(NULL)
    rv$data[s, ]
  })
  
  # Iz teksta selektiranog leta, ispisujemo neke osnovne informacije
  output$selected_flight_info <- renderText({
    sf <- selected_flight()
    if(is.null(sf)) {
      "Nije odabran niti jedan let."
    } else {
      paste("Odabrani let:", sf$`Pozivni znak leta`, "od", sf$`Zračna luka polaska`, "do", sf$`Zračna luka dolaska`,
            "\nPrvi put detektirano:", sf$`Prvi put detektirano`, " | Posljednji put detektirano:", sf$`Posljednji put detektirano`)
    }
  })
  
  # Reaktivno računanje statističkih obilježja za odabrani let
  selected_metrics <- reactive({
    sf <- selected_flight()
    if(is.null(sf)) return(NULL)
    track <- sf$track[[1]]
    if(is.null(track) || nrow(track) == 0) return(NULL)
    compute_metrics(track)
  })
  
  output$metrics_table <- renderTable({
    req(selected_metrics())
    selected_metrics()
  })
  
  # Reaktivno dobivanje podataka za analizu fraktalne dimenzije
  selected_fractal_details <- reactive({
    sf <- selected_flight()
    if(is.null(sf)) return(NULL)
    track <- sf$track[[1]]
    if(is.null(track) || nrow(track) == 0) return(NULL)
    compute_fractal_dimension_details(track)
  })
  
  # Grafički prikaz analize fraktalne dimenzije: log-log dijagram L(eps) ovisno o eps
  output$fractal_plot <- renderPlot({
    fd <- selected_fractal_details()
    req(fd)
    valid <- !is.na(fd$eps_values) & !is.na(fd$L_eps) & (fd$L_eps > 0)
    if(sum(valid) < 2) {
      plot.new()
      text(0.5, 0.5, "Nema dovoljno podataka za analizu fraktalne dimenzije.")
    } else {
      plot(log(fd$eps_values[valid]), log(fd$L_eps[valid]),
           xlab = "log(eps)", ylab = "log(L(eps))", main = "Analiza fraktalne dimenzije", pch = 16)
      fit <- lm(log(fd$L_eps[valid]) ~ log(fd$eps_values[valid]))
      abline(fit, col = "red", lwd = 2)
      legend("bottomright", legend = paste("Fraktalna dimenzija =", round(1 - coef(fit)[2], 3)),
             bty = "n")
    }
  })
  
  # Posebna karta za odabrani let: prikazuje samo putanju odabranog leta
  output$selected_map <- renderLeaflet({
    sf <- selected_flight()
    req(sf)
    track <- sf$track[[1]]
    if(is.null(track) || nrow(track) == 0) return(NULL)
    
    # Početna i krajnja točka
    departure <- c(as.numeric(sf$`Dužina zračne luke polaska`), as.numeric(sf$`Širina zračne luke polaska`))
    arrival <- c(as.numeric(sf$`Dužina zračne luke dolaska`), as.numeric(sf$`Širina zračne luke dolaska`))
    
    map <- leaflet() %>% addTiles() %>%
      addCircleMarkers(lng = departure[1], lat = departure[2],
                       color = "blue", radius = 6,
                       popup = paste("<strong>Zračna luka polaska:</strong>", sf$`Zračna luka polaska`)) %>%
      addCircleMarkers(lng = arrival[1], lat = arrival[2],
                       color = "red", radius = 6,
                       popup = paste("<strong>Zračna luka dolaska:</strong>", sf$`Zračna luka dolaska`))
    
    coords <- as.matrix(track[, c("longitude", "latitude")])
    map <- map %>% addPolylines(lng = coords[,1], lat = coords[,2], color = "purple", weight = 3)
    map
  })
}

shinyApp(ui = ui, server = server)