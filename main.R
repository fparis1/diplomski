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

# Set up parallel processing (multisession works on most platforms)
plan(multisession)

# ----- Helper Function: Fetch Track Data -----
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

# ----- Modified Function for Flight Retrieval -----
# Fetches flights from the "flights/departure" endpoint for the given departure airport and time window.
get_flight_data <- function(departure_airport, flight_datetime, username = NULL, password = NULL) {
  base_url <- "https://opensky-network.org/api/flights/departure"
  # Define the time window: 30 minutes before and after the given time.
  begin_time <- as.numeric(flight_datetime) - 1800
  end_time <- as.numeric(flight_datetime) + 1800
  
  # Build the full request URL with query parameters for console output.
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
    # Convert to data frame and remove any rows containing null (NA) values.
    flights <- as.data.frame(flights)
    flights <- na.omit(flights)
    return(flights)
  } else {
    stop(paste("Greška pri dohvaćanju podataka. Status kod:", response$status_code, 
               content(response, as = "text")))
  }
}

# ----- Function for Formatting Flight Data (unchanged) -----
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

# ----- Load Static Data (airports, airlines) -----
airport_data <- read.csv(file.path(getwd(), "airports.csv"), stringsAsFactors = FALSE)
colnames(airport_data) <- c("id", "name", "city", "country", "iata_code", 
                            "icao_code", "latitude", "longitude", "altitude", 
                            "timezone", "dst", "tz_database_time_zone", 
                            "type", "source")
airport_data <- airport_data[, c("name", "city", "icao_code", "latitude", "longitude")]

# Load airlines.csv – assuming the file has headers
airlines_data <- read.csv(file.path(getwd(), "airlines.csv"), header = TRUE, stringsAsFactors = FALSE)
colnames(airlines_data) <- c("id", "name", "alias", "IATA", "ICAO", "callsign", "country", "active")
# Use only the "ICAO" and "name" columns
airlines_data <- airlines_data[, c("ICAO", "name")]


airports_full_names <- list(
  "KATL" = "Hartsfield-Jackson Atlanta International Airport (Atlanta, USA)",
  "ZBAA" = "Beijing Capital International Airport (Beijing, China)",
  "KLAX" = "Los Angeles International Airport (Los Angeles, USA)",
  "OMDB" = "Dubai International Airport (Dubai, UAE)",
  "RJTT" = "Tokyo Haneda Airport (Tokyo, Japan)",
  "KORD" = "O'Hare International Airport (Chicago, USA)",
  "EGLL" = "London Heathrow Airport (London, UK)",
  "ZSPD" = "Shanghai Pudong International Airport (Shanghai, China)",
  "LFPG" = "Charles de Gaulle Airport (Paris, France)"
)

# ----- Define Shiny UI -----
# Note: The "Analiza putanje" tab has been removed.
ui <- navbarPage(
  "Dohvat i analiza letova",
  useShinyjs(),
  tabPanel("Podaci",
           sidebarLayout(
             sidebarPanel(
               # Input for flight number (optional)
               textInput("flight_number", "Broj leta (opcionalno):", value = ""),
               
               # Text input for airline search and dynamic select input for filtered results
               textInput("airline_search", "Pretraži aviokompanije:", value = ""),
               uiOutput("airline_select_ui"),
               
               textInput("airport_search", "Pretraži zračne luke:", value = ""),
               uiOutput("airport_select_ui"),
               
               # Date and time inputs for flight
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
                 tabPanel("Karta letova", leafletOutput("map", height = "600px")),
                 tabPanel("Tablica podataka", DTOutput("flight_table"))
               ),
               textOutput("status_message")
             )
           )
  )
)

# ----- Define Shiny Server -----
server <- function(input, output, session) {
  rv <- reactiveValues(data = NULL)
  
  disable("downloadData")
  
  # Reactive filtering of airlines_data based on the airline search text.
  filtered_airlines <- reactive({
    if (nchar(input$airline_search) == 0) {
      airlines_data
    } else {
      search_term <- tolower(input$airline_search)
      airlines_data[grepl(search_term, tolower(airlines_data$name)), ]
    }
  })
  
  # Dynamically generate selectInput for airline selection based on the filtered results.
  output$airline_select_ui <- renderUI({
    choices <- setNames(filtered_airlines()$ICAO, filtered_airlines()$name)
    selectInput("airline", "Odaberi aviokompaniju:", choices = choices)
  })
  
  ## Reactive filtering of airports_data based on the search text.
  filtered_airports <- reactive({
    if (nchar(input$airport_search) == 0) {
      airport_data
    } else {
      search_term <- tolower(input$airport_search)
      airport_data[grepl(search_term, tolower(airport_data$name)) | 
                     grepl(search_term, tolower(airport_data$city)), ]
    }
  })
  
  # Dynamically generate selectInput for airport selection with name and city.
  output$airport_select_ui <- renderUI({
    choices <- setNames(filtered_airports()$icao_code, 
                        paste0(filtered_airports()$name, " (", filtered_airports()$city, ")"))
    selectInput("airport", "Odaberi zračnu luku:", choices = choices)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("flight_data_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$data, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$fetch, {
    output$status_message <- renderText({ "Dohvaćam podatke..." })
    
    # Combine flight date and departure time into one datetime.
    flight_datetime <- as.POSIXct(paste(input$flight_date, input$departure_time),
                                  format = "%Y-%m-%d %H:%M",
                                  tz = "UTC")
    if (is.na(flight_datetime)) {
      output$status_message <- renderText({ "Neispravan format vremena. Koristite HH:MM." })
      return()
    }
    
    tryCatch({
      # Fetch flight data from the API using the specified departure airport and time window.
      flights <- get_flight_data(
        departure_airport = input$airport,
        flight_datetime = flight_datetime,
        username = input$username,
        password = input$password
      )
      
      flights <- flights %>%
        mutate(trim_callsign = trimws(callsign)) %>%
        mutate(airline_prefix = sub("^(\\D+).*", "\\1", trim_callsign))
      
      # If a flight number is provided, filter by it; otherwise skip this filter.
      if (nchar(input$flight_number) > 0) {
        flights <- flights %>% filter(trim_callsign == input$flight_number)
      }
      
      flights <- flights %>% filter(airline_prefix == input$airline)
      
      if (nrow(flights) == 0) {
        stop("Nema podataka za zadane parametre: provjerite broj leta (ako je uneseno), aviokompaniju, zračnu luku polaska, datum i vrijeme polijetanja.")
      }
      
      # Format the flight data.
      formatted_flights <- format_flight_data(flights)
      
      # Create local copies of needed variables for the parallel workers.
      user <- input$username
      pass <- input$password
      total_requests <- nrow(formatted_flights)
      
      # Execute the track requests in parallel with progress messages.
      track_results <- future_lapply(seq_len(total_requests), function(i, flights, user, pass, total_requests) {
        # Load required libraries in the worker
        library(httr)
        library(jsonlite)
        library(lubridate)
        
        # Print progress to the terminal (each worker prints its own message)
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
        
        if (!is.null(track_data) && !is.null(track_data$path) && length(track_data$path) > 0) {
          if (is.matrix(track_data$path)) {
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
      
      rv$data <- formatted_flights
      
      output$status_message <- renderText({ "Podaci uspješno dohvaćeni!" })
      enable("downloadData")
      
    }, error = function(e) {
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
    req(rv$data)
    # Start with base map and add markers for departure and arrival airports
    formatted_flights <- rv$data %>%
      mutate(
        `Širina zračne luke polaska` = as.numeric(`Širina zračne luke polaska`),
        `Dužina zračne luke polaska` = as.numeric(`Dužina zračne luke polaska`),
        `Širina zračne luke dolaska` = as.numeric(`Širina zračne luke dolaska`),
        `Dužina zračne luke dolaska` = as.numeric(`Dužina zračne luke dolaska`)
      ) %>%
      filter(
        !is.na(`Širina zračne luke polaska`),
        !is.na(`Dužina zračne luke polaska`),
        !is.na(`Širina zračne luke dolaska`),
        !is.na(`Dužina zračne luke dolaska`),
        `Širina zračne luke polaska` >= -90 & `Širina zračne luke polaska` <= 90,
        `Dužina zračne luke polaska` >= -180 & `Dužina zračne luke polaska` <= 180,
        `Širina zračne luke dolaska` >= -90 & `Širina zračne luke dolaska` <= 90,
        `Dužina zračne luke dolaska` >= -180 & `Dužina zračne luke dolaska` <= 180,
        `Zračna luka polaska` != `Zračna luka dolaska`
      )
    
    map <- leaflet(data = formatted_flights) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~`Dužina zračne luke polaska`,
        lat = ~`Širina zračne luke polaska`,
        color = "blue", radius = 5,
        popup = ~paste("<strong>Zračna luka polaska:</strong>", `Zračna luka polaska`)
      ) %>%
      addCircleMarkers(
        lng = ~`Dužina zračne luke dolaska`,
        lat = ~`Širina zračne luke dolaska`,
        color = "red", radius = 5,
        popup = ~paste("<strong>Zračna luka dolaska:</strong>", `Zračna luka dolaska`)
      )
    
    # For each flight, if actual track data is available, draw the polyline.
    for (i in 1:nrow(formatted_flights)) {
      track_df <- formatted_flights$track[[i]]
      if(!is.null(track_df)) {
        coords <- as.matrix(track_df[, c("longitude", "latitude")])
        map <- map %>% addPolylines(lng = coords[,1], lat = coords[,2], color = "purple", weight = 3)
      }
    }
    map
  })
}

shinyApp(ui = ui, server = server)
