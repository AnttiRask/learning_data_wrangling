# ---
# Title: "Data Wrangling with R, Chapter 14: Building an Application with Shiny in R - Spotify Example"
# author: "Marc Eixarch (original) | Antti Rask (modifications)"
# date: "2023-05-14"
# ---

# Building an Application with Shiny in R ####

## Loading Libraries ####
library(conflicted)   # An Alternative Conflict Resolution Strategy
library(extrafont)
library(ggExtra)
library(ggrepel)
# conflicts_prefer(plotly::layout)
# library(plotly)       # Create Interactive Web Graphics via 'plotly.js'
library(shiny)        # Web Application Framework for R
library(spotifyr)
# library(shinythemes)  # Themes for Shiny
library(tidyverse)    # Easily Install and Load the 'Tidyverse'

## User Interface ####
ui <- fluidPage(
  titlePanel("Spotify Playlist Generator"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("username", "Spotify Username"),
      passwordInput("client_id", "Spotify Client ID"),
      passwordInput("client_secret", "Spotify Client Secret"),
      actionButton("submit", "Generate Playlist")
    ),
    
    mainPanel(
      verbatimTextOutput("playlist")
    )
  )
)

## Server ####
server <- function(input, output, session) {
  observeEvent(input$submit, {
    # Ensure the username and API key fields are not empty
    if (input$username == "" | input$client_id == "" | input$client_secret == "") {
      showNotification("Please enter both a username and API keys.", type = "error")
      return()
    }
    
    # This is where you would insert your Spotify playlist creation code.
    # Replace the following line with your code, making sure to use the username, client ID and client secret
    # from the input$username, input$client_id and input$client_secret variables respectively.
    # playlist <- paste0("Generated playlist for ", input$username, ".")
    
    # Set Spotify API credentials
    Sys.setenv(SPOTIFY_CLIENT_ID     = input$client_id)
    Sys.setenv(SPOTIFY_CLIENT_SECRET = input$client_secret)
    options(spotifyr.client_id       = Sys.getenv('SPOTIFY_CLIENT_ID'))
    options(spotifyr.client_secret   = Sys.getenv('SPOTIFY_CLIENT_SECRET'))
    options(spotifyr.scopes          = 'user-top-read user-read-recently-played playlist-modify-public playlist-modify-private user-follow-read playlist-read-private playlist-read-collaborative')
    
    # Get Spotify API access token
    access_token <- get_spotify_access_token()
    
    # Get user's top artists
    my_top_artists <- get_my_top_artists_or_tracks(
      type       = "artists", 
      limit      = 50,           # the maximum
      time_range = "medium_term" # about 6 months history
    )
    
    # Get recommendations based on the chosen attributes and artists
    my_songs <- get_recommendations(
      seed_artists = head(my_top_artists, 5) %>% pull(id),
      min_energy   = 0.6,
      min_valence  = 0.6
    )
    
    # Create an empty playlist
    .playlist_id <- create_playlist(
      user_id     = input$username,
      name        = str_glue("My Songs ({today()})"),
      description = "Generated with R!"
    )$id
    
    # Populate the created playlist
    add_tracks_to_playlist(
      playlist_id = .playlist_id,
      uris        = my_songs$id
    )
    
    # Display the generated playlist
    output$playlist <- renderPrint({
      .playlist_id
    })
    
    # Erase the Spotify username and API keys at the end of the session
    onStop(function() {
      updateTextInput(session, "username", value = "")
      updatePasswordInput(session, "client_id", value = "")
      updatePasswordInput(session, "client_secret", value = "")
    })
  })
}

## Shiny App ####
shinyApp(ui = ui, server = server)

