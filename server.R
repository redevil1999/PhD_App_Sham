library(shiny)
library(tidyverse)
library(gganimate)
library(ggforce)
library(gifski)
library(lubridate)
library(shinyauthr)
library(rsconnect)
library(googlesheets4)
library(googledrive)
library(viridis)

# interactive, generates token
#options(gargle_oauth_cache = ".secrets")
#googledrive::drive_auth()

# non-interactive
googledrive::drive_auth(cache = ".secrets", email = "re.devillers1999@gmail.com")
googlesheets4::gs4_auth(token = drive_token(), email = TRUE)

# Load existing user data from Google Sheet
user_data_from_google <- read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/1XKnyX0lrGj2I8563VSq_aRqjuLdFXT5cKMVT5mQ3-DE/edit?usp=sharing",
  sheet = "Users",
  col_names = TRUE
)

# Convert passwords to hashed versions if they are plain text
user_base <- tibble::tibble(
  user = user_data_from_google$user,
  password = sapply(user_data_from_google$password, sodium::password_store), # Hashes passwords
  permissions = user_data_from_google$permissions,
  name = user_data_from_google$name,
  breathing_duration = user_data_from_google$breathing_duration
)

# New save session data functions
# Function to append session data to Google Sheet
saveSessionData <- function(user, session_duration, total_time_spent, completed_sessions, session_started) {
  new_row <- data.frame(
    user = user,
    session_duration = session_duration / 60,  # Convert to minutes
    total_time_spent = total_time_spent / 60,  # Convert to minutes
    completed_sessions = completed_sessions,
    session_started = session_started,
    timestamp = Sys.time()  # Add a timestamp for when the session was completed
  )
  
  # Append the new row to the Google Sheet
  sheet_append(
    ss = "https://docs.google.com/spreadsheets/d/1XKnyX0lrGj2I8563VSq_aRqjuLdFXT5cKMVT5mQ3-DE/edit?usp=sharing", 
    data = new_row,
    sheet = "Data"
  )
}

# # Save session data function
# saveSessionData <- function(data) {
#   data <- data.frame(data)
#   sheet_append(data, ss = "https://docs.google.com/spreadsheets/d/1ZfrZlrSlvWupOObvCFwWH06nULWElCXzHnxBbBqTfeU/edit?usp=sharing", sheet = "Data")
# }

# Load existing session data from Google
session_tracker <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1XKnyX0lrGj2I8563VSq_aRqjuLdFXT5cKMVT5mQ3-DE/edit?usp=sharing", sheet = "Data", col_names = TRUE)
session_tracker <- as_tibble(session_tracker)  # Ensure it's a tibble for consistency

length_42 <- c(seq(0.2, 10, by = 0.5), 10, 10, rev(seq(0.2, 10, by = 0.5)))
length_22 <-  c(seq(0.2, 10, by = 1), 10, 10, rev(seq(0.2, 10, by = 1)))
length_16 <-  c(seq(0.2, 10, by = 1.5), 10, 10, rev(seq(0.2, 10, by = 1.5)))

big_list <- list(length_42, length_22, length_16)
new_list <- sample(big_list, 3)
new_list <- unlist(new_list)
new_list

# dataframe for the circle
breathing_circle_data <- data.frame(x0 = c(5, each = 5),
                                    y0 = c(5, each = 5),
                                    r = new_list,
                                    speed_maybe = c(1:80))

server <- function(input, output, session) {
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # Logout to hide
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  # Default breathing time
  default_sessiontime <- 10
  
  # UI for logged-in users
  output$sidebarpanel <- renderUI({
    req(credentials()$user_auth)
    column(width = 4, p(paste("You have", credentials()$info[["permissions"]], "permission")))
    
    navbarPage('NTNU',
               tabPanel('Coherence Breathing',
                        paste("Hello", credentials()$info$name, "Welcome to the stress management app!"),
                        p("Remember to pay close attention to the circle."),
                        p("Try to notice changes in any aspect, for example speed or colour."),
                          mainPanel(plotOutput("distPlot"),
                                    hr(),
                                    actionButton('start', 'Start'),
                                    actionButton('stop', 'Stop'),
                                    actionButton('reset', 'Reset'),
                                    numericInput('minutes', 'Minutes:', value = default_sessiontime, min = 0, max = 99999, step = 1),
                                    textOutput('timeleft'),
                                    paste("If you would like to change the duration, remember to press reset after entering a different number.")
                        )
               ),
               tabPanel('Session Completion',
                        # Display session count for logged-in user
                        tableOutput("session_count"),
                        paste("This table displays the number of completed sessions and the total amount of time you have spend on the sessions from the stress management tool."))
    )
  })
  
  # Create Timer
  timer <- reactiveVal(default_sessiontime * 60)
  active <- reactiveVal(FALSE)
  session_start_time <- reactiveVal(0)  # Store the initial time when user starts
  session_started <- reactiveVal(FALSE) # Initialize session_started variable
  
  # Time left output
  output$timeleft <- renderText({
    paste("Time left: ", seconds_to_period(timer()))
  })
  
  # Observe start button click
  observeEvent(input$start, {
    session_start_time(timer())  # Store initial time
    active(TRUE)  # Start timer
    session_started(TRUE) #add a start variable for the fact that they started a session
  })
  
  # Observer for countdown and session tracking
  observe({
    invalidateLater(1000, session)
    isolate({
      if (active()) {
        timer(timer() - 1)
        if (timer() < 1) {
          active(FALSE)
          showModal(modalDialog(
            title = "Important message",
            "Session completed! The session tracker will update with the next login."
          ))
          
          # Get logged-in user's index
          user_index <- which(session_tracker$user == credentials()$info$user)
          
          # Calculate session duration (in seconds)
          session_duration <- session_start_time() - timer()
          
          # Update session tracker values for the user
          completed_sessions <- 1
          total_time_spent <- session_duration
          
          
          # Extract relevant session data
          user <- credentials()$info$user
          # total_time_spent <- session_tracker$total_time_spent[user_index]
          # completed_sessions <- session_tracker$completed_sessions[user_index]
          
          # Save the updated session data as a new row in the Google Sheet, including if the session was started
          saveSessionData(user, session_duration, total_time_spent, completed_sessions, session_started())
          
          # Print for debugging
          print(session_tracker)
          
          # Reset session started status
          session_started(FALSE) 
        }
      }
    })
  })
  
  # Render session count for logged-in user
  output$session_count <- renderTable({
    req(credentials()$user_auth)  # Ensure user is authenticated
    
    # Retrieve user data from session tracker
    user_data <- session_tracker %>%
      filter(user == credentials()$info$user)
    
    # Check if the user exists in the session tracker
    if (nrow(user_data) > 0) {
      # If the user exists, extract total_time_spent and completed_sessions
      total_time_spent <- sum(user_data$session_duration)
      completed_sessions <- sum(user_data$completed_sessions)
    } else {
      # If user doesn't exist, set default values
      total_time_spent <- 0
      completed_sessions <- 0
      
    }
    
    # Create a summary table to display user session data
    tibble(
      User = credentials()$info$user,
      Total_Time_Spent_Minutes = total_time_spent,  # Convert seconds to minutes
      Completed_Sessions = completed_sessions
    )
  })
  
  # Action button observers
  observeEvent(input$stop, { active(FALSE) })
  observeEvent(input$reset, { timer(input$minutes * 60) })
  
  # Plot
  output$distPlot <- renderImage({
    
    # Show plot only when authenticated
    req(credentials()$user_auth)
    
    # create a temporary file
    outfile <- tempfile(fileext = '.gif')
    
    # make the animation
    Breathing_Circle <- ggplot() +
      geom_circle(data = breathing_circle_data, mapping = aes(x0 = x0, y0 = y0, r = r, col = r, fill = r), show.legend = FALSE) +
      scale_fill_viridis(option = "D") +
      theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),legend.position="none",
            panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),panel.background = element_rect(fill = 'black', color = 'black')) +
      coord_fixed () +
      transition_states(speed_maybe,
                        transition_length = 1,
                        state_length = 0) +
      enter_fade() +
      exit_shrink() +
      ease_aes('linear')
    
    #participant_breathingduration <- credentials()$info$breathing_duration
    #print(participant_breathingduration)
    
    # save the animation
    animate(Breathing_Circle, nframes = 450, renderer = gifski_renderer('outfile.gif'), duration = 30)
    
    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)
  
}