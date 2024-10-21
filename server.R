library(shiny)
library(tidyverse)
library(gganimate)
library(ggforce)
library(gifski)
library(lubridate)
library(shinyauthr)
library(rsconnect)

# dataframe that holds usernames, passwords, and other user data
user_base <- tibble::tibble(
  user = c("user1", "user2"),
  password = sapply(c("pass1", "pass2"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two"),
  breathing_duration = c(10, 15)
)

# Save session data function
saveSessionData <- function(data) {
  write.csv(data, file = "session_data.csv", row.names = FALSE)
}

# Load existing session data from CSV if the file exists
if (file.exists("session_data.csv")) {
  session_tracker <- read.csv("session_data.csv", stringsAsFactors = FALSE)
  session_tracker <- as_tibble(session_tracker)  # Ensure it's a tibble for consistency
} else {
  # Initialize session tracker if no file exists
  session_tracker <- tibble::tibble(
    user = user_base$user,
    completed_sessions = rep(0, length(user_base$user)),
    total_time_spent = rep(0, length(user_base$user))
  )
}

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
  default_breathingtime <- 5
  
  # UI for logged-in users
  output$sidebarpanel <- renderUI({
    req(credentials()$user_auth)
    column(width = 4, p(paste("You have", credentials()$info[["permissions"]], "permission")))
    
    navbarPage('NTNU',
               tabPanel('Coherence Breathing',
                        sidebarLayout(
                          sidebarPanel(paste("Hello", credentials()$info$name, "Welcome to the guided breathing! The pace of this breathing is set to what was measured in the lab. Remember to inhale as the circle expands and exhale as it contracts."),
                          ),
                          mainPanel(plotOutput("distPlot"),
                                    hr(),
                                    actionButton('start', 'Start'),
                                    actionButton('stop', 'Stop'),
                                    actionButton('reset', 'Reset'),
                                    numericInput('minutes', 'Minutes:', value = default_breathingtime, min = 0, max = 99999, step = 1),
                                    textOutput('timeleft'),
                                    paste("If you would like to change the duration, remember to press reset after entering a different number."))
                        )
               ),
               tabPanel('Session Completion',
                        # Display session count for logged-in user
                        tableOutput("session_count"),
                        paste("This table displays the number of completed sessions and the total amount of time you have spend on the sessions from the stress management tool."))
    )
  })
  
  # Create Timer
  timer <- reactiveVal(default_breathingtime * 60)
  active <- reactiveVal(FALSE)
  session_start_time <- reactiveVal(0)  # Store the initial time when user starts
  
  # Time left output
  output$timeleft <- renderText({
    paste("Time left: ", seconds_to_period(timer()))
  })
  
  # Observe start button click
  observeEvent(input$start, {
    session_start_time(timer())  # Store initial time
    active(TRUE)  # Start timer
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
            "Countdown completed!"
          ))
          
          # Get logged-in user's index
          user_index <- which(session_tracker$user == credentials()$info$user)
          
          # Calculate session duration
          session_duration <- session_start_time() - timer()
          
          # Update session tracker
          session_tracker$completed_sessions[user_index] <- session_tracker$completed_sessions[user_index] + 1
          session_tracker$total_time_spent[user_index] <- (session_tracker$total_time_spent[user_index] + session_duration) / 60
          
          # Save updated session data
          saveSessionData(session_tracker)
          
          # print for debugging
          print(session_tracker)
        }
      }
    })
  })
  
  # Render session count for logged-in user
  output$session_count <- renderTable({
    req(credentials()$user_auth)
    
    # Filter session tracker for logged-in user
    session_tracker %>%
      filter(user == credentials()$info$user) %>%
      mutate(total_time_spent = total_time_spent / 60)  # Convert seconds to period
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