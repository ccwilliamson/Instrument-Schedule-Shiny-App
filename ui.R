
## R shinyapp to generate editable instrument scheduling
## Cecelia Williamson 2/2026

library(ggplot2)
library(shiny)
library(colourpicker)
library(shinyjs)

## load colours
cols <- toupper(c(
 "#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#FDBF6F","#A6CEE3",
 "#56B4E9","#B2DF8A","#FB9A99","#CAB2D6","#A9C4E2","#79C360","#FDB762","#9471B4",
 "#A4A4A4","#fbb4ae","#b3cde3","#ccebc5","#decbe4","#fed9a6","#ffffcc","#e5d8bd",
  "#fddaec","#f2f2f2","#8dd3c7","#d9d9d9"))

# Define instruments with fixed colors
instruments <- list(
  list(name="JSC324 thermocycler 1",    color="#fb8072"),
  list(name="JSC324 thermocycler 2",   color="#80b1d3"),
  list(name="JSC324 shaker 1",    color="#b3de69"),
  list(name="JSC324 shaker 2", color="#fdb462"),
  list(name="Miseq", color="#bebada"),
  list(name="Nextseq", color="#fccde5")
)

## UI -----------------------------------------------------------------------

ui <- fluidPage(
  
  useShinyjs(),

#make plot sizing correct for page
  tags$head(
    tags$style(HTML("
      #out_plot img {
        width: 100% !important;
        height: auto !important;
      }
      #out_plot {
        width: 100%;
        min-height: 300px;
      }
    "))
  ),

## set up the app panels   

  pageWithSidebar(
    headerPanel(title="Endicott Laboratory Equipment Booking", windowTitle="Endicott Laboratory Equipment Booking"),
    sidebarPanel(
      helpText("Reserve instruments for the intended time period they will be in use."),
      
## time range detection for calendar

      h3("Show Calendar"),
      div(class="row",
          div(class="col-md-6",
              dateInput("in_duration_date_start","From",value=format(as.Date(Sys.time(),"%Y-%m-%d",tz="Europe/Stockholm"),"%Y-%m-%d"))
          ),
          div(class="col-md-6",
              dateInput("in_duration_date_end","To",value=format(as.Date(Sys.time(),"%Y-%m-%d",tz="Europe/Stockholm")+30,"%Y-%m-%d"))
          )
      ),
      
## Instrument buttons to select the instrument

      h3("Instruments"),
      helpText("Toggle instruments on/off. Selected instruments will appear on the calendar. Set the date range for each selected instrument."),
      

## Instrument buttons

      div(
        lapply(seq_along(instruments), function(i) {
          inst <- instruments[[i]]
          actionButton(
            inputId = paste0("btn_inst_", i),
            label = inst$name,
            style = paste0(
              "margin: 4px; background-color: #d3d3d3; color: #333;",
              "border: 2px solid ", inst$color, ";"
            )
          )
        })
      ),
      
      tags$br(), tags$br(),
      
## Dynamic date range inputs for selected instruments
      uiOutput("instrument_dates"),
      
      tags$br(),
      colourpicker::colourInput("in_track_colour_available", label="Colour (Available)",
                                palette="limited", allowedCols=cols, value=cols[length(cols)-1]),
      colourpicker::colourInput("in_track_colour_weekend", label="Colour (Weekend)",
                                palette="limited", allowedCols=cols, value=cols[length(cols)])
    ),
    mainPanel(
      tags$br(),
      imageOutput("out_plot")
    )
  )
)

## SERVER ------------------------------------------------------------------------

server <- function(input, output, session) {
  
  store <- reactiveValues(week=NULL)
  
## Track which buttons are selected

  toggled <- reactiveValues()
  for (i in seq_along(instruments)) {
    local({
      idx <- i
      toggled[[paste0("inst_", idx)]] <- FALSE
    })
  }
  
## Observe each button click and toggle state + appearance
  lapply(seq_along(instruments), function(i) {
    local({
      idx <- i
      inst <- instruments[[idx]]
      observeEvent(input[[paste0("btn_inst_", idx)]], {
        current <- toggled[[paste0("inst_", idx)]]
        toggled[[paste0("inst_", idx)]] <- !current
        
## Update button style to reflect toggled state
        
        if (!current) {
          
## Turning ON - highlight with instrument color
          shinyjs::runjs(paste0(
            "document.getElementById('btn_inst_", idx, "').style.backgroundColor = '", inst$color, "';",
            "document.getElementById('btn_inst_", idx, "').style.color = '#fff';"
          ))
        } else {
## Turning OFF - reset to grey
          shinyjs::runjs(paste0(
            "document.getElementById('btn_inst_", idx, "').style.backgroundColor = '#d3d3d3';",
            "document.getElementById('btn_inst_", idx, "').style.color = '#333';"
          ))
        }
      }, ignoreInit=TRUE)
    })
  })
  
## Render date range inputs for toggled-on instruments
  
  output$instrument_dates <- renderUI({
    active <- which(sapply(seq_along(instruments), function(i) {
      toggled[[paste0("inst_", i)]]
    }))
    
    if (length(active) == 0) return(helpText("No instruments selected."))
    
##set up the scheduling timeframe
    
    lapply(active, function(i) {
      inst <- instruments[[i]]
      avail_times <- c("8:00 AM","8:30 AM","9:00 AM","9:30 AM",
                       "10:00 AM","10:30 AM","11:00 AM","11:30 AM",
                       "12:00 PM","12:30 PM","1:00 PM","1:30 PM",
                       "2:00 PM","2:30 PM","3:00 PM","3:30 PM",
                   "4:00 PM","4:30 PM","5:00 PM")
      
#create container boxes with div
      div(
        style = paste0("border-left: 4px solid ", inst$color, "; padding-left: 8px; margin-bottom: 10px;"),
        strong(inst$name),
        
        tags$br(), #line break
        
        dateInput(paste0("inst_date_", i),
                  label = "Booking Date",
                  value = Sys.Date()),
        
#add a start time booking option and a class booking option using div 

        div(class="row",
            div(class="col-xs-6", style="padding-right: 5px;",
                selectInput(paste0("inst_start_time_", i), label="Start Time",
                            choices = avail_times,
                            selected = "9:00 AM")
            ),
            div(class="col-xs-6", style="padding-left: 5px;",
              selectInput(paste0("inst_end_time_", i), label="End Time",
                          choices = avail_times,
                          selected = "5:00 PM")
            )
        )
      )
    })
  })
  
##fn_plot------------------------------
##reactive calender builder, updates a inputs change

  fn_plot <- reactive({
    
    shiny::req(input$in_duration_date_start) #validates the start date is input
    shiny::req(input$in_duration_date_end) #validates the end date is input
    
##only allow for selection of valid future times
    if(as.Date(input$in_duration_date_start) > as.Date(input$in_duration_date_end))
      stop("End duration date must be later than start duration date.")
    
## takes input dates and builds a dataframe for the time period with rows and columns that make build the calendar visual
    dfr <- data.frame(date=seq(as.Date(input$in_duration_date_start), as.Date(input$in_duration_date_end), by=1))
    dfr$day   <- factor(strftime(dfr$date, format="%a"), levels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
    dfr$week  <- factor(strftime(dfr$date, format="%V"))
    dfr$month <- strftime(dfr$date, format="%B")
    dfr$month <- factor(dfr$month, levels=unique(dfr$month))
    dfr$ddate <- factor(strftime(dfr$date, format="%d"))
    dfr$comment <- "Available"  ##labels each day as available until the instrument booking is selected
    
## Get active instruments
    active <- which(sapply(seq_along(instruments), function(i) toggled[[paste0("inst_", i)]]))
    
    active_names <- c()
    active_cols  <- c()

## for loop to input selected date and start and end time
    for (i in active) {
      req(input[[paste0("inst_date_", i)]])
      req(input[[paste0("inst_start_time_", i)]])
      req(input[[paste0("inst_end_time_", i)]])


      t_date       <- as.Date(input[[paste0("inst_date_", i)]])
      t_start_time <- input[[paste0("inst_start_time_", i)]]
      t_end_time   <- input[[paste0("inst_end_time_",   i)]]
      
      # Combine instrument name and time range as the calendar label
      label <- paste0(instruments[[i]]$name, "\n", t_start_time, " - ", t_end_time)
      
      # Mark that specific date on the calendar with the label
      dfr$comment[dfr$date == t_date] <- label
      active_names <- c(active_names, label)
      active_cols  <- c(active_cols,  instruments[[i]]$color)
    }
    
    dfr$comment[dfr$day == "Sat" | dfr$day == "Sun"] <- "Weekend" ##labels the weekend days so they are a different color than available

## create vector to properly color the calender based on what is selected and have a correct legend order. 
## unique gets all unique values when they appear, such as available, or instrument name, to check where they are before updating the booking
    fc <- c()
    if ("Available" %in% unique(dfr$comment)) fc <- c(fc, "Available")
    fc <- c(fc, active_names)  ## allows for instrument color coding when the instrument is selected
    if ("Weekend" %in% unique(dfr$comment))   fc <- c(fc, "Weekend")
    dfr$comment <- factor(dfr$comment, levels=fc) ## converts the text into factors, allowing for ordering and levels. This allows for the color to match the correct label

#build vector for the colors for available, active instrument colors, and weekend colors. goes in same order as FC, needs to match to have correct color coding   
    all_cols <- c(input$in_track_colour_available, active_cols, input$in_track_colour_weekend)
## build calendar using ggplot  
    p <- ggplot(dfr, aes(x=week, y=day)) +
      geom_tile(aes(fill=comment)) +
      geom_text(aes(label=ddate), size=2.5) +
      scale_fill_manual(values=all_cols) +
      facet_grid(~month, scales="free", space="free") +
      labs(x="Week", y="") +
      theme_bw(base_size=10) +
      theme(legend.title    = element_blank(),
            panel.grid      = element_blank(),
            panel.border    = element_blank(),
            axis.ticks      = element_blank(),
            axis.title      = element_text(colour="grey30"),
            strip.background= element_blank(),
            strip.text      = element_text(size=8),
            legend.position = "right",
            legend.justification = "right",
            legend.direction     = "vertical",
            legend.text     = element_text(size=5),
            legend.key.size = unit(0.3,"cm"),
            legend.spacing.x= unit(0.2,"cm"))
  
##store$week, counts the number of weeks based on the timeframe selected.
## number of weeks is saved into the store$week vector, allows for passing from reactiveValues to out_plot
    store$week <- length(levels(dfr$week))
    return(p)
  })
  
## OUT: out_plot ---------------------------------------------------------------------------------------

  
##render the calender image to be displayed in the UI, re-runs when rn_plot changes to update the image based on the inputs selected
   output$out_plot <- renderImage({
    shiny::req(fn_plot())  ## check that the plot has been produced before being displayed, prevents crashing
 
##set the dimensions of the plot to be standard   
    height <- 5.5
    width  <- 20
    res    <- 150 ##resolution

## stored the ggplot fn_plot as "p", ggsave saves plot (p) to a file which is then called into the UI
##overwrites existing plot with every update so there are not new files every time
    p <- fn_plot()
    ggsave("calendar_plot.png", p, height=height, width=width, units="cm", dpi=res)
    
## returns list of settings to renderImage for shiny to display
    return(list(src         = "calendar_plot.png", ##location of image
                contentType = "image/png", ##file type
                width       = "100%",   ## fills the main panel
                height      = "auto",   ## height adjusts proportionally
                alt         = "calendar_plot"))
  })
}

shinyApp(ui = ui, server = server, options = list(height = 1080))