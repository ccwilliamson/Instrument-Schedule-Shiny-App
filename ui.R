
## R shinyapp to generate editable instrument scheduling
## Cecelia WIlliamson 2/2026
library(ggplot2)
library(shiny)
library(colourpicker)

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


## UI 

ui <- fluidPage(

## set up the app panels
  
  pageWithSidebar(
    headerPanel(title="Endicott Laboratory Equipment Booking", windowTitle="Endicott Laboratory Equipment Booking"),
    sidebarPanel(
      helpText("Reserve instruments for the intended time period they will be in use."),
      
## time range delection for calendar

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

## SERVER 

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
      div(
        style = paste0("border-left: 4px solid ", inst$color, "; padding-left: 8px; margin-bottom: 10px;"),
        strong(inst$name),
        div(class="row",
            div(class="col-xs-6", style="padding-right: 5px;",
                dateInput(paste0("inst_start_", i), label="From",
                          value=format(as.Date(Sys.time(), "%Y-%m-%d", tz="Europe/Stockholm"), "%Y-%m-%d"))
            ),
            div(class="col-xs-6", style="padding-left: 5px;",
                dateInput(paste0("inst_end_", i), label="To",
                          value=format(as.Date(Sys.time(), "%Y-%m-%d", tz="Europe/Stockholm")+7, "%Y-%m-%d"))
            )
        )
      )
    })
  })
  
##fn_plot
  fn_plot <- reactive({
    
    shiny::req(input$in_duration_date_start)
    shiny::req(input$in_duration_date_end)
    
##only allow for seeltion of valid future times
    if(as.Date(input$in_duration_date_start) > as.Date(input$in_duration_date_end))
      stop("End duration date must be later than start duration date.")
    
## Prepare dates
    dfr <- data.frame(date=seq(as.Date(input$in_duration_date_start), as.Date(input$in_duration_date_end), by=1))
    dfr$day   <- factor(strftime(dfr$date, format="%a"), levels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
    dfr$week  <- factor(strftime(dfr$date, format="%V"))
    dfr$month <- strftime(dfr$date, format="%B")
    dfr$month <- factor(dfr$month, levels=unique(dfr$month))
    dfr$ddate <- factor(strftime(dfr$date, format="%d"))
    dfr$comment <- "Available"
    
## Get active instruments
    active <- which(sapply(seq_along(instruments), function(i) toggled[[paste0("inst_", i)]]))
    
    active_names <- c()
    active_cols  <- c()
    
    for (i in active) {
      inst <- instruments[[i]]
      start_id <- paste0("inst_start_", i)
      end_id   <- paste0("inst_end_", i)
      
      shiny::req(input[[start_id]])
      shiny::req(input[[end_id]])
      
      t_start <- as.Date(as.character(input[[start_id]]))
      t_end   <- as.Date(as.character(input[[end_id]]))
      
      if (!is.null(t_start) && !is.null(t_end)) {
        if (t_start > t_end) stop(paste(inst$name, "end date must be later than start date."))
      }
      
      dfr$comment[dfr$date >= t_start & dfr$date <= t_end] <- inst$name
      active_names <- c(active_names, inst$name)
      active_cols  <- c(active_cols,  inst$color)
    }
    
    dfr$comment[dfr$day == "Sat" | dfr$day == "Sun"] <- "Weekend"
    
    fc <- c()
    if ("Available" %in% unique(dfr$comment)) fc <- c(fc, "Available")
    fc <- c(fc, active_names)
    if ("Weekend" %in% unique(dfr$comment))   fc <- c(fc, "Weekend")
    dfr$comment <- factor(dfr$comment, levels=fc)
    
    all_cols <- c(input$in_track_colour_available, active_cols, input$in_track_colour_weekend)
    
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
    
    store$week <- length(levels(dfr$week))
    return(p)
  })
  
  ## OUT: out_plot ------------------------------------------------------------
  output$out_plot <- renderImage({
    shiny::req(fn_plot())
    
    height <- 5.5
    width  <- (store$week * 1) + 1
    res    <- 200
    
    p <- fn_plot()
    ggsave("calendar_plot.png", p, height=height, width=width, units="cm", dpi=res, type="cairo")
    
    return(list(src="calendar_plot.png",
                contentType="image/png",
                width  = round(((width*res)/2.54)*1, 0),
                height = round(((height*res)/2.54)*1, 0),
                alt    = "calendar_plot"))
  })
}

shinyApp(ui = ui, server = server, options = list(height = 1080))