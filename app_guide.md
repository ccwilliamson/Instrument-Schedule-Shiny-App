# Quick Reference

shiny app for Endicott laboratory instrument scheduling for students and faculty. 
App will have an instrument selection panel with buttons for each instrument.
It will also have a day & time filler with a reactive visual calendar showing when instruments are schedules and by who. 
color coding will also be used for different instruments.
Instrument options:
JSC324 thermocycler 1
JSC324 thermocycler 2
JSC324 shaker 1
JSC324 shaker 2
Miseq
Nextseq

[Shiny Gallery for Quick Reference](https://shiny.posit.co/r/gallery/)

### Layout description

![My Layout](Shiny layout.png)

### Inputs
The bullets below take the general form:

> Shiny Component  |  **variable_name** | optional: args

* Action Button | **btn_inst** | each instrument has a button that toggles on and off
* Date input | **in_duration_date_start** | Select start date on calender display
* Date input | **in_duration_date_end** | Select end date on calender display
* Date Input | **inst_date_i** | booking date for selected instrument
* Select drop down input | **inst_start_time_i** | drop down menu for start time
* Select drop down input | **inst_end_time_i** | drop down menu for end time
* Calender colors | **in_track_color_available** | color of available days
*  Calender colors | **in_track_color_weekend** | color of weekend days
### Outputs
The bullets below take the general form:

> Shiny Component  |  **variable_name**  | (inputs required)  | optional: function used

* Image | out_plot | btn_inst, in_duration_date_start, in_duration_date_end, inst_date_i,inst_start_time_i, inst_end_time_i, in_track_color_available, in_track_color_weekend
* UI output | instrument_dates | btn_inst | allows for date and time to render from buttons
### Reactive components and Server

> component type | **variable_name(s)** | Events that trigger 

* reactiveValues | toggle | triggered by button click, T/F for on off state for each button
* reactiveValues | store | triggered when fn_plot runs and stores the number of weeks that has been selected
* observeEvent | btn_inst | triggered by instrument button click, updates button color if on
* renderUI | instrument_dates | triggers with instrument selection so that the date and time fields come up for the selected instruments
* reactive | fn_plot | triggers when the inputs change and reformats the calendar accordingly 
* renderImage | out_plot | triggers when the plot is changed and updates and re-saves the ggplot image

### Functions and Set up

> **function_name**  |  (inputs)  | purpose

* lapply | (instruments) | loops through the instruments to build the UI toggle buttons
* paste0 | (strings) | strings text together and removes spaces to build dynamic inputs
* ggsave | (plot) | saves ggplot calendar as a png

