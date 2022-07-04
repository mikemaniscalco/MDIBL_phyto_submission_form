library(shiny)
library(shinysurveys)
library(tidyverse)
library(janitor)
library(googlesheets4)
library(lubridate)
df<-read_csv("demo_survey.csv")
# df<- shinysurveys::teaching_r_questions

ui <- fluidPage(
  surveyOutput(df = df,
               survey_title = "Hello, World!",
               survey_description = "Welcome! This is a demo survey showing off the {shinysurveys} package.")

  )

server <- function(input, output, session) {
  renderSurvey()
  
  observeEvent(input$submit, {
    # response_data <- getSurveyData()
    # sheet_append(response_data, sheet="https://docs.google.com/spreadsheets/d/1JGXLAvG_U9dWn7Ya_NorUU3mcgpi7aX2uanszfhXP8c/edit#gid=0")
    response_data <- getSurveyData()
    response_data <- response_data %>%
      select(question_id, response) %>% 
      t() %>%
      row_to_names(row_number = 1) %>%
      data.frame()
    rownames(response_data) <- NULL
    
    print(response_data)
    ss <- gs4_get("https://docs.google.com/spreadsheets/d/1JGXLAvG_U9dWn7Ya_NorUU3mcgpi7aX2uanszfhXP8c/edit#gid=0")
    sheet_append(ss, data=response_data)
    
    volume_of_rafter=0.001 
    total_rafter_grids=100 # per slide
    
    response_data <- response_data %>%
      mutate(across(volume_filtered:Other_zoos, 
                    ~as.numeric(.x))) %>% 
      mutate(across(Alexandrium:Other_zoos, 
                    ~.x* ((1/grids_counted) *   #convert counts to counts per grid
                            (total_rafter_grids/volume_of_rafter) * # counts per grid to counts per L
                            (resusp_volume/volume_filtered)))) %>% # factor of 
      mutate(monitor_names=paste0(last_name," ",first_name)) %>%
      
      mutate(eventDate = as.POSIXct(paste(ymd(date,tz ="America/New_York"),
                                          as.character(gsub(".* ","",time)),
                                  format="%Y-%m-%d %H:%M:%S",
                                  tz ="America/New_York"))) %>%
      mutate(eventDate=strftime(eventDate , "%Y-%m-%dT%H:%M:%S%Z",tz = "Zulu")) %>%
      select(monitor_names,	eventDate,	rain_48h,	weather,	method,
             water_temp,	air_temp,	trans_asc,	trans_desc,	bottom_depth,
             salinity,	ph,	DO,	nutrient_vial_id,	
             Alexandrium, PN_small,	PN_large,	Dinophysis_acuminata,	Dinophsis_norvegica,
             Dinophsis_spp,	Prorocentrum_lima,	Prorocentrum_spp,	Karenia,
             Margalefidinium_polykrikoides,	Other_diatoms,	Other_dinos,	Other_phytoplankton,
             Copepods,	Other_zoos,	Comments)
    
    
    sheet_append(ss ="https://docs.google.com/spreadsheets/d/1JGXLAvG_U9dWn7Ya_NorUU3mcgpi7aX2uanszfhXP8c/edit#gid=0",
                 sheet = "processed", data=response_data)
    
    
    showModal(modalDialog(
      title = "Your submission has been saved."
    ))
  })
}

shinyApp(ui, server)
