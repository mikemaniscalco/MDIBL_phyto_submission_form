library(shiny)
library(dplyr)

##  followed these directions for setting up google sheets API for .json gs4_auth key
load("core_data.RData")

phytoLog_UI <- fluidPage(
  shinysurveys::surveyOutput(df = df,
                             survey_title = "MDIBL   HAB monitoring",
                             survey_description = "If grids counted differ from entry field then please convert to the equivalent counts per designated # of grids")
)

phytoLog_Server <- function(input, output, session) {
  shinysurveys::renderSurvey()
  
  observeEvent(input$submit, {
    response_data <- shinysurveys::getSurveyData()
    response_data <- response_data %>%
      select(question_id, response) %>% 
      t() %>%
      janitor::row_to_names(row_number = 1) %>%
      data.frame()
    rownames(response_data) <- NULL
    
    googlesheets4::gs4_auth(path = '.secrets/enduring-coil-349821-3832e38b5b43.json')

    ss <- googlesheets4::gs4_get("https://docs.google.com/spreadsheets/d/1JGXLAvG_U9dWn7Ya_NorUU3mcgpi7aX2uanszfhXP8c/edit#gid=641716790")
    googlesheets4::sheet_append(ss, sheet = "raw",data=response_data)
    
    volume_of_rafter=0.001 
    total_rafter_grids=1000 # per slide
    grids_counted=200 # per slide
    response_data <- response_data %>%
      dplyr::mutate(across(volume_filtered:Other_zooplankton, 
                           ~as.numeric(.x))) %>% 
      dplyr::mutate(across(Alexandrium:Other_zooplankton, 
                           ~.x* ((1/grids_counted) *   #convert counts to counts per grid
                                   (total_rafter_grids/volume_of_rafter) * # counts per grid to counts per L
                                   (resusp_volume/volume_filtered)))) %>% # factor of 
      dplyr::mutate(monitor_names=paste0(last_name," ",first_name)) %>%
      dplyr::mutate(eventDate = as.POSIXct(paste(lubridate::ymd(date, tz ="America/New_York"),
                                                 as.character(gsub(".* ","",time)),
                                                 format="%Y-%m-%d %H:%M:%S",
                                                 tz ="UTC")),
                    time_high_tide = as.POSIXct(paste(lubridate::ymd(date, tz ="America/New_York"),
                                                 as.character(gsub(".* ","",time_high_tide)),
                                                 format="%Y-%m-%d %H:%M:%S",
                                                 tz ="UTC")),
                    time_low_tide = as.POSIXct(paste(lubridate::ymd(date, tz ="America/New_York"),
                                                     as.character(gsub(".* ","",time_low_tide)),
                                                     format="%Y-%m-%d %H:%M:%S",
                                                     tz ="UTC"))) %>%
      dplyr::left_join(., site_info, by="locationID") %>%
      dplyr::mutate(tide_stage=stringr::word(tide_stage, 1),
                    Other_diatoms=Other_diatoms*4,
                    Other_dinoflagellates=Other_dinoflagellates*4,
                    Other_phytoplankton=Other_phytoplankton*4,
                    transparency_depth_mean=(as.numeric(ascending_transparency)+as.numeric(descending_transparency))/2,
                    decimalLatitude= ifelse(locationID=="Other",decimalLatitude_other,decimalLatitude),
                    decimalLongitude= ifelse(locationID=="Other",decimalLongitude_other,decimalLongitude)) %>%
      dplyr::mutate(Total_phytoplankton=sum(c_across((Alexandrium:Other_phytoplankton)))) %>%
      dplyr::mutate(Total_zooplankton=sum(c_across((Copepods:Other_zooplankton)))) %>%
      tibble::add_column(countryCode="US") %>%
      dplyr::mutate(eventID= paste(locationID, 
                                   strftime(eventDate , "%Y-%m-%dT%H:%M:%S%Z", tz = "Zulu"), 
                                   sep="_")) %>% 
      dplyr::mutate(id= paste("MDIBL-habs", locationID, 
                              strftime(eventDate , "%Y-%m-%dT%H:%M:%S%Z",tz = "Zulu"), 
                              sep="_")) %>% 
      tibble::add_column(geodeticDatum="EPSG:4326 WGS84") %>%
      dplyr::select(id, eventID, eventDate, locationID, decimalLatitude,
                    decimalLongitude, coordinateUncertaintyInMeters,	geodeticDatum, countryCode, 
                    monitor_names, weather, sampling_method,
                    water_temp, air_temp,	tide_stage, time_high_tide, time_low_tide,
                    rainfall_mm, wind_description,	wind_direction,	wind_speed_knots,	water_surface,
                    water_current,
                    ascending_transparency, descending_transparency,	transparency_depth_mean,
                    bottom_depth,
                    salinity_ppt, pH, DOavg_ppm,	nutrient_vial_id,	
                    Alexandrium, Scrippsiella, Gonyaulax, Pseudo_nitzschia_small,
                    Pseudo_nitzschia_large,
                    Dinophysis_acuminata, Dinophysis_norvegica,
                    Dinophysis_spp,	Prorocentrum_lima,	Prorocentrum_spp,	Karenia,
                    Margalefidinium_polykrikoides,	Other_diatoms, Other_dinoflagellates,	
                    Other_phytoplankton, Total_phytoplankton,
                    Copepods,	Other_zooplankton,	Total_zooplankton, Comments) 
    
    googlesheets4::sheet_append(ss, sheet = "processed", data=response_data)
    
    ### Occurrence
    ##check and maybe remove extra columns
    
    response_data <- response_data %>%
      select(-c(Copepods, Other_zooplankton, Total_zooplankton)) %>%
      rowwise() %>%
      tidyr::pivot_longer(., cols=(Alexandrium:Total_phytoplankton),
                          names_to = "organismName",
                          values_to = "organismQuantity",) %>%
      dplyr::left_join(.,tax_table, by="organismName") %>%
      tibble::add_column(basisOfRecord="HumanObservation", .after="id") %>%
      tibble::add_column(organismQuantityType= "Cells per L", .after="organismQuantity") %>% 
      dplyr::mutate(organismQuantity=ifelse(is.na(organismQuantity)==T, 0, organismQuantity)) %>%
      dplyr::mutate(occurrenceID=paste(strftime(eventDate, "%Y-%m-%dT%H:%M:%S%Z", tz = "Zulu"), organismName, sep="_")) %>%
      dplyr::mutate(occurrenceStatus= ifelse(organismQuantity>0, "present", "absent")) %>%
      dplyr::select(id, basisOfRecord, organismName, organismQuantity, organismQuantityType,
                    occurrenceID, occurrenceStatus, scientificName, scientificNameID, taxonID, kingdom) %>%
      dplyr::filter_at(vars(occurrenceStatus), all_vars(!is.na(.))) %>%
      tibble::add_column(data_historic = "False")
    
    googlesheets4::sheet_append(ss,  sheet = "occurrence_temp", data=response_data)
    
    
    showModal(modalDialog(
      title = "Your submission has been saved."
    )
    )
  })
}


shinyApp(ui = phytoLog_UI, server = phytoLog_Server)  
