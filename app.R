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
      dplyr::mutate(wind_direction=stringr::str_replace(wind_direction, "None","999"),
             wind_direction=stringr::str_replace(wind_direction, "Northwest", "315"),
             wind_direction=stringr::str_replace(wind_direction, "Northeast", "35"),
             wind_direction=stringr::str_replace(wind_direction, "Southeast", "135"),
             wind_direction=stringr::str_replace(wind_direction, "Southwest", "225"),
             wind_direction=stringr::str_replace(wind_direction, "West", "270"),
             wind_direction=stringr::str_replace(wind_direction, "East", "90"),
             wind_direction=stringr::str_replace(wind_direction, "North", "0"),
             wind_direction=stringr::str_replace(wind_direction, "South", "90"),
             wind_direction=as.numeric(wind_direction)) %>%
      tibble::add_column(minimumDepthInMeters=NA) %>%
      dplyr::mutate(maximumDepthInMeters= ifelse(maximumDepthInMeters=="HIDDEN-QUESTION",
                                                 NA,maximumDepthInMeters),
                    minimumDepthInMeters=ifelse(sampling_method=="Tow",
                                                0,
                                                NA)) %>%
      tibble::add_column(geodeticDatum="EPSG:4326 WGS84") %>%
      tibble::add_column(basisOfRecord="HumanObservation") %>%
      tibble::add_column(organismQuantityType="Cells per L") %>%
      dplyr::select(id, eventID, eventDate, eventRemarks=Comments, 
                    locationID, decimalLatitude,
                    decimalLongitude, coordinateUncertaintyInMeters,	
                    geodeticDatum, countryCode, 
                    monitor_names, weather, sampling_method, minimumDepthInMeters,maximumDepthInMeters,
                    water_temp, air_temp,	tide_stage, time_high_tide, time_low_tide,
                    rainfall_mm, wind_description,	wind_direction,	wind_speed_knots,	water_surface,
                    water_current,
                    ascending_transparency, descending_transparency,	transparency_depth_mean,
                    bottom_depth,
                    salinity_ppt, pH, DOavg_ppm,	nutrient_vial_id,	basisOfRecord, organismQuantityType,
                    Alexandrium, Scrippsiella, Gonyaulax, Pseudo_nitzschia_small,
                    Pseudo_nitzschia_large,
                    Dinophysis_acuminata, Dinophysis_norvegica,
                    Dinophysis_spp,	Prorocentrum_lima,	Prorocentrum_spp,	Karenia,
                    Margalefidinium_polykrikoides,	Other_diatoms, Other_dinoflagellates,	
                    Other_phytoplankton, Total_phytoplankton,
                    Copepods,	Other_zooplankton,	Total_zooplankton) 
    googlesheets4::sheet_append(ss, sheet = "processed", data=response_data)
    
    
    ### ammend temp long files for shiny visualization
    df_phyto <- response_data %>%
      dplyr::rowwise() %>%
      tidyr::pivot_longer(., cols=(Alexandrium:Total_phytoplankton),
                          names_to = "organismName",
                          values_to = "organismQuantity") %>%
      dplyr::left_join(., tax_table, by="organismName") %>%
      dplyr::filter(organismQuantityType=="Cells per L") %>%
      dplyr::mutate(day=strftime(eventDate, "%Y-%m-%d", tz = "America/New_York")) %>%
      dplyr::filter_at(vars(organismQuantity), all_vars(!is.na(.))) %>%
      dplyr::select(day, locationID,  organismName, organismQuantity)
    googlesheets4::sheet_append(ss, sheet = "occurrence_temp", data=df_phyto)
    
    
    phyto_extended <- response_data %>%
      dplyr::relocate(.,c(salinity_ppt,DOavg_ppm,transparency_depth_mean), .after="water_temp") %>%
      dplyr::select(-eventRemarks, -nutrient_vial_id, 
                    -c(Alexandrium:Total_phytoplankton)) %>% 
      tidyr::pivot_longer(., 
                          cols=(water_temp:transparency_depth_mean),
                          names_to = "measurementType",
                          values_to = "measurementValue",
                          values_transform = list(measurementValue = as.character)) %>%
      dplyr::filter(!is.na(measurementValue)) %>%
      dplyr::mutate(day=strftime(eventDate, "%Y-%m-%d", tz = "America/New_York"), 
                    measurementValue=as.numeric(measurementValue)) %>%
      dplyr::select(.,day, locationID,	measurementType, 
                    measurementValue) %>%
      dplyr::arrange(day)
    googlesheets4::sheet_append(ss, sheet = "extended_temp", data=phyto_extended)
    
    
    showModal(modalDialog(
      title = "Your submission has been saved."
    )
    )
  })
}


shinyApp(ui = phytoLog_UI, server = phytoLog_Server)  
