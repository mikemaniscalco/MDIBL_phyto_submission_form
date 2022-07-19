



## Finish lightly processed data code for shiny app
## Code to convert lightly processed data into long form and join with old data-->
## currently use google drive links, but can probably change to links to EDI csv files
## Use that joined long data for visualization?
## maybe try and make a few graphs with long data first/now



### Occurrence
tax_table <- read_csv("source_data/tax_table.csv")
##check and maybe remove extra columns

response_data <- response_data %>%
  rowwise() %>%
  pivot_longer(., cols=(Alexandrium:Total_phytoplankton),
               names_to = "organismName",
               values_to = "organismQuantity",) %>%
  left_join(.,tax_table, by="organismName") %>%
  add_column(basisOfRecord="HumanObservation", .after="id") %>%
  add_column(organismQuantityType= "Cells per L", .after="organismQuantity") %>%
  mutate(organismQuantity=ifelse(is.na(organismQuantity)==T, 0, organismQuantity)) %>%
  mutate(occurrenceID=paste(eventDate, organismName, sep="_")) %>%
  mutate(occurrenceStatus= ifelse(organismQuantity>0, "present", "absent")) %>%
  select(id, basisOfRecord,organismName, organismQuantity, organismQuantityType,
         occurrenceID, occurrenceStatus, scientificName, scientificNameID, taxonID, kingdom) %>%
  filter_at(vars(occurrenceStatus), all_vars(!is.na(.)))

phyto_occurrence <- bind_rows(phyto_occurrence, new_occurrence)

write_csv(phyto_occurrence,"2022_mdibl_data_package/data_objects/occurrence.csv")
rm(tax_table, phyto_occurrence, tow_occurrence, phyto_occurrence_b)


### Create event file

new_events <- response_data %>%
  select(id, eventID,	eventDate, geodeticDatum,	countryCode, locationID,
         eventRemarks,	decimalLatitude,	decimalLongitude,	minimumDepthInMeters,
         maximumDepthInMeters,	coordinateUncertaintyInMeters) %>%
  arrange(eventDate)

phyto_events <- bind_rows(phyto_events, new_events)
write_csv(phyto_events,"2022_mdibl_data_package/data_objects/event.csv")
rm(phyto_events)


### Create ExtendedMeasurementOrFact table

extended_base <- read_csv("source_data/columns_units.csv") %>%
  filter(darwin_file=="all"|darwin_file=="ExtendedMeasurementOrFact")

phyto_extended <- df_full %>%
  select(-monitor_names, -c(Alexandrium:Other_phytoplankton)) %>%
  mutate(across(air_temp:transparency_depth_mean, ~as.character(.x))) %>%
  pivot_longer(.,
               cols=(air_temp:transparency_depth_mean),
               names_to = "measurementType",
               values_to = "measurementValue") %>%
  mutate(measurementValue=ifelse(measurementType=="rainfall_mm" & measurementValue=="0",
                                 NA,
                                 measurementValue)) %>%
  filter(!is.na(measurementValue)) %>%
  left_join(., extended_base, by= "measurementType") %>%
  select(.,id, measurementType,	measurementValue,	measurementID,
         measurementTypeID,	measurementUnit,
         measurementUnitID # is this one optional?
  ) %>%
  arrange(id)

write_csv(phyto_extended,"2022_mdibl_data_package/data_objects/ExtendedMeasurementOrFact.csv")

rm(phyto_extended, extended_base)


