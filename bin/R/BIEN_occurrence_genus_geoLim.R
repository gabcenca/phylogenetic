#'Extract occurrence data from BIEN for specified genera
#'
#'BIEN_occurrence_genus downloads occurrence records for specific genus/genera from the BIEN database.
#' @param genus A single genus, or a vector of genera. Genera should be capitalized.
#' @template occurrence
#' @return Dataframe containing occurrence records for the specified genera.
#' @examples \dontrun{
#' BIEN_occurrence_genus("Abutilon")
#' genus_vector<-c("Abutilon","Abronia")
#' BIEN_occurrence_genus(genus_vector)
#' BIEN_occurrence_genus(genus = "Abutilon",cultivated = TRUE,new.world = FALSE)}
#' @family occurrence functions
#' @export
BIEN_occurrence_genus <- function(genus,
                                  cultivated = FALSE,
                                  new.world = NULL,
                                  all.taxonomy = FALSE,
                                  native.status = FALSE,
                                  natives.only = TRUE,
                                  observation.type = FALSE,
                                  political.boundaries = FALSE,
                                  collection.info = FALSE,
                                  ...){
  .is_char(genus)
  .is_log(cultivated)
  .is_log(all.taxonomy)
  .is_log_or_null(new.world)
  .is_log(native.status)
  .is_log(observation.type)
  .is_log(political.boundaries)
  .is_log(natives.only)
  .is_log(collection.info)
  
  cultivated_<-.cultivated_check(cultivated)  
  newworld_<-.newworld_check(new.world)
  taxonomy_<-.taxonomy_check(all.taxonomy)  
  native_<-.native_check(native.status)
  observation_<-.observation_check(observation.type)
  political_<-.political_check(political.boundaries)  
  natives_<-.natives_check(natives.only)
  collection_<-.collection_check(collection.info)
  
  # set the query
  query <-
    paste("SELECT scrubbed_genus, scrubbed_species_binomial",taxonomy_$select,native_$select,political_$select," ,latitude, longitude,date_collected,datasource,dataset,dataowner,custodial_institution_codes,collection_code,view_full_occurrence_individual.datasource_id",collection_$select,cultivated_$select,newworld_$select,observation_$select, "
          FROM view_full_occurrence_individual 
          WHERE scrubbed_genus in (", paste(shQuote(genus, type = "sh"),collapse = ', '), ")",cultivated_$query,newworld_$query,natives_$query," 
            AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi') AND is_geovalid = 1 
            AND (georef_protocol is NULL OR georef_protocol<>'county centroid') AND (is_centroid IS NULL OR is_centroid=0) 
            AND observation_type IN ('plot','specimen','literature','checklist') 
            AND scrubbed_species_binomial IS NOT NULL ;")
  
  return(.BIEN_sql(query, ...))
  
}