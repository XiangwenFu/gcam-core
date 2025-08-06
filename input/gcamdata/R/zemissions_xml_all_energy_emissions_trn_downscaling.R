# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_all_energy_emissions_trn_downscaling_xml
#'
#' Construct XML data structure for \code{all_energy_emissions_trn_downscaling.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{all_energy_emissions_trn_downscaling.xml}}.
#' The corresponding file in the original data system was \code{batch_all_energy_emissions.xml.R} (emissions XML).
module_emissions_all_energy_emissions_trn_downscaling_xml <- function(command, ...) {
  input_names <- c("L201.en_pol_emissions",
                   "L201.en_ghg_emissions",
                   "L201.en_iron_and_steel_ef",
                   "L201.OutputEmissions_elec",
                   "L201.nonghg_max_reduction",
                   "L201.nonghg_steepness",
                   "L201.nonghg_max_reduction_res",
                   "L201.nonghg_steepness_res",
                   "L201.nonghg_res",
                   "L201.ghg_res",
                   "L201.ResReadInControl_nonghg_res",
                   "L201.ResReadInControl_ghg_res",
                   "L232.nonco2_prc",
                   "L232.nonco2_max_reduction",
                   "L232.nonco2_steepness",
                   "L241.nonco2_tech_coeff",
                   "L241.OutputEmissCoeff_elec",
                   "L241.nonco2_max_reduction",
                   "L241.nonco2_steepness",
                   "L252.ResMAC_fos",
                   "L252.ResMAC_fos_phaseInTime",
                   "L252.ResMAC_fos_tc_average")
  if(command == driver.DECLARE_INPUTS) {
    return(input_names)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "all_energy_emissions_trn_downscaling.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    tech.change <- tech.change.year <- emiss.coeff <- NULL  # silence package check note

    # Load required inputs
    L201.en_pol_emissions <- get_data(all_data, "L201.en_pol_emissions")
    L201.en_ghg_emissions <- get_data(all_data, "L201.en_ghg_emissions")
    L201.en_iron_and_steel_ef <- get_data(all_data, "L201.en_iron_and_steel_ef")
    L201.OutputEmissions_elec <- get_data(all_data, "L201.OutputEmissions_elec")
    L201.nonghg_max_reduction <- get_data(all_data, "L201.nonghg_max_reduction")
    L201.nonghg_steepness <- get_data(all_data, "L201.nonghg_steepness")
    L201.nonghg_max_reduction_res <- get_data(all_data, "L201.nonghg_max_reduction_res")
    L201.nonghg_steepness_res <- get_data(all_data, "L201.nonghg_steepness_res")
    L201.nonghg_res <- get_data(all_data, "L201.nonghg_res")
    L201.ghg_res <- get_data(all_data, "L201.ghg_res")
    L201.ResReadInControl_nonghg_res <- get_data(all_data, "L201.ResReadInControl_nonghg_res")
    L201.ResReadInControl_ghg_res <- get_data(all_data, "L201.ResReadInControl_ghg_res")
    L232.nonco2_prc <- get_data(all_data, "L232.nonco2_prc")
    L232.nonco2_max_reduction <- get_data(all_data, "L232.nonco2_max_reduction")
    L232.nonco2_steepness <- get_data(all_data, "L232.nonco2_steepness")
    L241.nonco2_tech_coeff <- get_data(all_data, "L241.nonco2_tech_coeff") %>% rename(emiss.coef = emiss.coeff)
    L241.OutputEmissCoeff_elec <- get_data(all_data, "L241.OutputEmissCoeff_elec")
    L241.nonco2_max_reduction <- get_data(all_data, "L241.nonco2_max_reduction")
    L241.nonco2_steepness <- get_data(all_data, "L241.nonco2_steepness")
    L252.ResMAC_fos <- get_data(all_data, "L252.ResMAC_fos")
    L252.ResMAC_fos_phaseInTime <- get_data(all_data, "L252.ResMAC_fos_phaseInTime")
    L252.ResMAC_fos_tc_average <- get_data(all_data, "L252.ResMAC_fos_tc_average")

    # Split domestic passenger transport into urban transport and other transport
    L201.en_pol_emissions <- L201.en_pol_emissions %>%
      mutate(input.emissions = ifelse(region == "USA" & supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W"), input.emissions/2, input.emissions))

    L201.en_pol_emissions <- L201.en_pol_emissions %>%
      mutate(supplysector = ifelse(region == "USA" & supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W"), paste0(supplysector, "_urban"), supplysector)) %>%
      bind_rows(
        L201.en_pol_emissions %>%
          filter(region == "USA", supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W")) %>%
          mutate(supplysector = paste0(supplysector, "_other"))
      ) %>%
      filter(!(supplysector == "trn_pass_urban" & subsector == "Domestic Aviation")) %>%
      filter(!(supplysector == "trn_pass_urban" & subsector == "HSR")) %>%
      filter(!(supplysector == "trn_pass_road_LDV_other" & subsector == "2W and 3W")) %>%
      mutate(input.emissions = ifelse(region == "USA" & supplysector == "trn_pass_other" & subsector == "Domestic Aviation", input.emissions*2, input.emissions)) %>%
      mutate(input.emissions = ifelse(region == "USA" & supplysector == "trn_pass_other" & subsector == "HSR", input.emissions*2, input.emissions)) %>%
      mutate(input.emissions = ifelse(region == "USA" & supplysector == "trn_pass_road_LDV_urban" & subsector == "2W and 3W", input.emissions*2, input.emissions))

    L201.en_ghg_emissions <- L201.en_ghg_emissions %>%
      mutate(input.emissions = ifelse(region == "USA" & supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W"), input.emissions/2, input.emissions))

    L201.en_ghg_emissions <- L201.en_ghg_emissions %>%
      mutate(supplysector = ifelse(region == "USA" & supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W"), paste0(supplysector, "_urban"), supplysector)) %>%
      bind_rows(
        L201.en_ghg_emissions %>%
          filter(region == "USA", supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W")) %>%
          mutate(supplysector = paste0(supplysector, "_other"))
      ) %>%
      filter(!(supplysector == "trn_pass_urban" & subsector == "Domestic Aviation")) %>%
      filter(!(supplysector == "trn_pass_urban" & subsector == "HSR")) %>%
      filter(!(supplysector == "trn_pass_road_LDV_other" & subsector == "2W and 3W")) %>%
      mutate(input.emissions = ifelse(region == "USA" & supplysector == "trn_pass_other" & subsector == "Domestic Aviation", input.emissions*2, input.emissions)) %>%
      mutate(input.emissions = ifelse(region == "USA" & supplysector == "trn_pass_other" & subsector == "HSR", input.emissions*2, input.emissions)) %>%
      mutate(input.emissions = ifelse(region == "USA" & supplysector == "trn_pass_road_LDV_urban" & subsector == "2W and 3W", input.emissions*2, input.emissions))

    L201.nonghg_max_reduction <- L201.nonghg_max_reduction %>%
      mutate(supplysector = ifelse(region == "USA" & supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W"), paste0(supplysector, "_urban"), supplysector)) %>%
      bind_rows(
        L201.nonghg_max_reduction %>%
          filter(region == "USA", supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W")) %>%
          mutate(supplysector = paste0(supplysector, "_other"))
      ) %>%
      filter(!(supplysector == "trn_pass_urban" & subsector == "Domestic Aviation")) %>%
      filter(!(supplysector == "trn_pass_urban" & subsector == "HSR")) %>%
      filter(!(supplysector == "trn_pass_road_LDV_other" & subsector == "2W and 3W"))

    L201.nonghg_steepness <- L201.nonghg_steepness %>%
      mutate(supplysector = ifelse(region == "USA" & supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W"), paste0(supplysector, "_urban"), supplysector)) %>%
      bind_rows(
        L201.nonghg_steepness %>%
          filter(region == "USA", supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W")) %>%
          mutate(supplysector = paste0(supplysector, "_other"))
      ) %>%
      filter(!(supplysector == "trn_pass_urban" & subsector == "Domestic Aviation")) %>%
      filter(!(supplysector == "trn_pass_urban" & subsector == "HSR")) %>%
      filter(!(supplysector == "trn_pass_road_LDV_other" & subsector == "2W and 3W"))


    # ===================================================
    # Produce outputs
    create_xml("all_energy_emissions_trn_downscaling.xml") %>%
      add_xml_data(L201.en_pol_emissions, "InputEmissions") %>%
      add_xml_data(L201.en_ghg_emissions, "InputEmissions") %>%
      add_xml_data(L201.en_iron_and_steel_ef, "OutputEmissCoeff") %>%
      add_xml_data(L201.OutputEmissions_elec, "OutputEmissions") %>%
      add_xml_data(L201.nonghg_max_reduction, "GDPCtrlMax") %>%
      add_xml_data(L201.nonghg_steepness, "GDPCtrlSteep") %>%
      add_node_equiv_xml("resource") %>%
      add_node_equiv_xml("subresource") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L201.nonghg_max_reduction_res, "GDPCtrlMaxResReserve") %>%
      add_xml_data(L201.nonghg_steepness_res, "GDPCtrlSteepRes") %>%
      add_xml_data(L201.nonghg_res, "ResEmissCoef") %>%
      add_xml_data(L201.ghg_res, "ResEmissCoef") %>%
      add_xml_data(L201.ResReadInControl_nonghg_res, "ResReadInControl") %>%
      add_xml_data(L201.ResReadInControl_ghg_res, "ResReadInControl") %>%
      add_xml_data(L232.nonco2_prc, "StbTechOutputEmissions") %>%
      add_xml_data(L232.nonco2_max_reduction, "GDPCtrlMax") %>%
      add_xml_data(L232.nonco2_steepness, "GDPCtrlSteep") %>%
      add_xml_data(L241.nonco2_tech_coeff, "InputEmissCoeff") %>%
      add_xml_data(L241.OutputEmissCoeff_elec, "OutputEmissCoeff") %>%
      add_xml_data(L241.nonco2_max_reduction, "GDPCtrlMax") %>%
      add_xml_data(L241.nonco2_steepness, "GDPCtrlSteep") %>%
      add_precursors("L201.en_pol_emissions", "L201.en_ghg_emissions",
                     "L201.en_iron_and_steel_ef", "L201.OutputEmissions_elec",
                     "L201.nonghg_max_reduction", "L201.nonghg_steepness", "L201.nonghg_max_reduction_res",
                     "L201.nonghg_steepness_res", "L201.nonghg_res", "L201.ghg_res",
                     "L201.ResReadInControl_nonghg_res", "L201.ResReadInControl_ghg_res", "L232.nonco2_prc",
                     "L232.nonco2_max_reduction", "L232.nonco2_steepness", "L241.nonco2_tech_coeff",
                     "L241.OutputEmissCoeff_elec", "L241.nonco2_max_reduction", "L241.nonco2_steepness") ->
      all_energy_emissions_trn_downscaling.xml
    # need to call add_precursors indirectly to ensure input_names gets "unlisted"
    all_energy_emissions_trn_downscaling.xml <- do.call("add_precursors", c(list(all_energy_emissions_trn_downscaling.xml), input_names))


    return_data(all_energy_emissions_trn_downscaling.xml)
  } else {
    stop("Unknown command")
  }
}
