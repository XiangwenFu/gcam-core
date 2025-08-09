# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_macro_xml_trn_downscaling
#'
#' Construct XML data structure for \code{socioeconomics_macro_trn_downscaling.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{socioeconomics_macro_trn_downscaling.xml}. There is no corresponding file in the
#' original data system.
module_socioeconomics_macro_xml_trn_downscaling <- function(command, ...) {

  MODULE_INPUTS <-
    c("L280.nationalAccounts",
      "L280.SavingsRateParams",
      "L280.GDP_macro_function",
      "L280.FactorProductivity",
      # tracking inputs
      "L281.BasePriceSectorMapping",
      "L281.GlobalTechAccountOutputUseBasePrice_fd",
      "L281.TrialValueResource")

  MODULE_OUTPUTS <-
    c(XML = "socioeconomics_macro_trn_downscaling.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    #Split domestic passenger transport into urban transport and other transport
    L281.BasePriceSectorMapping <- L281.BasePriceSectorMapping %>%
      bind_rows(
        L281.BasePriceSectorMapping %>%
          filter(from.sector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W")) %>%
          mutate(from.sector = paste0(from.sector, "_other"),
                 to.sector = paste0(to.sector, "_other"))
      )

    L281.GlobalTechAccountOutputUseBasePrice_fd <- L281.GlobalTechAccountOutputUseBasePrice_fd %>%
      bind_rows(
        L281.GlobalTechAccountOutputUseBasePrice_fd %>%
          filter(sector.name %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W")) %>%
          mutate(sector.name = paste0(sector.name, "_other"))
      ) %>%
      filter(!(sector.name == "trn_pass" & subsector.name == "Domestic Aviation")) %>%
      filter(!(sector.name == "trn_pass" & subsector.name == "HSR")) %>%
      filter(!(sector.name == "trn_pass_other" & subsector.name == "Cycle")) %>%
      filter(!(sector.name == "trn_pass_other" & subsector.name == "Walk")) %>%
      filter(!(sector.name == "trn_pass_road_LDV_other" & subsector.name == "2W and 3W"))

    # ===================================================

    # Produce outputs
    create_xml("socioeconomics_macro_trn_downscaling.xml") %>%
      add_xml_data(L280.nationalAccounts, "NationalAccount") %>%
      add_xml_data(L280.SavingsRateParams, "SavingsRateParams") %>%
      add_xml_data(L280.GDP_macro_function, "GDPMacroFunction") %>%
      add_xml_data(L280.FactorProductivity, "FactorProductivity") %>%
      add_xml_data(L281.BasePriceSectorMapping, "BasePriceSectorMap", NULL) %>%
      add_xml_data(L281.GlobalTechAccountOutputUseBasePrice_fd, "GlobalTechAccountOutputUseBasePrice") %>%
      add_xml_data(L281.TrialValueResource, "TrialValueRsrc") %>%
      add_precursors(MODULE_INPUTS) ->
      socioeconomics_macro_trn_downscaling.xml

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
