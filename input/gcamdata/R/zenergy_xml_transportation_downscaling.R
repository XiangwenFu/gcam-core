# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_transportation_downscaling_xml
#'
#' Construct XML data structure for \code{transportation_UCD_*.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{transportation_UCD_*.xml}. The corresponding file in the
#' original data system was \code{batch_transportation_UCD_CORE.xml} (energy XML).

module_energy_transportation_downscaling_xml <- function(command, ...) {
  # The below variable (trn_SPP) controls which scenario to run, as only one scenario can be run at a time.
  # This is a special case, and the way this is executed will likely change in the future.

  if(command == driver.DECLARE_INPUTS) {
    return(c("L254.Supplysector_trn",
             "L254.FinalEnergyKeyword_trn",
             "L254.tranSubsectorLogit",
             "L254.tranSubsectorShrwtFllt",
             "L254.tranSubsectorInterp",
             "L254.tranSubsectorSpeed",
             "L254.tranSubsectorSpeed_passthru",
             "L254.tranSubsectorSpeed_noVOTT",
             "L254.tranSubsectorSpeed_nonmotor",
             "L254.tranSubsectorVOTT",
             "L254.tranSubsectorFuelPref",
             "L254.StubTranTech",
             "L254.StubTech_passthru",
             "L254.StubTech_nonmotor",
             "L254.GlobalTechShrwt_passthru",
             "L254.GlobalTechShrwt_nonmotor",
             "L254.GlobalTechCoef_passthru",
             "L254.GlobalRenewTech_nonmotor",
             "L254.GlobalTranTechInterp",
             "L254.GlobalTranTechShrwt",
             "L254.GlobalTranTechSCurve",
             "L254.StubTranTechCalInput",
             "L254.StubTranTechLoadFactor",
             "L254.StubTranTechCost",
             "L254.StubTechTrackCapital",
             "L254.StubTranTechCoef",
             "L254.StubTechCalInput_passthru",
             "L254.StubTechProd_nonmotor",
             "L254.PerCapitaBased_trn",
             "L254.PriceElasticity_trn",
             "L254.IncomeElasticity_trn",
             "L254.BaseService_trn"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "transport_downscaling.xml"))
  } else if(command == driver.MAKE) {

    # all_data <- list(...)[[1]]

    # # Load required inputs
    # L254.StubTranTechCost <- get_data(all_data, "L254.StubTranTechCost")
    # L254.StubTranTechCoef <- get_data(all_data, "L254.StubTranTechCoef")
    # L254.StubTranTechLoadFactor <- get_data(all_data, "L254.StubTranTechLoadFactor")
    # L254.StubTranTechCalInput <- get_data(all_data, "L254.StubTranTechCalInput")
    #
    # L254.StubTranTechCost %>%
    #   filter(region == "USA", supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W"), year == 2025, sce=="CORE") %>%
    #   distinct(region, supplysector, tranSubsector, stub.technology, input.cost) ->
    #   cost_template
    #
    # L254.StubTranTechCoef %>%
    #   filter(region == "USA", supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W"), year == 2025, sce=="CORE") %>%
    #   distinct(region, supplysector, tranSubsector, stub.technology, minicam.energy.input, coefficient) ->
    #   intensity_template
    #
    # L254.StubTranTechLoadFactor %>%
    #   filter(region == "USA", supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W"), year == 2025, sce=="CORE") %>%
    #   distinct(region, supplysector, tranSubsector, stub.technology, loadFactor) ->
    #   loadfactor_template
    #
    # L254.StubTranTechCalInput %>%
    #   filter(region == "USA", supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W"), year == 2021, sce=="CORE") %>%
    #   distinct(region, supplysector, tranSubsector, stub.technology, minicam.energy.input, calibrated.value) ->
    #   cal_energy_template
    #
    # write_csv(cost_template, "cost_template.csv")
    # write_csv(intensity_template, "intensity_template.csv")
    # write_csv(loadfactor_template, "loadfactor_template.csv")
    # write_csv(cal_energy_template, "cal_energy_template.csv")

    ## silence package check.
    sce <- year <- . <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    L254.tranSubsectorSpeed <- get_data(all_data, "L254.tranSubsectorSpeed")
    L254.StubTranTech <- get_data(all_data, "L254.StubTranTech")
    L254.StubTranTechLoadFactor <- get_data(all_data, "L254.StubTranTechLoadFactor")
    L254.StubTranTechCost <- get_data(all_data, "L254.StubTranTechCost")
    L254.StubTechTrackCapital <- get_data(all_data, "L254.StubTechTrackCapital")

    L254.Supplysector_trn <- get_data(all_data, "L254.Supplysector_trn")
    L254.FinalEnergyKeyword_trn <- get_data(all_data, "L254.FinalEnergyKeyword_trn")
    L254.tranSubsectorLogit <- get_data(all_data, "L254.tranSubsectorLogit")
    L254.tranSubsectorShrwtFllt <- get_data(all_data, "L254.tranSubsectorShrwtFllt")
    L254.tranSubsectorInterp <- get_data(all_data, "L254.tranSubsectorInterp")

    L254.tranSubsectorSpeed_passthru <- get_data(all_data, "L254.tranSubsectorSpeed_passthru")
    L254.tranSubsectorSpeed_noVOTT <- get_data(all_data, "L254.tranSubsectorSpeed_noVOTT")
    L254.tranSubsectorSpeed_nonmotor <- get_data(all_data, "L254.tranSubsectorSpeed_nonmotor")
    L254.tranSubsectorVOTT <- get_data(all_data, "L254.tranSubsectorVOTT")
    L254.tranSubsectorFuelPref <- get_data(all_data, "L254.tranSubsectorFuelPref")

    L254.StubTech_passthru <- get_data(all_data, "L254.StubTech_passthru")
    L254.StubTech_nonmotor <- get_data(all_data, "L254.StubTech_nonmotor")
    L254.GlobalTechShrwt_passthru<- get_data(all_data, "L254.GlobalTechShrwt_passthru")
    L254.GlobalTechShrwt_nonmotor <- get_data(all_data, "L254.GlobalTechShrwt_nonmotor")
    L254.GlobalTechCoef_passthru <- get_data(all_data, "L254.GlobalTechCoef_passthru")
    L254.GlobalRenewTech_nonmotor <- get_data(all_data, "L254.GlobalRenewTech_nonmotor")
    L254.GlobalTranTechInterp <- get_data(all_data, "L254.GlobalTranTechInterp")
    L254.GlobalTranTechShrwt <- get_data(all_data, "L254.GlobalTranTechShrwt")
    L254.GlobalTranTechSCurve <- get_data(all_data, "L254.GlobalTranTechSCurve")
    L254.StubTranTechCalInput <- get_data(all_data, "L254.StubTranTechCalInput")


    L254.StubTranTechCoef <- get_data(all_data, "L254.StubTranTechCoef")
    L254.StubTechCalInput_passthru <- get_data(all_data, "L254.StubTechCalInput_passthru")
    L254.StubTechProd_nonmotor <- get_data(all_data, "L254.StubTechProd_nonmotor")
    L254.PerCapitaBased_trn <- get_data(all_data, "L254.PerCapitaBased_trn")
    L254.PriceElasticity_trn <- get_data(all_data, "L254.PriceElasticity_trn")
    L254.IncomeElasticity_trn <- get_data(all_data, "L254.IncomeElasticity_trn")
    L254.BaseService_trn <- get_data(all_data, "L254.BaseService_trn")


    # ===================================================

    # Produce outputs
    # Because `return_data` gets the name of the object from what's actually given in the call,
    # we need to assign xml_tmp to a correctly-named variable in the current environment
    # transportation_UCD_CORE.xml <- transportation_UCD_SSP1.xml <- transportation_UCD_SSP2.xml <-
    #   transportation_UCD_SSP3.xml <- transportation_UCD_SSP5.xml <- transportation_UCD_CORE_highEV.xml <- NULL  # silence package check notes
    transport_downscaling.xml <- NULL  # silence package check notes

    ret_data <- c()
    curr_env <- environment()

    for (i in c("CORE")){
      xml_name <- paste0("transport_downscaling.xml")
      #Read SSP specific data
      L254.tranSubsectorSpeed_SSP <- L254.tranSubsectorSpeed %>% filter(sce== i)
      L254.StubTranTech_SSP <- L254.StubTranTech %>% filter(sce== i)

      L254.tranSubsectorSpeed_passthru_SSP <- L254.tranSubsectorSpeed_passthru %>% filter(sce=="CORE")
      L254.tranSubsectorVOTT_SSP<- L254.tranSubsectorVOTT %>% filter(sce=="CORE")
      L254.tranSubsectorFuelPref_SSP<-L254.tranSubsectorFuelPref %>% filter(sce=="CORE")
      L254.PerCapitaBased_trn_SSP <- L254.PerCapitaBased_trn %>% filter(sce=="CORE")
      L254.PriceElasticity_trn_SSP <- L254.PriceElasticity_trn %>% filter(sce=="CORE")
      L254.IncomeElasticity_trn_SSP <- L254.IncomeElasticity_trn %>% filter(sce=="CORE")

      #kbn 2020-02-11 For the SSPs, we want to bring in values such as co-efficients, load factors and costs after the base year. This is because we are
      # feeding the model outputs from the CORE in the base year, so having SSP values for these variables in the base year would lead to a calibration error
      # i.e. mismatch between calibrated output and actual.

      L254.StubTranTechLoadFactor_SSP <- L254.StubTranTechLoadFactor %>% filter(sce== i)
      if (i != "CORE"){L254.StubTranTechLoadFactor_SSP<-L254.StubTranTechLoadFactor %>%  filter(sce== i) %>% filter(year>MODEL_FINAL_BASE_YEAR)}

      L254.StubTranTechCost_SSP <- L254.StubTranTechCost %>%  filter(sce== i)
      if (i != "CORE"){L254.StubTranTechCost_SSP<-L254.StubTranTechCost %>%  filter(sce== i) %>% filter(year>MODEL_FINAL_BASE_YEAR)}

      L254.StubTechTrackCapital_SSP <- L254.StubTechTrackCapital %>%  filter(sce== i)
      if (i != "CORE"){L254.StubTechTrackCapital_SSP<-L254.StubTechTrackCapital %>%  filter(sce== i) %>% filter(year>MODEL_FINAL_BASE_YEAR)}

      L254.StubTranTechCoef_SSP <- L254.StubTranTechCoef %>%  filter(sce== i)

      if (i != "CORE"){L254.StubTranTechCoef_SSP<-L254.StubTranTechCoef %>%  filter(sce== i) %>% filter(year>MODEL_FINAL_BASE_YEAR)}

      L254.StubTech_passthru_SSP <- L254.StubTech_passthru %>% filter(sce==i)
      L254.StubTech_nonmotor_SSP <- L254.StubTech_nonmotor %>% filter(sce==i)
      L254.Supplysector_trn_SSP  <- L254.Supplysector_trn %>% filter(sce==i)
      L254.FinalEnergyKeyword_trn_SSP <- L254.FinalEnergyKeyword_trn %>% filter(sce==i)
      L254.tranSubsectorLogit_SSP <- L254.tranSubsectorLogit %>% filter(sce==i)
      #L254.tranSubsectorShrwt_SSP <- L254.tranSubsectorShrwt %>%  filter(sce ==i)
      L254.tranSubsectorShrwtFllt_SSP <- L254.tranSubsectorShrwtFllt %>%  filter(sce ==i)
      L254.tranSubsectorInterp_SSP <- L254.tranSubsectorInterp %>%  filter(sce ==i)
      L254.tranSubsectorFuelPref_SSP <- L254.tranSubsectorFuelPref %>%  filter(sce ==i)
      L254.StubTranTechCalInput_SSP <-  L254.StubTranTechCalInput %>% filter(sce ==i)
      L254.GlobalTranTechInterp_SSP <- L254.GlobalTranTechInterp %>% filter(sce==i)
      L254.GlobalTranTechShrwt_SSP <- L254.GlobalTranTechShrwt %>%  filter(sce==i)
      if (i != "CORE"){L254.StubTranTechCalInput_SSP<-L254.StubTranTechCalInput %>%  filter(sce== i) %>% filter(year>MODEL_FINAL_BASE_YEAR)}

      L254.BaseService_trn_SSP <- L254.BaseService_trn %>% filter(sce =="CORE")

      #Split domestic passenger transport into urban transport and other transport
      L254.Supplysector_trn_SSP <- L254.Supplysector_trn_SSP %>%
        mutate(supplysector = ifelse(region == "USA" & supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W"), paste0(supplysector, "_urban"), supplysector)) %>%
        bind_rows(
          L254.Supplysector_trn_SSP %>%
            filter(region == "USA", supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W")) %>%
            mutate(supplysector = paste0(supplysector, "_other"))
        )

      L254.FinalEnergyKeyword_trn_SSP <- L254.FinalEnergyKeyword_trn_SSP %>%
        mutate(supplysector = ifelse(region == "USA" & supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W"), paste0(supplysector, "_urban"), supplysector)) %>%
        bind_rows(
          L254.FinalEnergyKeyword_trn_SSP %>%
            filter(region == "USA", supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W")) %>%
            mutate(supplysector = paste0(supplysector, "_other"))
        )

      L254.tranSubsectorLogit_SSP <- L254.tranSubsectorLogit_SSP %>%
        mutate(supplysector = ifelse(region == "USA" & supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W"), paste0(supplysector, "_urban"), supplysector)) %>%
        bind_rows(
          L254.tranSubsectorLogit_SSP %>%
            filter(region == "USA", supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W")) %>%
            mutate(supplysector = paste0(supplysector, "_other"))
        ) %>%
        filter(!(supplysector == "trn_pass_urban" & tranSubsector == "Domestic Aviation")) %>%
        filter(!(supplysector == "trn_pass_urban" & tranSubsector == "HSR")) %>%
        filter(!(supplysector == "trn_pass_other" & tranSubsector == "Cycle")) %>%
        filter(!(supplysector == "trn_pass_other" & tranSubsector == "Walk")) %>%
        filter(!(supplysector == "trn_pass_road_LDV_other" & tranSubsector == "2W and 3W"))

      L254.tranSubsectorShrwtFllt_SSP <- L254.tranSubsectorShrwtFllt_SSP %>%
        mutate(supplysector = ifelse(region == "USA" & supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W"), paste0(supplysector, "_urban"), supplysector)) %>%
        bind_rows(
          L254.tranSubsectorShrwtFllt_SSP %>%
            filter(region == "USA", supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W")) %>%
            mutate(supplysector = paste0(supplysector, "_other"))
        ) %>%
        filter(!(supplysector == "trn_pass_urban" & tranSubsector == "Domestic Aviation")) %>%
        filter(!(supplysector == "trn_pass_urban" & tranSubsector == "HSR")) %>%
        filter(!(supplysector == "trn_pass_other" & tranSubsector == "Cycle")) %>%
        filter(!(supplysector == "trn_pass_other" & tranSubsector == "Walk")) %>%
        filter(!(supplysector == "trn_pass_road_LDV_other" & tranSubsector == "2W and 3W"))

      L254.tranSubsectorInterp_SSP <- L254.tranSubsectorInterp_SSP %>%
        mutate(supplysector = ifelse(region == "USA" & supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W"), paste0(supplysector, "_urban"), supplysector)) %>%
        bind_rows(
          L254.tranSubsectorInterp_SSP %>%
            filter(region == "USA", supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W")) %>%
            mutate(supplysector = paste0(supplysector, "_other"))
        ) %>%
        filter(!(supplysector == "trn_pass_urban" & tranSubsector == "Domestic Aviation")) %>%
        filter(!(supplysector == "trn_pass_urban" & tranSubsector == "HSR")) %>%
        filter(!(supplysector == "trn_pass_other" & tranSubsector == "Cycle")) %>%
        filter(!(supplysector == "trn_pass_other" & tranSubsector == "Walk")) %>%
        filter(!(supplysector == "trn_pass_road_LDV_other" & tranSubsector == "2W and 3W"))

      L254.tranSubsectorSpeed_SSP <- L254.tranSubsectorSpeed_SSP %>%
        mutate(supplysector = ifelse(region == "USA" & supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W"), paste0(supplysector, "_urban"), supplysector)) %>%
        bind_rows(
          L254.tranSubsectorSpeed_SSP %>%
            filter(region == "USA", supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W")) %>%
            mutate(supplysector = paste0(supplysector, "_other"))
        ) %>%
        filter(!(supplysector == "trn_pass_urban" & tranSubsector == "Domestic Aviation")) %>%
        filter(!(supplysector == "trn_pass_urban" & tranSubsector == "HSR")) %>%
        filter(!(supplysector == "trn_pass_other" & tranSubsector == "Cycle")) %>%
        filter(!(supplysector == "trn_pass_other" & tranSubsector == "Walk")) %>%
        filter(!(supplysector == "trn_pass_road_LDV_other" & tranSubsector == "2W and 3W"))

      L254.tranSubsectorSpeed_passthru_SSP <- L254.tranSubsectorSpeed_passthru_SSP %>%
        mutate(supplysector = ifelse(region == "USA" & supplysector == "trn_pass_road_LDV", paste0(supplysector, "_urban"), supplysector)) %>%
        bind_rows(
          L254.tranSubsectorSpeed_passthru_SSP %>%
            filter(region == "USA", supplysector == "trn_pass_road_LDV") %>%
            mutate(supplysector = paste0(supplysector, "_other"))
        )

      L254.tranSubsectorSpeed_noVOTT <- L254.tranSubsectorSpeed_noVOTT %>%
        mutate(supplysector = ifelse(region == "USA" & supplysector %in% c("trn_pass", "trn_pass_road"), paste0(supplysector, "_urban"), supplysector)) %>%
        bind_rows(
          L254.tranSubsectorSpeed_noVOTT %>%
            filter(region == "USA", supplysector %in% c("trn_pass", "trn_pass_road")) %>%
            mutate(supplysector = paste0(supplysector, "_other"))
        )

      L254.tranSubsectorSpeed_nonmotor <- L254.tranSubsectorSpeed_nonmotor %>%
        mutate(supplysector = ifelse(region == "USA" & supplysector == "trn_pass", paste0(supplysector, "_urban"), supplysector))

      L254.tranSubsectorVOTT_SSP <- L254.tranSubsectorVOTT_SSP %>%
        mutate(supplysector = ifelse(region == "USA" & supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV"), paste0(supplysector, "_urban"), supplysector)) %>%
        bind_rows(
          L254.tranSubsectorVOTT_SSP %>%
            filter(region == "USA", supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV")) %>%
            mutate(supplysector = paste0(supplysector, "_other"))
        ) %>%
        filter(!(supplysector == "trn_pass_urban" & tranSubsector == "Domestic Aviation")) %>%
        filter(!(supplysector == "trn_pass_urban" & tranSubsector == "HSR")) %>%
        filter(!(supplysector == "trn_pass_other" & tranSubsector == "Cycle")) %>%
        filter(!(supplysector == "trn_pass_other" & tranSubsector == "Walk")) %>%
        filter(!(supplysector == "trn_pass_road_LDV_other" & tranSubsector == "2W and 3W"))

      L254.tranSubsectorFuelPref_SSP <- L254.tranSubsectorFuelPref_SSP %>%
        mutate(supplysector = ifelse(region == "USA" & supplysector == "trn_pass_road_LDV_4W", paste0(supplysector, "_urban"), supplysector)) %>%
        bind_rows(
          L254.tranSubsectorFuelPref_SSP %>%
            filter(region == "USA", supplysector == "trn_pass_road_LDV_4W") %>%
            mutate(supplysector = paste0(supplysector, "_other"))
        )

      L254.StubTranTech_SSP <- L254.StubTranTech_SSP %>%
        mutate(supplysector = ifelse(region == "USA" & supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W"), paste0(supplysector, "_urban"), supplysector)) %>%
        bind_rows(
          L254.StubTranTech_SSP %>%
            filter(region == "USA", supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W")) %>%
            mutate(supplysector = paste0(supplysector, "_other"))
        ) %>%
        filter(!(supplysector == "trn_pass_urban" & tranSubsector == "Domestic Aviation")) %>%
        filter(!(supplysector == "trn_pass_urban" & tranSubsector == "HSR")) %>%
        filter(!(supplysector == "trn_pass_other" & tranSubsector == "Cycle")) %>%
        filter(!(supplysector == "trn_pass_other" & tranSubsector == "Walk")) %>%
        filter(!(supplysector == "trn_pass_road_LDV_other" & tranSubsector == "2W and 3W"))

      L254.StubTech_passthru_SSP <- L254.StubTech_passthru_SSP %>%
        mutate(supplysector = ifelse(region == "USA" & supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV"), paste0(supplysector, "_urban"), supplysector)) %>%
        bind_rows(
          L254.StubTech_passthru_SSP %>%
            filter(region == "USA", supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV")) %>%
            mutate(supplysector = paste0(supplysector, "_other"))
        )

      L254.StubTech_nonmotor_SSP <- L254.StubTech_nonmotor_SSP %>%
        mutate(supplysector = ifelse(region == "USA" & supplysector == "trn_pass", paste0(supplysector, "_urban"), supplysector))

      L254.GlobalTechShrwt_passthru <- L254.GlobalTechShrwt_passthru %>%
        bind_rows(
          L254.GlobalTechShrwt_passthru %>%
            filter(sector.name %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV")) %>%
            mutate(sector.name = paste0(sector.name, "_urban"))
        ) %>%
        bind_rows(
          L254.GlobalTechShrwt_passthru %>%
            filter(sector.name %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV")) %>%
            mutate(sector.name = paste0(sector.name, "_other"))
        )

      L254.GlobalTechShrwt_nonmotor <- L254.GlobalTechShrwt_nonmotor %>%
        bind_rows(
          L254.GlobalTechShrwt_nonmotor %>%
            filter(sector.name == "trn_pass") %>%
            mutate(sector.name = paste0(sector.name, "_urban"))
        )

      L254.GlobalTechCoef_passthru <- L254.GlobalTechCoef_passthru %>%
        bind_rows(
          L254.GlobalTechCoef_passthru %>%
            filter(sector.name %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV")) %>%
            mutate(sector.name = paste0(sector.name, "_urban"),
                   minicam.energy.input = paste0(minicam.energy.input, "_urban"))
        ) %>%
        bind_rows(
          L254.GlobalTechCoef_passthru %>%
            filter(sector.name %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV")) %>%
            mutate(sector.name = paste0(sector.name, "_other"),
                   minicam.energy.input = paste0(minicam.energy.input, "_other"))
        )

      L254.GlobalRenewTech_nonmotor <- L254.GlobalRenewTech_nonmotor %>%
        bind_rows(
          L254.GlobalRenewTech_nonmotor %>%
            filter(sector.name == "trn_pass") %>%
            mutate(sector.name = paste0(sector.name, "_urban"))
        )

      L254.GlobalTranTechInterp_SSP <- L254.GlobalTranTechInterp_SSP %>%
        bind_rows(
          L254.GlobalTranTechInterp_SSP %>%
            filter(sector.name %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W")) %>%
            mutate(sector.name = paste0(sector.name, "_urban"))
        ) %>%
        bind_rows(
          L254.GlobalTranTechInterp_SSP %>%
            filter(sector.name %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W")) %>%
            mutate(sector.name = paste0(sector.name, "_other"))
        ) %>%
        filter(!(sector.name == "trn_pass_urban" & subsector.name == "Domestic Aviation")) %>%
        filter(!(sector.name == "trn_pass_urban" & subsector.name == "HSR")) %>%
        filter(!(sector.name == "trn_pass_other" & subsector.name == "Cycle")) %>%
        filter(!(sector.name == "trn_pass_other" & subsector.name == "Walk")) %>%
        filter(!(sector.name == "trn_pass_road_LDV_other" & subsector.name == "2W and 3W"))

      L254.GlobalTranTechShrwt_SSP <- L254.GlobalTranTechShrwt_SSP %>%
        bind_rows(
          L254.GlobalTranTechShrwt_SSP %>%
            filter(sector.name %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W")) %>%
            mutate(sector.name = paste0(sector.name, "_urban"))
        ) %>%
        bind_rows(
          L254.GlobalTranTechShrwt_SSP %>%
            filter(sector.name %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W")) %>%
            mutate(sector.name = paste0(sector.name, "_other"))
        ) %>%
        filter(!(sector.name == "trn_pass_urban" & subsector.name == "Domestic Aviation")) %>%
        filter(!(sector.name == "trn_pass_urban" & subsector.name == "HSR")) %>%
        filter(!(sector.name == "trn_pass_other" & subsector.name == "Cycle")) %>%
        filter(!(sector.name == "trn_pass_other" & subsector.name == "Walk")) %>%
        filter(!(sector.name == "trn_pass_road_LDV_other" & subsector.name == "2W and 3W"))

      L254.GlobalTranTechSCurve <- L254.GlobalTranTechSCurve %>%
        bind_rows(
          L254.GlobalTranTechSCurve %>%
            filter(sector.name == "trn_pass_road_LDV_4W") %>%
            mutate(sector.name = paste0(sector.name, "_urban"))
        ) %>%
        bind_rows(
          L254.GlobalTranTechSCurve %>%
            filter(sector.name == "trn_pass_road_LDV_4W") %>%
            mutate(sector.name = paste0(sector.name, "_other"))
        )

      L254.StubTranTechCalInput_SSP <- L254.StubTranTechCalInput_SSP %>%
        mutate(calibrated.value = ifelse(region == "USA" & supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W"), calibrated.value/2, calibrated.value))

      L254.StubTranTechCalInput_SSP <- L254.StubTranTechCalInput_SSP %>%
        mutate(supplysector = ifelse(region == "USA" & supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W"), paste0(supplysector, "_urban"), supplysector)) %>%
        bind_rows(
          L254.StubTranTechCalInput_SSP %>%
            filter(region == "USA", supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W")) %>%
            mutate(supplysector = paste0(supplysector, "_other"))
        ) %>%
        filter(!(supplysector == "trn_pass_urban" & tranSubsector == "Domestic Aviation")) %>%
        filter(!(supplysector == "trn_pass_urban" & tranSubsector == "HSR")) %>%
        filter(!(supplysector == "trn_pass_road_LDV_other" & tranSubsector == "2W and 3W")) %>%
        mutate(calibrated.value = ifelse(region == "USA" & supplysector == "trn_pass_other" & tranSubsector == "Domestic Aviation", calibrated.value*2, calibrated.value)) %>%
        mutate(calibrated.value = ifelse(region == "USA" & supplysector == "trn_pass_other" & tranSubsector == "HSR", calibrated.value*2, calibrated.value)) %>%
        mutate(calibrated.value = ifelse(region == "USA" & supplysector == "trn_pass_road_LDV_urban" & tranSubsector == "2W and 3W", calibrated.value*2, calibrated.value))

      L254.StubTranTechLoadFactor_SSP <- L254.StubTranTechLoadFactor_SSP %>%
        mutate(supplysector = ifelse(region == "USA" & supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W"), paste0(supplysector, "_urban"), supplysector)) %>%
        bind_rows(
          L254.StubTranTechLoadFactor_SSP %>%
            filter(region == "USA", supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W")) %>%
            mutate(supplysector = paste0(supplysector, "_other"))
        ) %>%
        filter(!(supplysector == "trn_pass_urban" & tranSubsector == "Domestic Aviation")) %>%
        filter(!(supplysector == "trn_pass_urban" & tranSubsector == "HSR")) %>%
        filter(!(supplysector == "trn_pass_road_LDV_other" & tranSubsector == "2W and 3W"))

      L254.StubTechTrackCapital_SSP <- L254.StubTechTrackCapital_SSP %>%
        mutate(supplysector = ifelse(region == "USA" & supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W"), paste0(supplysector, "_urban"), supplysector)) %>%
        bind_rows(
          L254.StubTechTrackCapital_SSP %>%
            filter(region == "USA", supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W")) %>%
            mutate(supplysector = paste0(supplysector, "_other"))
        ) %>%
        filter(!(supplysector == "trn_pass_urban" & subsector == "Domestic Aviation")) %>%
        filter(!(supplysector == "trn_pass_urban" & subsector == "HSR")) %>%
        filter(!(supplysector == "trn_pass_road_LDV_other" & subsector == "2W and 3W"))

      L254.StubTranTechCost_SSP <- L254.StubTranTechCost_SSP %>%
        mutate(supplysector = ifelse(region == "USA" & supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W"), paste0(supplysector, "_urban"), supplysector)) %>%
        bind_rows(
          L254.StubTranTechCost_SSP %>%
            filter(region == "USA", supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W")) %>%
            mutate(supplysector = paste0(supplysector, "_other"))
        ) %>%
        filter(!(supplysector == "trn_pass_urban" & tranSubsector == "Domestic Aviation")) %>%
        filter(!(supplysector == "trn_pass_urban" & tranSubsector == "HSR")) %>%
        filter(!(supplysector == "trn_pass_road_LDV_other" & tranSubsector == "2W and 3W"))

      L254.StubTranTechCoef_SSP <- L254.StubTranTechCoef_SSP %>%
        mutate(supplysector = ifelse(region == "USA" & supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W"), paste0(supplysector, "_urban"), supplysector)) %>%
        bind_rows(
          L254.StubTranTechCoef_SSP %>%
            filter(region == "USA", supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV", "trn_pass_road_LDV_4W")) %>%
            mutate(supplysector = paste0(supplysector, "_other"))
        ) %>%
        filter(!(supplysector == "trn_pass_urban" & tranSubsector == "Domestic Aviation")) %>%
        filter(!(supplysector == "trn_pass_urban" & tranSubsector == "HSR")) %>%
        filter(!(supplysector == "trn_pass_road_LDV_other" & tranSubsector == "2W and 3W"))

      L254.StubTechCalInput_passthru <- L254.StubTechCalInput_passthru %>%
        mutate(calibrated.value = ifelse(region == "USA" & supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV"), calibrated.value/2, calibrated.value))

      L254.StubTechCalInput_passthru <- L254.StubTechCalInput_passthru %>%
        mutate(supplysector = ifelse(region == "USA" & supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV"), paste0(supplysector, "_urban"), supplysector)) %>%
        bind_rows(
          L254.StubTechCalInput_passthru %>%
            filter(region == "USA", supplysector %in% c("trn_pass", "trn_pass_road", "trn_pass_road_LDV")) %>%
            mutate(supplysector = paste0(supplysector, "_other"),
                   minicam.energy.input = paste0(minicam.energy.input, "_other"))
        ) %>%
        mutate(minicam.energy.input = ifelse(region == "USA" & supplysector %in% c("trn_pass_urban", "trn_pass_road_urban", "trn_pass_road_LDV_urban"), paste0(minicam.energy.input, "_urban"), minicam.energy.input)) %>%
        mutate(calibrated.value = case_when(region == "USA" & minicam.energy.input == "trn_pass_road_LDV_urban" & year == 1975 ~ 2109880.51533151,
                                            region == "USA" & minicam.energy.input == "trn_pass_road_LDV_urban" & year == 1990 ~ 2459976.58480722,
                                            region == "USA" & minicam.energy.input == "trn_pass_road_LDV_urban" & year == 2005 ~ 3302121.35131459,
                                            region == "USA" & minicam.energy.input == "trn_pass_road_LDV_urban" & year == 2010 ~ 3321738.22110968,
                                            region == "USA" & minicam.energy.input == "trn_pass_road_LDV_urban" & year == 2015 ~ 3841172.90615024,
                                            region == "USA" & minicam.energy.input == "trn_pass_road_LDV_urban" & year == 2021 ~ 4099863.57718159,
                                            region == "USA" & minicam.energy.input == "trn_pass_road_urban" & year == 1975 ~ 2273864.02053798,
                                            region == "USA" & minicam.energy.input == "trn_pass_road_urban" & year == 1990 ~ 2651170.1601483,
                                            region == "USA" & minicam.energy.input == "trn_pass_road_urban" & year == 2005 ~ 3577021.07100884,
                                            region == "USA" & minicam.energy.input == "trn_pass_road_urban" & year == 2010 ~ 3594036.53269294,
                                            region == "USA" & minicam.energy.input == "trn_pass_road_urban" & year == 2015 ~ 4125748.69312287,
                                            region == "USA" & minicam.energy.input == "trn_pass_road_urban" & year == 2021 ~ 4398711.25491208,
                                            region == "USA" & minicam.energy.input == "trn_pass_road_other" & year == 1975 ~ 2250245.87349525,
                                            region == "USA" & minicam.energy.input == "trn_pass_road_other" & year == 1990 ~ 2623632.98408229,
                                            region == "USA" & minicam.energy.input == "trn_pass_road_other" & year == 2005 ~ 3540056.9213919,
                                            region == "USA" & minicam.energy.input == "trn_pass_road_other" & year == 2010 ~ 3556852.18349354,
                                            region == "USA" & minicam.energy.input == "trn_pass_road_other" & year == 2015 ~ 4086579.52863074,
                                            region == "USA" & minicam.energy.input == "trn_pass_road_other" & year == 2021 ~ 4358650.13137165,
                                            TRUE ~ calibrated.value
        ))

      L254.StubTechProd_nonmotor <- L254.StubTechProd_nonmotor %>%
        mutate(supplysector = ifelse(region == "USA" & supplysector == "trn_pass", paste0(supplysector, "_urban"), supplysector))

      L254.PerCapitaBased_trn_SSP <- L254.PerCapitaBased_trn_SSP %>%
        mutate(energy.final.demand = ifelse(region == "USA" & energy.final.demand == "trn_pass", paste0(energy.final.demand, "_urban"), energy.final.demand)) %>%
        bind_rows(
          L254.PerCapitaBased_trn_SSP %>%
            filter(region == "USA", energy.final.demand == "trn_pass") %>%
            mutate(energy.final.demand = paste0(energy.final.demand, "_other"))
        )

      L254.PriceElasticity_trn_SSP <- L254.PriceElasticity_trn_SSP %>%
        mutate(energy.final.demand = ifelse(region == "USA" & energy.final.demand == "trn_pass", paste0(energy.final.demand, "_urban"), energy.final.demand)) %>%
        bind_rows(
          L254.PriceElasticity_trn_SSP %>%
            filter(region == "USA", energy.final.demand == "trn_pass") %>%
            mutate(energy.final.demand = paste0(energy.final.demand, "_other"))
        )

      L254.IncomeElasticity_trn_SSP <- L254.IncomeElasticity_trn_SSP %>%
        mutate(energy.final.demand = ifelse(region == "USA" & energy.final.demand == "trn_pass", paste0(energy.final.demand, "_urban"), energy.final.demand)) %>%
        bind_rows(
          L254.IncomeElasticity_trn_SSP %>%
            filter(region == "USA", energy.final.demand == "trn_pass") %>%
            mutate(energy.final.demand = paste0(energy.final.demand, "_other"))
        )

      L254.BaseService_trn_SSP <- L254.BaseService_trn_SSP %>%
        mutate(base.service = ifelse(region == "USA" & energy.final.demand == "trn_pass", base.service/2, base.service))

      L254.BaseService_trn_SSP <- L254.BaseService_trn_SSP %>%
        mutate(energy.final.demand = ifelse(region == "USA" & energy.final.demand == "trn_pass", paste0(energy.final.demand, "_urban"), energy.final.demand)) %>%
        bind_rows(
          L254.BaseService_trn_SSP %>%
            filter(region == "USA", energy.final.demand == "trn_pass") %>%
            mutate(energy.final.demand = paste0(energy.final.demand, "_other"))
        ) %>%
        mutate(base.service = case_when(region == "USA" & energy.final.demand == "trn_pass_urban" & year == 1975 ~ 2319429.4010548,
                                        region == "USA" & energy.final.demand == "trn_pass_urban" & year == 1990 ~ 2704181.42578904,
                                        region == "USA" & energy.final.demand == "trn_pass_urban" & year == 2005 ~ 3641983.49324781,
                                        region == "USA" & energy.final.demand == "trn_pass_urban" & year == 2010 ~ 3661819.15584363,
                                        region == "USA" & energy.final.demand == "trn_pass_urban" & year == 2015 ~ 4198483.68431905,
                                        region == "USA" & energy.final.demand == "trn_pass_urban" & year == 2021 ~ 4477748.24006887,
                                        region == "USA" & energy.final.demand == "trn_pass_other" & year == 1975 ~ 2811702.92328387,
                                        region == "USA" & energy.final.demand == "trn_pass_other" & year == 1990 ~ 3462177.49347627,
                                        region == "USA" & energy.final.demand == "trn_pass_other" & year == 2005 ~ 4469331.20708423,
                                        region == "USA" & energy.final.demand == "trn_pass_other" & year == 2010 ~ 4356539.41199294,
                                        region == "USA" & energy.final.demand == "trn_pass_other" & year == 2015 ~ 4976624.04677327,
                                        region == "USA" & energy.final.demand == "trn_pass_other" & year == 2021 ~ 5190691.64827983,
                                        TRUE ~ base.service
        ))

      #Create xmls
      create_xml(xml_name) %>%
        add_logit_tables_xml(L254.Supplysector_trn_SSP, "Supplysector") %>%
        add_xml_data(L254.FinalEnergyKeyword_trn_SSP, "FinalEnergyKeyword") %>%
        add_logit_tables_xml(L254.tranSubsectorLogit_SSP, "tranSubsectorLogit", "tranSubsector") %>%
        add_xml_data(L254.tranSubsectorShrwtFllt_SSP, "tranSubsectorShrwtFllt") %>%
        add_xml_data(L254.tranSubsectorInterp_SSP, "tranSubsectorInterp") %>%
        add_xml_data(L254.tranSubsectorSpeed_SSP, "tranSubsectorSpeed") %>%
        add_xml_data(L254.tranSubsectorSpeed_passthru_SSP, "tranSubsectorSpeed") %>%
        add_xml_data(L254.tranSubsectorSpeed_noVOTT, "tranSubsectorSpeed") %>%
        add_xml_data(L254.tranSubsectorSpeed_nonmotor, "tranSubsectorSpeed") %>%
        add_xml_data(L254.tranSubsectorVOTT_SSP, "tranSubsectorVOTT") %>%
        add_xml_data(L254.tranSubsectorFuelPref_SSP, "tranSubsectorFuelPref") %>%
        add_xml_data(L254.StubTranTech_SSP, "StubTranTech") %>%
        add_xml_data(L254.StubTech_passthru_SSP, "StubTranTech") %>%
        add_xml_data(L254.StubTech_nonmotor_SSP, "StubTranTech") %>%
        add_xml_data(L254.GlobalTechShrwt_passthru, "GlobalTechShrwt") %>%
        add_xml_data(L254.GlobalTechShrwt_nonmotor, "GlobalTechShrwt") %>%
        add_xml_data(L254.GlobalTechCoef_passthru, "GlobalTechCoef") %>%
        add_xml_data(L254.GlobalRenewTech_nonmotor, "GlobalRenewTech") %>%
        add_xml_data(L254.GlobalTranTechInterp_SSP, "GlobalTranTechInterp") %>%
        add_xml_data(L254.GlobalTranTechShrwt_SSP, "GlobalTranTechShrwt") %>%
        add_xml_data(L254.GlobalTranTechSCurve, "GlobalTranTechSCurve") %>%
        add_node_equiv_xml("technology") %>%
        add_node_equiv_xml("input") %>%
        add_xml_data(L254.StubTranTechCalInput_SSP, "StubTranTechCalInput") %>%
        add_xml_data(L254.StubTranTechLoadFactor_SSP, "StubTranTechLoadFactor") %>%
        add_node_equiv_xml("subsector") %>%
        add_xml_data(L254.StubTechTrackCapital_SSP, "StubTechTrackCapital") %>%
        add_xml_data(L254.StubTranTechCost_SSP, "StubTranTechCost") %>%
        add_xml_data(L254.StubTranTechCoef_SSP, "StubTranTechCoef") %>%
        add_xml_data(L254.StubTechCalInput_passthru, "StubTranTechCalInput") %>%
        add_xml_data(L254.StubTechProd_nonmotor, "StubTranTechProd") %>%
        add_xml_data(L254.PerCapitaBased_trn_SSP, "PerCapitaBased") %>%
        add_xml_data(L254.PriceElasticity_trn_SSP, "PriceElasticity") %>%
        add_xml_data(L254.IncomeElasticity_trn_SSP, "IncomeElasticity") %>%
        add_xml_data(L254.BaseService_trn_SSP, "BaseService") %>%
        add_precursors("L254.Supplysector_trn",
                       "L254.FinalEnergyKeyword_trn",
                       "L254.tranSubsectorLogit",
                       "L254.tranSubsectorShrwtFllt",
                       "L254.tranSubsectorInterp",
                       "L254.tranSubsectorSpeed",
                       "L254.tranSubsectorSpeed_passthru",
                       "L254.tranSubsectorSpeed_noVOTT",
                       "L254.tranSubsectorSpeed_nonmotor",
                       "L254.tranSubsectorVOTT",
                       "L254.tranSubsectorFuelPref",
                       "L254.StubTranTech",
                       "L254.StubTech_passthru",
                       "L254.StubTech_nonmotor",
                       "L254.GlobalTechShrwt_passthru",
                       "L254.GlobalTechShrwt_nonmotor",
                       "L254.GlobalTechCoef_passthru",
                       "L254.GlobalRenewTech_nonmotor",
                       "L254.GlobalTranTechInterp",
                       "L254.GlobalTranTechShrwt",
                       "L254.GlobalTranTechSCurve",
                       "L254.StubTranTechCalInput",
                       "L254.StubTranTechLoadFactor",
                       "L254.StubTranTechCost",
                       "L254.StubTechTrackCapital",
                       "L254.StubTranTechCoef",
                       "L254.StubTechCalInput_passthru",
                       "L254.StubTechProd_nonmotor",
                       "L254.PerCapitaBased_trn",
                       "L254.PriceElasticity_trn",
                       "L254.IncomeElasticity_trn",
                       "L254.BaseService_trn")  %>%
        assign(xml_name, ., envir = curr_env)


      ret_data <- c(ret_data, xml_name)

    }
    #Return all xmls
    ret_data %>%
      paste(collapse = ", ") %>%
      paste0("return_data(", ., ")") %>%
      parse(text = .) %>%
      eval()

  } else {
    stop("Unknown command")
  }
}


