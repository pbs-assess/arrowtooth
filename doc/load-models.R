# This is a list of vectors of bridge groups (bridge models that will be
# plotted against each other). It can be `NULL` if to be ignored.
bridge_models_dirs <-
  list(c("01-base-2015",
         "02-bridge-update-data-to-2014",
         "03-bridge-update-data-to-2021",
         "04-bridge-add-wchg"),
       c("05-bridge-switch-to-dm-likelihood",
         "06-bridge-switch-to-2-fleet-model",
         "07-bridge-add-discard-cpue",
         "08-bridge-switch-to-split-sex"),
       c("09-bridge-switch-fishing-year-to-feb-21-feb-20",
         "10-bridge-remove-wchg",
         "11-bridge-fix-natural-mortalities",
         "12-bridge-qcs-tv-selex",
         "13-bridge-estimate-total-variance-qcs-tv"))

bridge_models_text <-
  list(c("2015 Base model (one fleet, single sex)",
         "Update data to 2014 (one fleet, single sex)",
         "Update data to 2021 (one fleet, single sex)",
         "Add WCHG survey (one fleet, single sex)"),
       c("Switch to DM likelihood (one fleet, single sex)",
         "Change to two-fleet model (two fleet, single sex)",
         "Add Discard CPUE index (two fleets, single sex)",
         "Convert model to split sex (two fleets, split sex)"),
       c("Change year start to Feb 21 (two fleets, split sex)",
         "Remove WCHG survey (two fleets, split sex)",
         "Fix natural mortalities (two fleets, split sex)",
         "QCS survey TV selectivity (two fleets, split sex)",
         "Estimate total variance and QCS TV (two fleets, split sex)"))

# Make these factors so that they can be reordered in the legends later
bridge_models_text <- bridge_models_text %>% map(~{factor(.x, levels = .x)})

# This is a list of vectors of sensitivity groups (sensitivity models that
# will be plotted against each other). It can be `NULL` if to be ignored.
# The base model will be prepended to each group later in set_dirs() so that
# it is first on the plots for each group.
sens_models_dirs <-
  list(c("01-sigma-0.135",
         "02-tau-1.0",
         "03-tau-0.6",
         "04-low-steepness"),
       c("05-m-female-tight-prior",
         "06-m-female-loose-prior",
         "07-m-male-tight-prior",
         "08-m-male-loose-prior"),
       c("09-qk-mean-1.0",
         "10-qk-loose-prior"),
       c("11-selex-equal-maturity",
         "12-geostat-surveys"))
sens_models_text <-
  list(c(ifelse(fr(),
                "Diminuer $\\sigma$ à 0,135",
                "Decrease $\\sigma$ to 0.135"),
         ifelse(fr(),
                "Augmenter $\\tau$ à 1,0",
                "Increase $\\tau$ to 1.0"),
         ifelse(fr(),
                "Diminuer $\\tau$ de 0,6",
                "Decrease $\\tau$ to 0.6"),
         ifelse(fr(),
                "Diminuer la moyenne de $h$ avant 0,72",
                "Decrease mean of $h$ prior to 0.72")),
       c(ifelse(fr(),
                "Estimation de $M_{femme}$ avec sd préalable = 0.6",
                "Estimate $M_{Female}$ with prior sd = 0.6"),
         ifelse(fr(),
                "Estimation de $M_{femme}$ avec sd préalable = 1.6",
                "Estimate $M_{Female}$ with prior sd = 1.6"),
         ifelse(fr(),
                "Estimation de $M_{homme}$ avec sd préalable = 0.6",
                "Estimate $M_{Male}$ with prior sd = 0.6"),
         ifelse(fr(),
                "Estimation de $M_{homme}$ avec sd préalable = 0.6",
                "Estimate $M_{Male}$ with prior sd = 1.6")),
       c(ifelse(fr(),
                "Augmenter la moyenne antérieure de $q_{k}$ à 1,0",
                "Increase $q_k$ prior mean to 1.0"),
         ifelse(fr(),
                "Priorité plus faible sur $q_{k}$",
                "Looser prior on $q_k$")),
       c(ifelse(fr(),
                "Courbe de sélectivité égale à l'ogive de maturité",
                "Selectivity curve equals maturity ogive"),
         ifelse(fr(),
                "Indices d'enquête basés sur la géostatistique",
                "Geostatistical based survey indices")))
# This will be used to generate the sensitivity parameter table later
sens_models_text_no_base <- sens_models_text
# Add base model text to each sensitivity group
sens_models_text <- map(sens_models_text, ~{c("Base model", .x)})
# Make these factors so that they can be reordered in the legends later
sens_models_text <- sens_models_text %>% map(~{factor(.x, levels = .x)})

sens_changes_text <-
  list(c("$\\vartheta^2$ = 1.538; $\\rho$ = 0.059",
         "$\\vartheta^2$ = 0.962; $\\rho$ = 0.038",
         "$\\vartheta^2$ = 2.500; $\\rho$ = 0.100",
         "$h$ = Beta($\\alpha$ = 11.72, $\\beta$ = 4.56)"),
       c(ifelse(fr(),
                "ln($M_\\mathrm{Femelle}$) = Normale(ln(0.35), 0.5)",
                "ln($M_\\mathrm{Female}$) = Normal(ln(0.35), 0.5)"),
         ifelse(fr(),
                "ln($M_\\mathrm{Femelle}$) = Normale(ln(0.35), 2.5)",
                "ln($M_\\mathrm{Female}$) = Normal(ln(0.35), 2.5)"),
         ifelse(fr(),
                "ln($M_\\mathrm{M\\hat{a}le}$) = Normale(ln(0.2), 0.5)",
                "ln($M_\\mathrm{Male}$) = Normal(ln(0.2), 0.5)"),
         ifelse(fr(),
                "ln($M_\\mathrm{M\\hat{a}le}$) = Normale(ln(0.2), 2.5)",
                "ln($M_\\mathrm{Male}$) = Normal(ln(0.2), 2.5)")),
       c(ifelse(fr(),
                "Moyenne antérieure de $q_\\mathrm{k}$ = 1,0 pour tous les $k$",
                "$q_\\mathrm{k}$ prior mean = 1.0 for all $k$"),
         ifelse(fr(),
                "$q_\\mathrm{k}$ prior sd = 1,5 pour tous les $k$",
                "$q_\\mathrm{k}$ prior sd = 1.5 for all $k$")),
       c(ifelse(fr(),
                "Paramètres de sélectivité des pêcheries = maturité ($A_\\mathrm{50\\%}$ and $SD_\\mathrm{50\\%}$)",
                "Fishery selectivity parameters = maturity ($A_\\mathrm{50\\%}$ and $SD_\\mathrm{50\\%}$)"),
         ifelse(fr(),
                "Indices d'enquête géostatistique (Annexe : \\@ref(app:geostat))",
                "Geostatistical survey indices (Appendix \\@ref(app:geostat))")))

retro_models_dirs <-
  list(c("01-retrospective-1-years",
         "02-retrospective-2-years",
         "03-retrospective-3-years",
         "04-retrospective-4-years",
         "05-retrospective-5-years",
         "06-retrospective-6-years",
         "07-retrospective-7-years"))
retro_models_text <-
  list(c(ifelse(fr(),
                "Modèle de base moins 1 an de données",
                "Base model - 1 year data"),
         ifelse(fr(),
                "Modèle de base moins 2 ans de données",
                "Base model - 2 years data"),
         ifelse(fr(),
                "Modèle de base moins 3 ans de données",
                "Base model - 3 years data"),
         ifelse(fr(),
                "Modèle de base moins 4 ans de données",
                "Base model - 4 years data"),
         ifelse(fr(),
                "Modèle de base moins 5 ans de données",
                "Base model - 5 years data"),
         ifelse(fr(),
                "Modèle de base moins 6 ans de données",
                "Base model - 6 years data"),
         ifelse(fr(),
                "Modèle de base moins 7 ans de données",
                "Base model - 7 years data")))
# Add base model text to each retrospective group
retro_models_text <- map(retro_models_text, ~{c("Base model", .x)})
# Make these factors so that they can be reordered in the legends later
retro_models_text <- retro_models_text %>% map(~{factor(.x, levels = .x)})

if(here() == "/home/rstudio"){
  # For an Rstudio server spawned inside a Docker container
  drs <- set_dirs(nongit_dir = file.path(dirname(here("arrowtooth")),
                                         "arrowtooth-nongit"),
                  base_model_dir = "base",
                  bridge_models_dirs = bridge_models_dirs,
                  sens_models_dirs = sens_models_dirs,
                  retro_models_dirs = retro_models_dirs,
                  check_dir_exists = FALSE)
}else{
  drs <- set_dirs(base_model_dir = "base",
                  bridge_models_dirs = bridge_models_dirs,
                  sens_models_dirs = sens_models_dirs,
                  retro_models_dirs = retro_models_dirs,
                  check_dir_exists = FALSE)
}

models <- model_setup(main_dirs = drs,
                      bridge_models_text = bridge_models_text,
                      sens_models_text = sens_models_text,
                      retro_models_text = retro_models_text,
                      overwrite_rds_files = build_rds)
