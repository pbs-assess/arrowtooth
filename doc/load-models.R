models_dir <- "/srv/arrowtooth/2022"
nongit_dir <- "/srv/arrowtooth/arrowtooth-nongit"

# This is a list of vectors of bridge groups (bridge models that will be
# plotted against each other). It can be `NULL` if to be ignored.
bridge_models_dirs <-
  list(c("01-base-2015",
         "02-bridge-update-data-to-2014",
         "03-bridge-update-data-to-2021",
         "04-bridge-add-wchg"),
       c("05-bridge-switch-to-2-fleet-model",
         "06-bridge-switch-to-dm-likelihood",
         "07-bridge-add-discard-cpue",
         "08-bridge-switch-to-split-sex"),
       c("09-bridge-switch-fishing-year-to-feb-21-feb-20",
         "10-bridge-remove-wchg",
         "11-bridge-fix-natural-mortalities"),
       c("01-base-2015",
         "12-retrospective-7-years"))

bridge_models_text <-
  list(c(ifelse(fr(),
                "2015 Modèle de base (une flotte, un seul sexe)",
                "2015 Base model (one fleet, single sex)"),
         ifelse(fr(),
                "Mise à jour des données jusqu'en 2014 (une flotte, un seul sexe)",
                "Update data to 2014 (one fleet, single sex)"),
         ifelse(fr(),
                "Mise à jour des données jusqu'en 2021 (une flotte, un seul sexe)",
                "Update data to 2021 (one fleet, single sex)"),
         ifelse(fr(),
                "Ajouter l'enquête COHG (une flotte, un seul sexe)",
                "Add WCHG survey (one fleet, single sex)")),
       c(ifelse(fr(),
                "Passage au modèle à deux flottes (deux flottes, un seul sexe)",
                "Change to two-fleet model (two fleets, single sex)"),
         ifelse(fr(),
                "Passage à Dirichlet-Multinomial (deux flottes, un seul sexe)",
                "Switch to Dirichlet-Multinomial (two fleets, single sex)"),
         ifelse(fr(),
                "Ajouter l'indice CPUE des rejets (deux flottes, un seul sexe)",
                "Add Discard CPUE index (two fleets, single sex)"),
         ifelse(fr(),
                "Convertir le modèle en sexe partagé (deux flottes, sexe partagé)",
                "Convert model to split sex (two fleets, split sex)")),
       c(ifelse(fr(),
                "Changement du début de l'année au 21 février (deux flottes, sexe partagé)",
                "Change year start to Feb 21 (two fleets, split sex)"),
         ifelse(fr(),
                "Suppression de l'enquête COHG (deux flottes, sexe divisé)",
                "Remove WCHG survey (two fleets, split sex)"),
         ifelse(fr(),
                "Corriger les mortalités naturelles (deux flottes, sexe divisé)",
                "Fix natural mortalities (two fleets, split sex)")),
       c(ifelse(fr(),
                "French here",
                "2015 base model"),
         ifelse(fr(),
                "French here",
                "Retrospective -7 years")))

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
         "13-qcs-tv-selex"),
       c("12-geostat-surveys"),
       c("16-remove-discard-cpue",
         "17-change-maturity-without-resting",
         "18-set-dcpue-sel-comm-sel",
         "19-fix-all-survey-selex"))
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
                "Estimé de $ln(M_{femme})$ avec sd préalable=0,2",
                "Estimated $ln(M_{female})$ with prior sd=0.2"),
         ifelse(fr(),
                "Estimé de $ln(M_{femme})$ avec sd préalable=1,6",
                "Estimated $ln(M_{female})$ with prior sd=1.6"),
         ifelse(fr(),
                "Estimé de $ln(M_{homme})$ avec sd préalable=0,2",
                "Estimated $ln(M_{male})$ with prior sd=0.2"),
         ifelse(fr(),
                "Estimé de $ln(M_{homme})$ avec sd préalable=1,6",
                "Estimated $ln(M_{male})$ with prior sd=1.6")),
       c(ifelse(fr(),
                "Augmenter la moyenne antérieure de $ln}(q_{k})$ à 1,0",
                "Increase $ln(q_k)$ prior mean to 1.0"),
         ifelse(fr(),
                "Priorité plus large sur $ln(q_{k})$, sd préalable=1,5",
                "Broad prior on $ln(q_k)$, prior sd=1.5")),
       c(ifelse(fr(),
                "Sélectivité commerciale égale à l'ogive de maturité",
                "Comm. selectivities equal maturity ogive"),
         ifelse(fr(),
                "Sélectivité QCS TV blocs de 3 ans",
                "QCS TV selectivity 3 year blocks")),
       c(ifelse(fr(),
                "Indices d'enquête basés sur la géostatistique",
                "Geostatistical based survey indices")),
       c(ifelse(fr(),
                "Retirer Rejeter CPUE",
                "Remove Discard CPUE"),
         ifelse(fr(),
                "Maturité sans 'développement/repos'",
                "Maturity without 'developing/resting'"),
         ifelse(fr(),
                "Fixer la sélectivité DCPUE",
                "Fix DCPUE selectivity"),
         ifelse(fr(),
                "Fixer les sélectivités de l'enquête",
                "Fix survey selectivities")))

# This will be used to generate the sensitivity parameter table later
sens_models_text_no_base <- sens_models_text
# Add base model text to each sensitivity group
sens_models_text <- map(sens_models_text, ~{c("Base model", .x)})
# Make these factors so that they can be reordered in the legends later
sens_models_text <- sens_models_text %>% map(~{factor(.x, levels = .x)})

sens_changes_text <-
  list(c("$\\vartheta^2 = 1.519$; $\\rho = 0.028$",
         "$\\vartheta^2 = 0.962$; $\\rho = 0.038$",
         "$\\vartheta^2 = 2.500$; $\\rho = 0.100$",
         "$Beta(\\alpha = 11.72, \\beta = 4.56)$"),
       c(ifelse(fr(),
                "$Normale(ln(0.20), 0.5)$",
                "$Normal(ln(0.20), 0.5)$"),
         ifelse(fr(),
                "$Normale(ln(0.20), 2.5)$",
                "$Normal(ln(0.20), 2.5)$"),
         ifelse(fr(),
                "$Normale(ln(0.35), 0.5)$",
                "$Normal(ln(0.35), 0.5)$"),
         ifelse(fr(),
                "$Normale(ln(0.35), 2.5)$",
                "$Normal(ln(0.35), 2.5)$")),
       c(ifelse(fr(),
                "$Normal(ln(1.0), 0.5)$ pour tous les engrenages $k$",
                "$Normal(ln(1.0), 0.5)$ for all gears $k$"),
         ifelse(fr(),
                "$Normal(ln(0.5), 1.5)$ pour tous les engrenages $k$",
                "$Normal(ln(0.5), 1.5)$ for all gears $k$")),
       c(ifelse(fr(),
                "$\\hat{a_k} = \\dot{a}; \\hat{\\gamma_k} = \\dot{\\gamma}$ pour les deux flottes $k$",
                "$\\hat{a_k} = \\dot{a}; \\hat{\\gamma_k} = \\dot{\\gamma}$ for both fleets $k$"),
         ifelse(fr(),
                "La sélectivité du DRC varie dans le temps avec les blocs d'années 2003--2010, 2011-2016 et 2017--2021.",
                "QCS selectivity is time-varying with year blocks 2003--2010, 2011-2016, and 2017--2021")),
       c(ifelse(fr(),
                "Indices basés sur la conception remplacés par des indices basés sur la géostatistique pour toutes les enquêtes",
                "Design-based indices replaced with Geostatistical-based indices for all surveys")),
       c(ifelse(fr(),
                "Retirer Rejeter CPUE",
                "Remove Discard CPUE index"),
         ifelse(fr(),
                "La maturité n'inclut pas le 'développement/repos'",
                "Maturity codes 2, 3, and 7 removed from the maturity-at-age estimation"),
         ifelse(fr(),
                "Fixer la sélectivité DCPUE à l'estimation Shoreside ($a_{k,F} = 8.67; \\gamma_{k,F} = 1.06; a_{k,M} = 8.40; \\gamma_{k,F} = 0.96$)",
                "Fix DCPUE selectivity to the Shoreside estimate ($a_{k,F} = 8.67; \\gamma_{k,F} = 1.06; a_{k,M} = 8.40; \\gamma_{k,F} = 0.96$)"),
         ifelse(fr(),
                "Correction des sélectivités de l'enquête pour l'estimation de Shorside ($a_{k,F} = 8.67; \\gamma_{k,F} = 1.06; a_{k,M} = 8.40; \\gamma_{k,F} = 0.96$)",
                "Fix all survey selectivities to the Shorside estimate ($a_{k,F} = 8.67; \\gamma_{k,F} = 1.06; a_{k,M} = 8.40; \\gamma_{k,F} = 0.96$)")))

# This is a list of vectors of retrospective groups (retrospective models that
# will be plotted against each other). It can be `NULL` if to be ignored.
# The base model will be prepended to each group later in set_dirs() so that
# it is first on the plots for each group.
retro_models_dirs <-
  list(c("01-retrospective-1-years",
         "02-retrospective-2-years",
         "03-retrospective-3-years",
         "04-retrospective-4-years",
         "05-retrospective-5-years",
         "06-retrospective-6-years",
         "07-retrospective-7-years",
         "08-retrospective-8-years"))
retro_models_text <-
  list(c(ifelse(fr(),
                "Modèle de base moins 1 an de données",
                "Base model - 1 year"),
         ifelse(fr(),
                "Modèle de base moins 2 ans de données",
                "Base model - 2 years"),
         ifelse(fr(),
                "Modèle de base moins 3 ans de données",
                "Base model - 3 years"),
         ifelse(fr(),
                "Modèle de base moins 4 ans de données",
                "Base model - 4 years"),
         ifelse(fr(),
                "Modèle de base moins 5 ans de données",
                "Base model - 5 years"),
         ifelse(fr(),
                "Modèle de base moins 6 ans de données",
                "Base model - 6 years"),
         ifelse(fr(),
                "Modèle de base moins 7 ans de données",
                "Base model - 7 years"),
         ifelse(fr(),
                "Modèle de base moins 8 ans de données",
                "Base model - 8 years")))

# Add base model text to each retrospective group
retro_models_text <- map(retro_models_text, ~{c("Base model", .x)})
# Make these factors so that they can be reordered in the legends later
retro_models_text <- retro_models_text %>% map(~{factor(.x, levels = .x)})

# if(here() == "/home/rstudio"){
#   # For an Rstudio server spawned inside a Docker container
#   drs <- set_dirs(models_dir = models_dir,
#                   nongit_dir = nongit_dir,
#                   base_model_dir = "base",
#                   bridge_models_dirs = bridge_models_dirs,
#                   sens_models_dirs = sens_models_dirs,
#                   check_dir_exists = FALSE)
# }else{
drs <- set_dirs(models_dir = models_dir,
                nongit_dir = nongit_dir,
                bridge_models_dirs = bridge_models_dirs,
                sens_models_dirs = sens_models_dirs,
                retro_models_dirs = retro_models_dirs)
# }

models <- model_setup(drs = drs,
                      bridge_models_text = bridge_models_text,
                      sens_models_text = sens_models_text,
                      retro_models_text = retro_models_text,
                      overwrite_rds_files = build_rds)
