#' NEJM Diet Study Dataset
#'
#' Lipid panel data from a diet study with 36 subjects across 3 diet groups
#' (AAD, Mono, Step1). Contains baseline and study values for total cholesterol,
#' triglycerides, and HDL cholesterol, plus computed changes.
#'
#' @format A data frame with 36 rows and 13 variables:
#' \describe{
#'   \item{sex}{Factor: subject sex (F/M)}
#'   \item{tcpre}{Numeric: baseline total cholesterol}
#'   \item{tgpre}{Numeric: baseline triglycerides}
#'   \item{hcpre}{Numeric: baseline HDL cholesterol}
#'   \item{Diet}{Factor: diet group (AAD, Mono, Step1)}
#'   \item{tcstudy}{Numeric: study total cholesterol}
#'   \item{tgstudy}{Numeric: study triglycerides}
#'   \item{hcstudy}{Numeric: study HDL cholesterol}
#'   \item{tcchange}{Numeric: change in total cholesterol}
#'   \item{tgchange}{Numeric: change in triglycerides}
#'   \item{hcchange}{Numeric: change in HDL cholesterol}
#'   \item{badtcp}{Numeric: intentionally bad baseline TC values}
#'   \item{diffvar}{Numeric: variable for testing}
#' }
"NEJM"

#' Metabolic Syndrome Dataset
#'
#' Metabolic panel data from 85 subjects with lipids, glucose, insulin,
#' and metabolic syndrome classification.
#'
#' @format A data frame with 85 rows and 14 variables:
#' \describe{
#'   \item{Sex}{Factor: subject sex (F/M)}
#'   \item{TC}{Numeric: total cholesterol}
#'   \item{TG}{Numeric: triglycerides}
#'   \item{HDL}{Numeric: HDL cholesterol}
#'   \item{LDL}{Numeric: LDL cholesterol}
#'   \item{LN_TG}{Numeric: log triglycerides}
#'   \item{BMI}{Numeric: body mass index}
#'   \item{GLUC}{Numeric: glucose}
#'   \item{INS}{Numeric: insulin}
#'   \item{Lpa}{Numeric: lipoprotein(a)}
#'   \item{FIBR}{Numeric: fibrinogen}
#'   \item{WTCAT}{Factor: weight category (lean, overwt, obese)}
#'   \item{feel}{Factor: how subject feels (bad, ok, good)}
#'   \item{MetSyn}{Factor: metabolic syndrome (Yes/No)}
#' }
"Met"

#' Delta Diet Repeated Measures Dataset
#'
#' Lipid panel data from 103 subjects measured under 3 diet conditions
#' (AAD, Step1, LowSat) for repeated measures analysis.
#'
#' @format A data frame with 103 rows and 14 variables:
#' \describe{
#'   \item{sex}{Factor: subject sex (F/M)}
#'   \item{age}{Factor: age group (old/young)}
#'   \item{TC.AAD, TC.Step1, TC.LowSat}{Numeric: total cholesterol by diet}
#'   \item{HC.AAD, HC.Step1, HC.LowSat}{Numeric: HDL cholesterol by diet}
#'   \item{TG.AAD, TG.Step1, TG.LowSat}{Numeric: triglycerides by diet}
#'   \item{like.AAD, like.Step1, like.LowSat}{Factor: diet preference}
#' }
"delta"

#' HepC Survival Dataset
#'
#' Survival data from 88 hepatitis C patients comparing treatment groups.
#'
#' @format A data frame with 88 rows and 5 variables:
#' \describe{
#'   \item{time}{Numeric: follow-up time}
#'   \item{status}{Numeric: event status (1=death, 0=censored)}
#'   \item{Treatment}{Factor: treatment group (Control/Prednisolone)}
#'   \item{dead}{Factor: death indicator (yes/0)}
#'   \item{Treat3}{Factor: 3-level treatment (Control/Drugtwo/Prednisolone)}
#' }
"hepc"

#' Cox Modeling Dataset
#'
#' Time-to-event data from 194 subjects for Cox proportional hazards modeling.
#'
#' @format A data frame with 194 rows and 6 variables:
#' \describe{
#'   \item{TimeToEvent}{Numeric: time to event}
#'   \item{Outcome}{Numeric: event indicator (1=event, 0=censored)}
#'   \item{C.Index}{Numeric: C-index grouping variable}
#'   \item{BNP}{Numeric: brain natriuretic peptide}
#'   \item{LVEF}{Numeric: left ventricular ejection fraction}
#'   \item{gender}{Factor: subject gender (F/M)}
#' }
"coxdata"

#' Volcano Plot Dataset
#'
#' Pre-computed fold-change and p-value data for 918 variables,
#' used to generate volcano plots with \code{cuvolc}.
#'
#' @format A data frame with 918 rows and 3 variables:
#' \describe{
#'   \item{varnam}{Character: variable/protein name}
#'   \item{fold}{Numeric: fold change}
#'   \item{pval}{Numeric: p-value}
#' }
"volc"

#' Volcano Omics Summary Dataset
#'
#' Summary statistics from an omics analysis comparing lean, obese, and
#' overweight groups across 10 metabolic variables. Contains group means,
#' spread (SD or IQR), and pairwise p-values.
#'
#' @format A data frame with 10 rows and 10 variables:
#' \describe{
#'   \item{Variable}{Factor: variable name (TC, TG, HDL, etc.)}
#'   \item{lean_M}{Numeric: mean for lean group}
#'   \item{lean_sd/iqr}{Character: SD or IQR for lean group}
#'   \item{obese_M}{Numeric: mean for obese group}
#'   \item{obese_sd/iqr}{Character: SD or IQR for obese group}
#'   \item{overwt_M}{Numeric: mean for overweight group}
#'   \item{overwt_sd/iqr}{Character: SD or IQR for overweight group}
#'   \item{leanvsobese}{Numeric: p-value lean vs obese}
#'   \item{leanvsoverwt}{Numeric: p-value lean vs overweight}
#'   \item{obesevsoverwt}{Numeric: p-value obese vs overweight}
#' }
"volcomic"
