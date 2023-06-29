# Here we summarise the phylogenetic models we explored

library(tibble)

model_summary_tbl <-
  tribble(~"Model", ~"Tree",         ~"Subsitution",              ~"Clock",
          1,        "Pure birth",     "Brownian motion",            "Strict",
          2,        "Birth-death",    "Brownian motion",            "Strict",
          3,        "Birth-death",    "Brownian motion",           "Relaxed",
          4,        "Birth-death",    "Relaxed Brownian motion",   "Relaxed")


psm_files <- fs::dir_ls(here::here(),
                        glob = "*psm.csv",
                        recurse = TRUE)
psm_num <-
  purrr::map_chr(psm_files, ~read_csv(.x) %>% names()) %>%
  unname %>%
  parse_number()

ssm_files <- fs::dir_ls(here::here(),
                        glob = "*ssm.csv",
                        recurse = TRUE)
ssm_num <-
  purrr::map_chr(ssm_files, ~read_csv(.x) %>% names()) %>%
  unname %>%
  parse_number()

# Bayes factor is ratio of marginal likelihoods of the two models.
BF <- round(ssm_num[1] / ssm_num, 2)


model_summary_tbl_nums <-
model_summary_tbl %>%
  mutate(`Bayes Factor` = BF)

write_csv(model_summary_tbl_nums,
          here::here("analysis/data/derived_data/model_summary_tbl_nums.csv"))
