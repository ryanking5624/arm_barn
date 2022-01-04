setwd("~/GitHub/arm_barn")
source("header.R")

tran <- jsonlite::read_json("import/transactions2019.json")$transaction_all$queryResults$row

tran_stack <- bind_rows(tran %>%
                          unlist()) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  setNames(c("col_name", "value")) %>%
  mutate(col_name = str_remove_all(col_name, "\\..*")) %>%
  rownames_to_column(var = "r") %>%
  mutate(r = ceiling(as.numeric(r)/23)) %>%
  pivot_wider(id_cols = r,
              names_from = col_name,
              values_from = value) %>%
  mutate_at(.vars = vars(trans_date, resolution_date, effective_date),
            .funs = ~ as_date(str_sub(.x, end = 10))) %>%
  select(-r)

write_rds(tran_stack, "export/a/a02a.rds")

