if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               mice,
               rio)

## Import ODI data------------------------------------------------------

# Week 5, 10, 26 ODI
df <- import ("../2020_LBPcausal/data/osw_dat.xlsx") %>%
  slice (-1) %>%
  arrange (`STOPS ID`)

# Week 52 ODI
df_52 <- import ("../2020_LBPcausal/data/osw_52.xlsx") %>%
  arrange (`Patient ID`)

dat_52 <- df_52 [, c(4:13)]
# Baseline ODI
dat <-  import ("../2020_LBPcausal/data/stops_dat.xlsx",
                sheet = "data")

dat <- dat[c(1:300),]


## Merge data------------------------------------------------------

# Extract groups
grp <- ifelse (dat$`TREATMENT GROUP` == "IndividualisedPhysio", "IndPT", "adv")

cnames <- names (dat)
cnames_mod <- str_replace_all (tolower(cnames),
                               pattern = " ",
                               repl = "_")
names (dat) <- cnames_mod
dat_sub <- dat[, !names (dat) %in% grep ("dummy|0,|v_", cnames_mod, value = TRUE)] %>%
  arrange (stops_id)
test <- dat_sub [, grepl ("osw", names (dat_sub))]
test <- test[, grepl ("q", names (test))]

# Combine ODIs
dat_base <- test %>% map_df (as.character)
dat_5 <- df[, c(3:12)]
dat_10 <- df[, c(16:25)]
dat_26 <- df[, c(29:38)]

colnames (dat_base) <-
  colnames (dat_5) <-
  colnames  (dat_10) <-
  colnames (dat_26) <-
  colnames (dat_52) <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10")

df_all <- data.frame(subj  = rep (df$`STOPS ID`, 5),
                     grp = rep (grp, 5),
                     time = rep (c(0, 5, 10, 26, 52), each = 300),
                     bind_rows (dat_base, dat_5, dat_10, dat_26, dat_52)) %>%
  mutate_at (vars(starts_with("Q")), factor, levels = c("0", "1", "2", "3", "4", "5"))


## Export merged data------------------------------------------------------
saveRDS(df_all, "output/dat_odi.RDS")
