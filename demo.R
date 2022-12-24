library(tidyverse)
library(survival)
library(ggsurvfit)


# Load data ---------------------------------------------------------------

OS <- jsonlite::read_json("OS.json")$datasetColl[[1]]$data
OS <- map_dfr(OS, ~ c(time = .$value[1], surv = .$value[2]))
OS <- OS %>%
  arrange(time) %>%
  mutate(dsurv = diff(c(1,surv)),
         event = as.numeric(abs(dsurv) > .01))

PFS <- jsonlite::read_json("PFS.json")$datasetColl[[1]]$data
PFS <- map_dfr(PFS, ~ c(time = .$value[1], surv = .$value[2]))
PFS <- PFS %>%
  arrange(time) %>%
  mutate(dsurv = diff(c(1,surv)),
         event = as.numeric(abs(dsurv) > .01))


# Check -------------------------------------------------------------------

osfit <- survfit(Surv(time, event) ~ 1, OS)

plot(osfit, conf.int = F, mark.time = T)
lines(OS$time, OS$surv, type = 's', col = 'red')
ggsurvfit(osfit) +
  add_risktable(risktable_stats = 'n.risk') +
  scale_x_continuous(breaks = seq(0,40,5))

pfsfit <- survfit(Surv(time, event) ~ 1, PFS)

plot(pfsfit, conf.int = F, mark.time = T)
lines(PFS$time, PFS$surv, type = 's', col = 'red')
ggsurvfit(pfsfit) +
  add_risktable(risktable_stats = 'n.risk') +
  scale_x_continuous(breaks = seq(0,40,5))


# Side by side ------------------------------------------------------------
plot(osfit, conf.int = F, mark.time = T, col = 2, lwd = 2)
lines(pfsfit, conf.int = F, mark.time = T, col = 3, lwd = 2)
abline(h = .5, lty = 2)
legend('topright', legend = c('OS', 'PFS'), lty = 1, col = c(2,3))


# Write data --------------------------------------------------------------

bind_rows(OS = OS, PFS = PFS, .id = "curve") %>%
  select(-dsurv) %>%
  write_csv("data.csv")
