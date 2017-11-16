# load libraries
library(ggplot2)  # plotting
library(dplyr)    # data manipulation
library(readr)    # reading clean tab files

# function input
read_site_info <- function(exp_site = "208-1264",
                           ext = "*.txt$",
                           summary_file = "coresumm.csv",
                           basedir = "data/") {

  dir <- paste(basedir, exp_site, sep = "/")

  # TODO: deal with unavailable files correctly

  # read the files
  cor <-
    list.files(paste(dir, "cores", sep = "/"), ext, full.names = TRUE) %>%
    lapply(FUN = function(i) {read_tsv(i, skip = 2)}) %>%
    bind_rows() %>%
    mutate(type = "cor") %>%
    mutate(top = TOP_DEPTH + MCD_OFFSET) %>%
    mutate(bot = top + DRILLED_LENGTH) %>%
    mutate(mid = top + (bot - top) / 2)

  sec <-
    list.files(paste(dir, "sections", sep = "/"), ext, full.names = TRUE) %>%
    lapply(FUN = function(i) {read_tsv(i, skip = 2)}) %>%
    bind_rows() %>%
    mutate(type = "sec") %>%
    mutate(top = MCD_TOP) %>%
    mutate(bot = top + REV_LENGTH) %>%
    mutate(mid = top + (bot - top) / 2)

  smp <-
    list.files(paste(dir, "samples", sep = "/"), ext, full.names = TRUE) %>%
    lapply(FUN = function(i) {read_tsv(i, skip = 2)}) %>%
    bind_rows() %>%
    mutate(type = "smp") %>%
    rename(top = MCD_TOP) %>%
    mutate(bot = top + .01 * (BOTTOM_DEPTH - TOP_DEPTH)) %>%
    mutate(mid = top + (bot - top) / 2)

  smy <-
    read_csv(paste(dir, summary_file, sep = "/")) %>%
    filter(!is.na(Comment)) %>%
    rename(EXPEDITION = "Leg", SITE = "Site", HOLE = "H", CORE = "Cor",
        CORE_TYPE = "T", SECTION_LABEL = "Sc", INIT_LENGTH = "LL(m)",
        REV_LENGTH = "CL(m)", TOP_DEPTH = "Top(mbsf)") %>%
    mutate(type = "sum") %>%
    left_join(sec %>% select(TOP_DEPTH, top, bot, mid))

  # stretches of core that are longer than 15 cm and have not been sampled yet
  smp_left <- smp %>%
    mutate(type = "smp_left") %>%
    mutate(diff = c(top[-1], NA) - bot) %>%
    filter(diff >= 0.01)

  # bind_rows(cor, sec, smp, smy, smp_left)
  full_join(cor, sec) %>%
    full_join(smp) %>%
    full_join(smy) %>%
    full_join(smp_left)
}

plot_site_info <- function(x, width = c(cor = .45, sec = .3, sum = .05,
  smp = .1, smp_left = .1)) {
  # plotting range defaults
  ranges <- tibble(
      id = c("cor", "sec", "sum", "smp", "smp_left"),
      width = width,
      max = cumsum(width),
      min = max - width,
      mid = min + .5 * width)
  # set up the plot
  p <- ggplot() +
    facet_grid(~ HOLE) +
    ylab("Depth (mbsf)") +
    scale_y_reverse(breaks = seq(min(x$top), max(x$bot), 10)) +
    scale_x_continuous("Info", breaks = ranges$mid,
      labels = c("Core", "Section", "Annotation", "Sample taken", "Samples available")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.1),
      panel.grid = element_blank())

  # plot core rectangles
  p <- p + geom_rect(aes(xmin = ranges %>% filter(id == "cor") %>% pull(min),
    xmax = ranges %>% filter(id == "cor") %>% pull(max),
    ymin = bot, ymax = top, fill = as.factor(CORE)),
    col = "black", data = x %>% filter(type == "cor"),
    show.legend = FALSE) +
    # plot core text
    geom_text(aes(ranges %>% filter(id == "cor") %>% pull(mid),
      mid, label = CORE), data = x %>% filter(type == "cor"))

  # plot section rectangles
  p <- p + geom_rect(aes(xmin = ranges %>% filter(id == "sec") %>% pull(min),
    xmax = ranges %>% filter(id == "sec") %>% pull(max),
    ymin = bot, ymax = top, fill = as.factor(SECTION_LABEL)),
    col = "black", data = x %>% filter(type == "sec"),
    show.legend = FALSE) +
    # plot section text
    geom_text(aes(ranges %>% filter(id == "sec") %>% pull(mid),
      mid, label = SECTION_LABEL), size = 2, data = x %>% filter(type == "sec"))

  # plot sample rectangle
  p <- p + geom_rect(aes(xmin = ranges %>% filter(id == "smp") %>% pull(min),
      xmax = ranges %>% filter(id == "smp") %>% pull(max),
      ymin = bot, ymax = top), data = x %>% filter(type == "smp"))

  # plot sample unavailable
  p <- p + geom_rect(aes(xmin = ranges %>% filter(id == "sum") %>% pull(min),
      xmax = ranges %>% filter(id == "sum") %>% pull(max),
      ymin = bot, ymax = top), data = x %>% filter(type == "sum"),
      fill = "black", alpha = 0.5) +
    # unavailable comment
    geom_text(aes(ranges %>% filter(id == "sum") %>% pull(mid),
      mid, label = Comment), size = 1, data = x %>% filter(type == "sum"),
      hjust = 0)

  # plot sample_left rectangle
  p <- p + geom_rect(aes(xmin = ranges %>% filter(id == "smp_left") %>% pull(min),
    xmax = ranges %>% filter(id == "smp_left") %>% pull(max),
    ymin = bot, ymax = bot + diff), data = x %>% filter(type == "smp_left"),
    fill = "red", alpha = .5)
  p
}
