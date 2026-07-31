library(emuR)
library(tidyverse)
library(lme4)
library(emmeans)
library(sjPlot)

db <- load_emuDB('final_emuDB')

sl <- query(db, 'landmarks == clo')
sl$end <- query(db, 'landmarks == offset')$start
sl$sample_end <- query(db, 'landmarks == offset')$sample_start
sl$rel <- query(db, 'landmarks == rel')$start

sl <- sl %>%
  mutate(bndl_info = str_sub(bundle, start=4)) %>%
  separate(bndl_info, into=c(
    'foc_item', 'foc_voi', 'foc_poa', 'foc_vheight', 'foc_st',
    'nfoc_item', 'nfoc_voi', 'nfoc_poa', 'nfoc_vheight', 'nfoc_st'))

sl$ord <- rep(NA, nrow(sl))
for (i in 2:nrow(sl)) {
  bndl <- sl$bundle[i]
  n_item <- nrow(filter(sl, bundle==bndl))
  if (n_item > 1) {
    if (sl$bundle[i-1] == sl$bundle[i]) {
      sl$ord[i] <- 2
    } else {
      sl$ord[i] <- 1
    }
  }
}

sl <- sl %>%
  mutate(cond = case_when(
    foc_item == 'NA' ~ 'nfoc',
    nfoc_item == 'NA' ~ 'foc',
    ord == 1 ~ 'foc',
    ord == 2 ~ 'nfoc'),
    item = case_when(
      foc_item == 'NA' ~ nfoc_item,
      nfoc_item == 'NA' ~ foc_item,
      ord == 1 ~ foc_item,
      ord == 2 ~ nfoc_item),
    voi = case_when(
      foc_item == 'NA' ~ nfoc_voi,
      nfoc_item == 'NA' ~ foc_voi,
      ord == 1 ~ foc_voi,
      ord == 2 ~ nfoc_voi),
    poa = case_when(
      foc_item == 'NA' ~ nfoc_poa,
      nfoc_item == 'NA' ~ foc_poa,
      ord == 1 ~ foc_poa,
      ord == 2 ~ nfoc_poa),
    vheight = case_when(
      foc_item == 'NA' ~ nfoc_vheight,
      nfoc_item == 'NA' ~ foc_vheight,
      ord == 1 ~ foc_vheight,
      ord == 2 ~ nfoc_vheight),
    st = case_when(
      foc_item == 'NA' ~ nfoc_st,
      nfoc_item == 'NA' ~ foc_st,
      ord == 1 ~ foc_st,
      ord == 2 ~ nfoc_st)
  ) %>%
  select(-c(nfoc_item, foc_item, nfoc_voi, foc_voi, nfoc_poa, foc_poa,
            nfoc_vheight, foc_vheight, nfoc_st, foc_st, ord))

sl_vo <- query(db, 'landmarks == vo')

sl_nas <- sl %>% filter(voi == 'nasal') %>%
  mutate(vo = 0)
sl_stop <- sl %>% filter(voi != 'nasal') %>%
  mutate(vo = sl_vo$start)
sl <- rbind(sl_nas, sl_stop)

sl <- sl %>% mutate(
  vdur = ifelse(voi == 'nasal', end - rel, end - vo),
  opdur = end - rel,
  vot = ifelse(voi == 'nasal', NA, vo - rel),
  cldur = rel - start,
  syldur = end - start
)

meta <- read.delim('data/meta.csv', sep=';')
meta$session <- paste0('_', meta$session)
sl <- left_join(sl, meta, by = 'session')

alt_sl <- sl %>% mutate(start = ifelse(vo == 0, start, vo))

alt_sl <- sl %>% mutate(start = ifelse(vo == 0, rel, vo))

makeTheMeans <- function(dat) {
  dat <- dat %>% group_by(session) %>%
    mutate(upp = mean(T1, na.rm=T) + 3*sd(T1, na.rm=T),
           low = mean(T1, na.rm=T) - 3*sd(T1, na.rm=T),
           T1 = ifelse(T1 > upp | T1 < low, NA, T1),
           myMeasure = as.numeric(scale(T1))) %>%
    ungroup()
  means <- dat %>% group_by(sl_rowIdx, variety, cond, st, session) %>%
    summarize(m = mean(myMeasure, na.rm = T))
}

### for f0

dat <- get_trackdata(db, alt_sl, ssffTrackName='f0')
dat[which(dat$T1 == 0),'T1'] <- NA

exF0_nst <- dat %>% filter(bundle == '007maler-nasal-bilab-low-nst-NA-NA-NA-NA-NA')
exF0_st <- dat %>% filter(bundle == '007maler-nasal-bilab-low-st-NA-NA-NA-NA-NA')

dat <- dat %>% group_by(session) %>%
  mutate(upp = mean(T1, na.rm=T) + 3*sd(T1, na.rm=T),
         low = mean(T1, na.rm=T) - 3*sd(T1, na.rm=T),
         T1 = ifelse(T1 > upp | T1 < low, NA, T1),
         myMeasure = as.numeric(scale(T1))) %>%
  ungroup()

means1 <- dat %>% filter(times_norm < 0.5) %>%
  group_by(sl_rowIdx, variety, cond, st, session) %>%
  summarize(m = mean(myMeasure, na.rm = T))
means2 <- dat %>% filter(times_norm > 0.5) %>%
  group_by(sl_rowIdx, variety, cond, st, session) %>%
  summarize(m = mean(myMeasure, na.rm = T))
means <- means1
means$m <- means1$m - means2$m

speakerMeansPlot <- function(means, ylab, legpos) {
  means[means$cond == 'nfoc','cond'] <- 'Ytringsfinal'
  means[means$cond == 'foc','cond'] <- 'Ytringsmedial'

  means[means$variety == 'j','variety'] <- 'Østjysk'
  means[means$variety == 'z','variety'] <- 'Københavnsk'

  means$cond <- as.factor(means$cond)
  means$cond <- relevel(means$cond, 'Ytringsmedial')

  means %>%
    group_by(session, cond, st, variety) %>%
    summarize(m = mean(m, na.rm=T)) %>%
    mutate(session = paste0(variety, session)) %>%
    ggplot +
    aes(x = session, y = m, col = st, shape = variety) +
    geom_point() +
    facet_grid(~cond) +
    theme(panel.background = element_rect(fill = NA),
          panel.border = element_rect(color = 'black', fill=NA),
          panel.grid.major = element_line(color='grey90'),
          panel.grid.minor = element_line(color='grey95'),
          legend.position = legpos,
          text = element_text(size = 12),
          legend.box = 'vertical',
          axis.title.y = ggtext::element_markdown(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.spacing.x = unit(10,'mm')) +
    ylab(ylab) +
    xlab('Taler') +
    ggtitle('') +
    scale_color_manual(values = c('black', 'darkorange'),
                       name = 'Stød',
                       labels = c('Ikke-stød', 'Stød')) +
    scale_shape_manual(values = c('circle', 'square'),
                       name = 'Lekt')

}

speakerMeansPlot(means, 'ΔF0 (norm.)', 'none') -> speakerMeansF0

makeTheModel <- function(means) {
  means[means$cond == 'nfoc','cond'] <- 'Ytringsfinal'
  means[means$cond == 'foc','cond'] <- 'Ytringsmedial'

  means[means$variety == 'j','variety'] <- 'Østjysk'
  means[means$variety == 'z','variety'] <- 'Københavnsk'

  means$variety <- as.factor(means$variety)
  means$cond <- as.factor(means$cond)
  means$cond <- relevel(means$cond, 'Ytringsmedial')
  means$st <- as.factor(means$st)
  means$session <- as.factor(means$session)

  lmer(m ~ variety*st*cond + (cond*st|session),
       data = means) -> mod
  return(mod)
}

modF0 <- makeTheModel(means)

summary(modF0)
emmeans(modF0, ~ st | variety | cond,
        lmerTest.limit = 5000, pbkrtest.limit = 5000) |>
  pairs()

### for resid h1

dat <- get_trackdata(db, alt_sl, ssffTrackName='H1c')
dat[which(dat$T1 == 0),'T1'] <- NA

dat$rms <- get_trackdata(db, alt_sl, 'intensity')$T1
mod <- lm(T1 ~ rms, data = dat)

dat <- dat %>% filter(!is.na(T1))
dat$T1 <- resid(mod)

exrH1_nst <- dat %>% filter(bundle == '007maler-nasal-bilab-low-nst-NA-NA-NA-NA-NA')
exrH1_st <- dat %>% filter(bundle == '007maler-nasal-bilab-low-st-NA-NA-NA-NA-NA')

means <- makeTheMeans(dat)

speakerMeansPlot(means, 'Residual H1* (norm.)', 'none') -> speakerMeansRH1

modResH1 <- makeTheModel(means)

summary(modResH1)
emmeans(modResH1, ~ st | variety | cond,
        lmerTest.limit = 5000, pbkrtest.limit = 5000) |>
  pairs()

### for HNR

dat <- get_trackdata(db, alt_sl, ssffTrackName='hnr')
dat[which(dat$T1 == 0),'T1'] <- NA

exHNR_nst <- dat %>% filter(bundle == '007maler-nasal-bilab-low-nst-NA-NA-NA-NA-NA')
exHNR_st <- dat %>% filter(bundle == '007maler-nasal-bilab-low-st-NA-NA-NA-NA-NA')

means <- makeTheMeans(dat)

speakerMeansPlot(means, 'HNR<500 (norm.)', 'none') -> speakerMeansHNR

modHNR <- makeTheModel(means)

summary(modHNR)
emmeans(modHNR, ~ st | variety | cond,
        lmerTest.limit = 5000, pbkrtest.limit = 5000) |>
  pairs()

### for SoE

dat <- get_trackdata(db, alt_sl, ssffTrackName='soe')
dat[which(dat$T1 == 0),'T1'] <- NA

exSOE_nst <- dat %>% filter(bundle == '007maler-nasal-bilab-low-nst-NA-NA-NA-NA-NA')
exSOE_st <- dat %>% filter(bundle == '007maler-nasal-bilab-low-st-NA-NA-NA-NA-NA')

means <- makeTheMeans(dat)

speakerMeansPlot(means, 'SoE (norm.)', 'none') -> speakerMeansSOE
speakerMeansPlot(means, '', 'bottom') -> speakerMeansLegend

modSoE <- makeTheModel(means)

summary(modSoE)
emmeans(modSoE, ~ st | variety | cond,
        lmerTest.limit = 5000, pbkrtest.limit = 5000) |>
  pairs()

###

# library(sjPlot)

makeThePlot <- function(mod, ylab, legpos) {
  plot_model(mod, type = 'int', terms = 4) -> pm
  pm[[4]] +
    theme(panel.background = element_rect(fill = NA),
          panel.border = element_rect(color = 'black', fill=NA),
          panel.grid.major = element_line(color='grey90'),
          panel.grid.minor = element_line(color='grey95'),
          legend.position = legpos,
          text = element_text(size = 12),
          legend.box = 'vertical',
          axis.title.y = ggtext::element_markdown(),
          panel.spacing.x = unit(10,'mm')) +
    ylab(ylab) +
    xlab('Lekt') +
    ggtitle('') +
    scale_color_manual(values = c('black', 'darkorange'),
                       name = 'Stød',
                       labels = c('Ikke-stød', 'Stød'))
}

plotF0 <- makeThePlot(modF0, 'ΔF0 (norm.)', 'none')
plotResH1 <- makeThePlot(modResH1, 'Residual H1* (norm.)', 'none')
plotHNR <- makeThePlot(modHNR, 'HNR<500 (norm.)', 'none')
plotSoE <- makeThePlot(modSoE, 'SoE (norm.)', 'none')
plotLegend <- makeThePlot(modF0, '', 'bottom')

get_only_legend <- function(plot) {
  plot_table <- ggplot_gtable(ggplot_build(plot))
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")
  legend <- plot_table$grobs[[legend_plot]]
  return(legend)
}
legend <- get_only_legend(plotLegend)

library(gridExtra)
grid.arrange(plotF0, plotResH1, plotHNR, plotSoE, legend, ncol = 2, nrow = 3,
             layout_matrix = rbind(c(1, 2), c(3, 4), c(5, 5)),
             heights = c(10,10,1))

png('DaTaRes2.png', width = 19, height = 15, units = 'cm', res = 300)

###

legend <- get_only_legend(speakerMeansLegend)
grid.arrange(speakerMeansF0, speakerMeansRH1, speakerMeansHNR, speakerMeansSOE,
             legend, ncol = 2, nrow = 3,
             layout_matrix = rbind(c(1, 2), c(3, 4), c(5, 5)),
             heights = c(10,10,3))

png('DaTaSpeakerMeans.png', width = 19, height = 15, units = 'cm', res = 300)



###

library(praatutils)

fn_st <- paste0(db$basePath, '/', '__0007_ses', '/',
                '007maler-nasal-bilab-low-st-NA-NA-NA-NA-NA_bndl/',
                '007maler-nasal-bilab-low-st-NA-NA-NA-NA-NA.wav')
fn_nst <- paste0(db$basePath, '/', '__0007_ses', '/',
                '007maler-nasal-bilab-low-nst-NA-NA-NA-NA-NA_bndl/',
                '007maler-nasal-bilab-low-nst-NA-NA-NA-NA-NA.wav')

exOSCI_nst <- readSound(fn_nst, 2.124, 2.1)
exOSCI_st <- readSound(fn_st, 2.124, 2.419)

graphics::par(mar = c(0,2,0,2), mfrow = c(5,1), oma = c(5,5,5,5))
plot(exOSCI_st$t - exOSCI_st$start,
      exOSCI_st$signal - min(exOSCI_st$signal),
      col = 'darkorange',
     type = 'l', ylim = c(-1.3, 1.3),
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
lines(exOSCI_nst$t - exOSCI_nst$start,
     exOSCI_nst$signal - max(exOSCI_nst$signal))
plot(exF0_st$times_rel / 1000, exF0_st$T1, type = 'l', col = 'darkorange',
     ylim = c(180,260), xlab = '', xaxt = 'n')
lines(exF0_nst$times_rel / 1000, exF0_nst$T1)
graphics::mtext('F0 (Hz)', side=2, line=3.5, cex=0.8)
plot(exrH1_st$times_rel / 1000, exrH1_st$T1, type = 'l', col = 'darkorange',
     ylim = c(-1,7), ylab = 'rH1* (dB)', xlab = '', xaxt = 'n')
lines(exrH1_nst$times_rel / 1000, exrH1_nst$T1)
graphics::mtext('rH1* (dB)', side=2, line=3.5, cex=0.8)
plot(exHNR_st$times_rel / 1000, exHNR_st$T1, type = 'l', col = 'darkorange',
     ylim = c(-5,45), ylab = 'HNR<500 (dB)', xlab = '', xaxt = 'n')
lines(exHNR_nst$times_rel / 1000, exHNR_nst$T1)
graphics::mtext('HNR<500 (dB)', side=2, line=3.5, cex=0.8)
plot(exSOE_st$times_rel / 1000, exSOE_st$T1, type = 'l', col = 'darkorange',
     ylim = c(0.015,0.05), ylab = 'SoE (Pa)', xlab = 'Tid (s)')
lines(exSOE_nst$times_rel / 1000, exSOE_nst$T1)
graphics::mtext('SoE (Pa)', side=2, line=3.5, cex=0.8)
graphics::mtext('Tid (s)', side=1, line=3, outer=T, cex=0.8)

png('DaTaVQ.png', width = 19, height = 18, units = 'cm', res = 300)


###

library(tuneR)

sr <- 16000

soe_est <- function(sndFile, start, end, f0vals) {
  snd <- readWave(sndFile, from = start, to = end, units = 'seconds')
  snd <- downsample(snd, sr)
  sig <- diff(snd@left, 1)

  meanF0 <- mean(sr/f0vals, na.rm=T)
  n0 <- round(meanF0/1.5)
  wid <- 2*n0+1
  len <- length(sig)

  zfr1_filt <- signal::filter(1, c(1, -2*0.999, 0.999^2), sig)
  a <- signal::filter(rep(1, wid)/wid, 1, zfr1_filt)
  abegin <- cumsum(sig[1:(wid-2)])
  abegin <- abegin[seq(1, length(abegin), by=2)] / seq(1, wid-2, by=2)
  aend <- cumsum(sig[len:(len-wid+3)])
  aend <- aend[seq(length(aend), 1, by=-2)] / seq(wid-2, 1, by=-2)
  a <- c(abegin, a[wid:length(a)], aend)
  zfr1_trendRem <- zfr1_filt - a

  zfr2_filt <- signal::filter(1, c(1, -2*0.999, 0.999^2), zfr1_trendRem)
  a <- signal::filter(rep(1, wid)/wid, 1, zfr2_filt)
  abegin <- cumsum(zfr2_filt[1:(wid-2)])
  abegin <- abegin[seq(1, length(abegin), by=2)] / seq(1, wid-2, by=2)
  aend <- cumsum(zfr2_filt[len:(len-wid+3)])
  aend <- aend[seq(length(aend), 1, by=-2)] / seq(wid-2, 1, by=-2)
  a <- c(abegin, a[wid:length(a)], aend)
  zfr_out <- zfr2_filt - a

  z <- 0.95*zfr_out[1:(length(zfr_out)-wid)]/
    max(abs(zfr_out[1:(length(zfr_out)-wid)]))

  z1 <- c(NA, z[1:length(z)-1])
  tf <- z1 > 0 & z<=0

  pulses <- which(tf)
  pitch <- data.frame(
    pulses = pulses / sr,
    diff = c(NA, pulses[-1] / sr - pulses[-length(pulses)] / sr))

  zfr1_trendRem[1:60] <- NA
  zfr1_trendRem[(length(zfr1_trendRem)-60):length(zfr1_trendRem)] <- NA

  zfr_out[1:60] <- NA
  zfr_out[(length(zfr_out)-60):length(zfr_out)] <- NA


  return(list(snd@left, sig, zfr1_trendRem, zfr_out, pulses / sr))
}

f0vals_st <- exF0_st$T1
f0vals_st[which(f0vals_st==0)] <- NA

f0vals_nst <- exF0_nst$T1
f0vals_nst[which(f0vals_nst==0)] <- NA

soeVals_st <- soe_est(fn_st, 2.124, 2.419, f0vals_st)
soeVals_nst <- soe_est(fn_nst, 1.81, 2.1, f0vals_nst)

timeDomain <- 1:length(soeVals_st[[1]]) / sr

graphics::par(mar = c(0,2,0,2), mfrow = c(5,1), oma = c(5,5,5,5))

plot(soeVals_st[[1]] - min(soeVals_st[[1]]),
     type = 'l', xaxt = 'n', yaxt = 'n', col = 'orange',
     ylim = c(min(soeVals_nst[[1]] - max(soeVals_nst[[1]])),
              max(soeVals_st[[1]] - min(soeVals_st[[1]]))))
lines(soeVals_nst[[1]] - max(soeVals_nst[[1]]))
graphics::mtext('Oprindeligt\nsignal', side=2, line=3.5, cex=0.8)
plot(soeVals_st[[2]] - min(soeVals_st[[2]]),
     type = 'l', xaxt = 'n', yaxt = 'n', col = 'orange',
     ylim = c(min(soeVals_nst[[2]] - max(soeVals_nst[[2]])),
              max(soeVals_st[[2]] - min(soeVals_st[[2]]))))
lines(soeVals_nst[[2]] - max(soeVals_nst[[2]]))
graphics::mtext('Førstederivat', side=2, line=3.5, cex=0.8)
plot(soeVals_st[[3]] - min(soeVals_st[[3]], na.rm=T),
     type = 'l', xaxt = 'n', yaxt = 'n', col = 'orange',
     ylim = c(min(soeVals_nst[[3]] - max(soeVals_nst[[3]], na.rm=T), na.rm=T),
              max(soeVals_st[[3]] - min(soeVals_st[[3]], na.rm=T), na.rm=T)))
lines(soeVals_nst[[3]] - max(soeVals_nst[[3]], na.rm=T))
graphics::mtext('Første\nfiltrering', side=2, line=3.5, cex=0.8)
plot(soeVals_st[[4]] - min(soeVals_st[[4]], na.rm=T),
     type = 'l', xaxt = 'n', yaxt = 'n', col = 'orange',
     ylim = c(min(soeVals_nst[[4]] - max(soeVals_nst[[4]], na.rm=T), na.rm=T),
              max(soeVals_st[[4]] - min(soeVals_st[[4]], na.rm=T), na.rm=T)))
lines(soeVals_nst[[4]] - max(soeVals_nst[[4]], na.rm=T))
graphics::mtext('Anden\nfiltrering', side=2, line=3.5, cex=0.8)
plot(exSOE_st$times_rel / 1000, exSOE_st$T1, type = 'l', col = 'darkorange',
     ylim = c(0.015,0.05), ylab = 'SoE (Pa)', xlab = 'Tid (s)')
lines(exSOE_nst$times_rel / 1000, exSOE_nst$T1)
graphics::mtext('SoE (Pa)', side=2, line=3.5, cex=0.8)
graphics::mtext('Tid (s)', side=1, line=3, outer=T, cex=0.8)

png('DaTaSoE.png', width = 19, height = 18, units = 'cm', res = 300)
