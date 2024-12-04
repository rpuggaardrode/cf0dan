library(rPraat)
library(tuneR)
library(seewave)
library(soundgen)
library(tidyverse)
library(emuhelpeR)
library(emuR)
library(getVOT)

master <- read.csv('data/master.csv')

make_master_wav_tg <- function(sp) {
  data_loc <- paste0('../data_suppl/', sp, '/')
  wavs <- list.files(data_loc, pattern='*.wav')

  base_fn <- wavs[1] %>% str_sub(start=5, end=-5)
  snd <- readWave(paste0(data_loc, wavs[1])) %>% mono
  filt <- bandpass(snd, lwr=50, upr=10000, bw=c(0,100))
  snd@left <- round(filt, 0)
  dur <- length(snd@left)/snd@samp.rate
  t1 <- 0
  t2 <- dur
  ort <- master$sentence[which(master$filename==base_fn)]

  for (w in 2:length(wavs)) {
    base_fn <- wavs[w] %>% str_sub(start=5, end=-5)
    tmp <- readWave(paste0(data_loc, wavs[w])) %>% mono
    filt <- bandpass(tmp, lwr=50, upr=10000, bw=c(0,100))
    tmp@left <- round(filt, 0)
    dur <- length(tmp@left)/tmp@samp.rate
    t1 <- c(t1, tail(t2, n=1))
    t2 <- c(t2, tail(t1, n=1)+dur)
    ort <- c(ort, master$sentence[which(master$filename==base_fn)])
    snd <- tuneR::bind(snd, tmp)
  }

  clip <- which(snd@left > 32767)
  snd@left[clip] <- 32767

  tg <- tg.createNewTextGrid(0, (length(snd@left)/snd@samp.rate)+1)
  tg <- tg.insertNewIntervalTier(tg, newTierName='ort')
  for (l in 1:length(ort)) {
    tg <- tg.insertInterval(tg, 'ort', tStart=t1[l], tEnd=t2[l], label=ort[l])
  }

  writeWave(snd, paste0(sp, 'suppl.wav'))
  tg.write(tg, paste0(sp, 'suppl.TextGrid'), 'text')
}

make_master_wav_tg2 <- function(sp) {
  data_loc <- paste0('../Aarhus_data2/')
  wavs <- list.files(data_loc, pattern='*.wav')

  #base_fn <- wavs[1] %>% str_sub(start=5, end=-5)
  #snd <- readWave(paste0(data_loc, wavs[1])) %>% mono
  #filt <- bandpass(snd, lwr=50, upr=10000, bw=c(0,100))
  #snd@left <- round(filt, 0)
  #dur <- length(snd@left)/snd@samp.rate
  #t1 <- 0
  #t2 <- dur
  #ort <- master$sentence[which(master$filename==base_fn)]

  for (w in 1:length(wavs)) {
    #base_fn <- wavs[w] %>% str_sub(start=5, end=-5)
    snd <- readWave(paste0(data_loc, wavs[w])) %>% mono
    filt <- bandpass(snd, lwr=60, upr=10000, bw=c(0,100))
    snd@left <- round(filt, 0)
    #dur <- length(tmp@left)/tmp@samp.rate
    #t1 <- c(t1, tail(t2, n=1))
    #t2 <- c(t2, tail(t1, n=1)+dur)
    #ort <- c(ort, master$sentence[which(master$filename==base_fn)])
    #snd <- tuneR::bind(snd, tmp)
    clip <- which(snd@left > 32767)
    snd@left[clip] <- 32767
    writeWave(snd, paste0(wavs[w]))
  }


  #tg <- tg.createNewTextGrid(0, (length(snd@left)/snd@samp.rate)+1)
  #tg <- tg.insertNewIntervalTier(tg, newTierName='ort')
  #for (l in 1:length(ort)) {
  #  tg <- tg.insertInterval(tg, 'ort', tStart=t1[l], tEnd=t2[l], label=ort[l])
  #}

  #tg.write(tg, paste0(sp, 'suppl.TextGrid'), 'text')
}

#### Loop
d <- list.dirs('../data') %>% str_replace_all('../data/', '')
d <- d[-1]

make_master_wav_tg('0015')

for (sp in d) make_master_wav_tg(sp)

dir.create('tg_proc')

### NOTE: Still missing FA for 0009 and 0015 because of Autophon outage
#nonetheless can make f0 tracks and concat them

split_tg_wav <- function(sp) {
  sp_dir <- paste0('tg_proc/', sp)
  dir.create(sp_dir)
  tg <- tg.read(paste0('../Aarhus_data/', sp, '_edit.TextGrid'))
  len <- length(tg$trans$t1)
  t1 <- tg$trans$t1[-len]
  t2 <- tg$trans$t2[-len]
  sentences <- str_replace_all(master$sentence, '[?]', '') %>% tolower
  labs <- str_replace_all(tg$trans$label, '[?]', '')
  for (i in 1:(len-1)) {
    tg_sub <- tg.cut0(tg, t1[i], t2[i])
    id <- which(sentences==labs[i])
    base_fn <- master$filename[id]
    fn_tg <- paste0(sp, base_fn, '.TextGrid')
    fn_snd <- paste0(sp, base_fn, '.wav')
    tg.write(tg_sub, paste0(sp_dir, '/', fn_tg), 'text')
    snd <- readWave(paste0('../Aarhus_data/', sp, '_edit.wav'),
                    from=t1[i], to=t2[i], units='seconds')
    writeWave(snd, paste0(sp_dir, '/', fn_snd), extensible=FALSE)
  }
}

aarhus <- 31:46 %>% as.character %>% paste0('0', .)
for (s in aarhus) split_tg_wav(s)

### REAPER pipeline

call_reaper <- function(file, f0min=40, f0max=500, interval=0.005,
                        hilbert=FALSE, suppress_highpass_filter=FALSE,
                        unvoiced_cost=0.9, unvoiced_pulse_interval=0.01,
                        reaper_loc='reaper', need_wsl=FALSE, console=FALSE) {
  call <- paste(reaper_loc, '-i', file, '-x', f0max, '-m', f0min,
                '-u', unvoiced_pulse_interval, '-w', unvoiced_cost, '-f tmp -a')
  if (need_wsl) call <- paste('wsl', call)
  if (hilbert) call <- paste(call, '-t')
  if (suppress_highpass_filter) call <- paste(call, '-s')

  system(call, show.output.on.console=console)
  f0_file <- read_file('tmp')
  unlink('tmp')
  f0est <- unlist(strsplit(f0_file, 'EST_Header_End\\n'))[2]
  f0est <- suppressMessages(read_delim(f0est))
  colnames(f0est) <- c('time', 'voiced', 'f0')
  fn <- tail(strsplit(file, '/')[[1]], 1)
  f0est$file <- rep(fn, nrow(f0est))
  return(f0est)
}

bulk_reaper <- function(directory, ...) {
  wavs <- list.files(directory, pattern='*.wav')
  reaper_out <- call_reaper(paste0(directory, '/', wavs[1]), ...)
  for (f in wavs[-1]) {
    tmp <- call_reaper(paste0(directory, '/', f), ...)
    reaper_out <- rbind(reaper_out, tmp)
  }
  return(reaper_out)
}

reaper_dynamic_minmax <- function(...) {
  tmp <- bulk_reaper(f0min=60, f0max=500, ...)
  tmp$f0[which(tmp$f0 == -1)] <- NA
  q <- quantile(tmp$f0, probs=c(0.25, 0.75), na.rm=T, names=F)
  tmp_dyn <- bulk_reaper(f0min=0.75*q[1], f0max=1.5*q[2], ...)
  return(tmp_dyn)
}

#think I figured the problem. also add a col with speaker info

run_reaper_dynamic_minmax <- function(directory, verbose=TRUE, ...) {
  speakers <- list.dirs(directory, recursive=F)
  rdyn <- reaper_dynamic_minmax(directory=paste0(directory, '/', speakers[1]),
                                ...)
  sn <- tail(strsplit(speakers[1], '/')[[1]], 1)
  rdyn$session <- rep(sn, nrow(rdyn))
  for (s in speakers[-1]) {
    tmp <- reaper_dynamic_minmax(directory=paste0(directory, '/', s),
                                 ...)
    sn <- tail(strsplit(s, '/')[[1]], 1)
    tmp$session <- rep(sn, nrow(tmp))
    rdyn <- rbind(rdyn, tmp)
    if (verbose) print(paste(s, 'processed'))
  }
  return(rdyn)
}

# set wd as the data_processed folder

wavs <- list.files(pattern='*.wav', recursive=TRUE)

clean_audio <- function(file) {
  snd <- readWave(file) %>% mono
  filt <- bandpass(snd, lwr=50, upr=10000, bw=c(0,100))
  snd@left <- round(filt, 0)
  snd <- snd %>% normalize(unit='16')
  writeWave(snd, file, extensible=FALSE)
}

for (w in wavs) clean_audio(w)

#for some reason this wouldn't work when I had console=F
#have no clue why, a lot of what system does is a mystery to me...
reaper_out <- run_reaper_dynamic_minmax('.', need_wsl=T, console=T)

#load('reaper_out2.Rda')
#save(reaper_out, file='reaper_out2.Rda')

convert_TextGridCollection(dir='tg_proc', dbName='aarhus',
                           targetDir='../aarhus')

db <- load_emuDB('../aarhus/aarhus_emuDB')
serve(db, useViewer=FALSE)

#046due -- should be NA-taget
#046NAminus

load('reaper_out2.Rda')
reaper_out$file <- sapply(strsplit(reaper_out$file, '/'), tail, 1)
reaper_out$file <- str_replace_all(reaper_out$file, '.wav', '')
reaper_out$session <- sapply(strsplit(reaper_out$session, '/'), tail, 1)
reaper_out$session <- paste0('_', reaper_out$session)

reaper2ssff <- function(reaper_output, db_handle, session=FALSE) {

  ro <- as.data.frame(reaper_output)
  sr <- round(1 / (ro[[2,'time']] - ro[[1,'time']]), 0)

  dir.create(paste0(getwd(), '/ssff/'))

  if (!session) {
    sessions <- '0000'
  } else {
    sessions <- unique(ro$session)
  }

  for (s in sessions) {
    dir.create(paste0(getwd(), '/ssff/', s))
    ssff_path <- paste0(getwd(), '/ssff/', s)

    if (!session) {
      tmp_s <- r
    } else {
      tmp_s <- ro[which(ro$session==s),]
    }
    fls <- unique(tmp_s$file)

    for (f in fls) {
      tmp <- tmp_s[which(tmp_s$file==f),]
      start <- tmp$time[1]
      vals <- as.matrix(tmp$f0)
      ado <- list()
      attr(ado, 'trackFormats') <- c('REAL32')
      attr(ado, 'sampleRate') <- sr
      attr(ado, 'origFreq') <- 0
      attr(ado, 'startTime') <- start
      attr(ado, 'endRecord') <- nrow(tmp)
      class(ado) <- 'AsspDataObj'
      wrassp::AsspFileFormat(ado) <- 'SSFF'
      wrassp::AsspDataFormat(ado) <- as.integer(2)
      ado <- wrassp::addTrack(ado, 'rf0', vals, 'REAL32')

      new_path <- paste0(ssff_path, '/', f, '.rf0')
      wrassp::write.AsspDataObj(ado, file=new_path)
    }

    emuR::add_files(db_handle, ssff_path, 'rf0', s)

  }

  emuR::add_ssffTrackDefinition(db_handle, 'rf0', 'rf0', 'rf0')
  unlink('ssff', recursive=T)

}

reaper2ssff(reaper_out, db, TRUE)
signal_on_spec(db, 'rf0')

setwd('aarhus_tg_coll')
ps_out <- run_ps_dynamic_minmax(getwd(), FALSE, FALSE)

load('ps_out2.Rda')
ps_out <- ps_out %>% mutate(
  session = str_sub(Filename, end=3)
) %>% relocate(session, .after=Label) %>%
  mutate(seg_Start = as.numeric(seg_Start),
         session = paste0('_', session))

ps_out %>% praatsauce2ssff(db, 'session')

add_linkDefinition(db, 'ONE_TO_MANY', 'word', 'phone')
autobuild_linkFromTimes(db, superlevelName='word',
                   sublevelName='phone', convertSuperlevel=TRUE)

###getVOT

stop_items <- master %>%
  filter(foc.moa %in% c('asp', 'unasp')) %>%
  pull(foc.item) %>%
  data.frame(ort = .) %>%
  mutate(fn = str_replace_all(ort, c('æ' = 'E', 'ø' = 'OE', 'å' = 'O')),
         ort = tolower(ort))

sl <- query(db, paste0('word == ', paste(stop_items$ort, collapse='|')))
params <- pos_setParams('getVOT_training', cat=TRUE)

addVOT2emuDB(db, sl, level='word', sign='positive', pos_params_list=params)

### after handcorrecting VOT

sl <- query(db, paste0('word-autobuildBackup == ', stop_items$ort[1]), timeRefSegmentLevel='phone')
for (i in stop_items$ort[-1]) {
  tmp <- query(db, paste0('word-autobuildBackup == ', i), timeRefSegmentLevel='phone')
  sl <- rbind(sl, tmp)
}
items <- c()
for (i in sl$labels) {
  tmp <- stop_items[which(stop_items$ort == i), 'fn']
  items <- c(items, tmp)
}
sl <- sl[str_detect(sl$bundle, items),]
sl_vot <- sl_vot %>% arrange(bundle)
sl <- sl %>% arrange(bundle)

sl_vot <- query(db, 'vot == vot')
#add_linkDefinition(db, type='ONE_TO_MANY', superlevelName='word',
#                   sublevelName='vot')

add_levelDefinition(db, 'landmarks', type='EVENT')
# set_levelCanvasesOrder(db, 'default', c('phone', 'word-autobuildBackup', 'trans', 'vot'))

len <- nrow(sl)

clo <- data.frame(session = sl$session,
                  bundle = sl$bundle,
                  level = rep('landmarks', len),
                  start = sl$sample_start / sl$sample_rate[1] * 1000,
                  attribute = rep('landmarks', len),
                  labels = rep('clo', len),
                  db_uuid = db$UUID)
#create_itemsInLevel(db, clo)


rel <- data.frame(session = sl$session,
                  bundle = sl$bundle,
                  level = rep('landmarks', len),
                  start = sl_vot$sample_start / sl_vot$sample_rate[1] * 1000,
                  attribute = rep('landmarks', len),
                  labels = rep('rel', len),
                  db_uuid = db$UUID)
#create_itemsInLevel(db, rel)

vo <- data.frame(session = sl$session,
                 bundle = sl$bundle,
                 level = rep('landmarks', len),
                 start = sl_vot$sample_end / sl_vot$sample_rate[1] * 1000,
                 attribute = rep('landmarks', len),
                 labels = rep('vo', len),
                 db_uuid = db$UUID)
#create_itemsInLevel(db, vo)

offset <- data.frame(session = sl$session,
                     bundle = sl$bundle,
                     level = rep('landmarks', len),
                     start = sl$sample_end / sl$sample_rate[1] * 1000,
                     attribute = rep('landmarks', len),
                     labels = rep('offset', len),
                     db_uuid = db$UUID)
#create_itemsInLevel(db, offset)

itc <- rbind(clo, rel, vo, offset)

#also add a boundary some 100 msec before the closure, and do the nasals,
#which shouldnt be that tricky - should be the same as what ive done with
#the stops in the first place when making the vot seglistS

co <- emuR::get_levelCanvasesOrder(db, 'default')
co <- c(co, 'events')

nas_items <- master %>%
  filter(foc.moa == 'nasal') %>%
  pull(foc.item) %>%
  data.frame(ort = .) %>%
  mutate(fn = str_replace_all(ort, c('æ' = 'E', 'ø' = 'OE', 'å' = 'O')),
         ort = tolower(ort))

nas_ort <- paste(nas_items$ort, collapse='|')

#sl1 <- query(db, paste0('[#phone == m|n ^ word == ', nas_ort, ']'))
sl1 <- query(db, paste0('[#phone == m|n & Start(word,phone) == TRUE ^ word == ',
                       nas_ort, ']'))
q <- '[#phone == n & End(word,phone) == FALSE ^ word == fornyet|forneden]'
sl2 <- query(db, q)

sl <- rbind(sl1, sl2)

len <- nrow(sl)

clo <- data.frame(session = sl$session,
                  bundle = sl$bundle,
                  level = rep('landmarks', len),
                  start = sl$sample_start / sl$sample_rate[1] * 1000,
                  attribute = rep('landmarks', len),
                  labels = rep('clo', len),
                  db_uuid = db$UUID)
#create_itemsInLevel(db, clo)

rel <- data.frame(session = sl$session,
                  bundle = sl$bundle,
                  level = rep('landmarks', len),
                  start = sl$sample_end / sl$sample_rate[1] * 1000,
                  attribute = rep('landmarks', len),
                  labels = rep('rel', len),
                  db_uuid = db$UUID)
#create_itemsInLevel(db, rel)


sl <- query(db, paste0('word == ', nas_ort), timeRefSegmentLevel='phone') %>%
  distinct(.keep_all=TRUE)

len <- nrow(sl)

offset <- data.frame(session = sl$session,
                     bundle = sl$bundle,
                     level = rep('landmarks', len),
                     start = sl$sample_end / sl$sample_rate[1] * 1000,
                     attribute = rep('landmarks', len),
                     labels = rep('offset', len),
                     db_uuid = db$UUID)
#create_itemsInLevel(db, offset)

itc <- rbind(itc, clo, rel, offset)

create_itemsInLevel(db, itc)
emuR::set_levelCanvasesOrder(db, 'default', c('phone', 'landmarks'))

### looking at the data

sl_rel <- query(db, 'landmarks == rel')
sl_off <- query(db, 'landmarks == offset')
sl_clo <- query(db, 'landmarks == clo')

sl <- sl_rel
sl$end <- sl_off$start
sl$sample_end <- sl_off$sample_start
sl$clo <- sl_clo$start
sl <- sl %>%
  mutate(bndl_info = str_sub(bundle, start=5)) %>%
  separate(bndl_info, into=c('foc_item', 'foc_voi', 'foc_poa', 'foc_vheight', 'foc_st',
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
  vdur = ifelse(voi == 'nasal', end - start, end - vo),
  opdur = end - start,
  vot = ifelse(voi == 'nasal', NA, vo - start),
  cldur = start - clo
)

sl %>% group_by(voi, session) %>% summarize(vdur = mean(vdur),
                                   opdur = mean(opdur),
                                   vot = mean(vot),
                                   cldur = mean(cldur)) %>%
  print(n=50)

sl %>%
  #filter(voi %in% c('asp', 'unasp')) %>%
  ggplot() +
  aes(x=cldur, fill=voi) +
  geom_histogram()

sl %>%
  filter(voi == 'asp' & vot < 150 & vot > 40) %>%
  ggplot() +
  aes(x=vot, y=vdur, col=voi) +
  geom_point() +
  geom_smooth(method='lm')

####

#sl %>% filter(!session %in% c('_0008', '_0018', '_0020', '_0022')) -> sl

td <- get_trackdata(db, sl, ssffTrackName='f0')
td[which(td$T1 == 0),'T1'] <- NA

tdr <- get_trackdata(db, sl, ssffTrackName='rf0')
tdr[which(tdr$T1 == -1),'T1'] <- NA

td <- td %>% rename(praat_f0 = T1)
td$reaper_f0 <- tdr$T1

td <- td %>% mutate(
  low_est = ifelse(reaper_f0 < praat_f0, reaper_f0, praat_f0),
  f0_diff = (reaper_f0-praat_f0)/low_est,
  f0 = case_when(
    is.na(f0_diff) ~ NA,
    abs(f0_diff) > 0.2 ~ NA,
    times_orig < vo ~ NA,
    TRUE ~ (praat_f0+reaper_f0)/2
  )) %>% group_by(session) %>%
  mutate(uppF0 = mean(f0, na.rm=T) + 3*sd(f0, na.rm=T),
         lowF0 = mean(f0, na.rm=T) - 3*sd(f0, na.rm=T),
         f0 = ifelse(f0 > uppF0 | f0 < lowF0, NA, f0))


td <- td %>% group_by(session) %>%
  mutate(zf0 = as.numeric(scale(f0))) %>%
  ungroup()

td %>%
  #filter(!session %in% c('_0008', '_0018', '_0022')) %>%
  ggplot() +
  aes(x=times_norm, y=zf0, color=voi) %>%
  #geom_line() +
  geom_smooth() +
  facet_grid(st~cond)

td %>%
  #filter(!session %in% c('_0008', '_0018', '_0022')) %>%
  ggplot() +
  aes(x=times_rel, y=zf0, color=voi) %>%
  #geom_line() +
  geom_smooth() +
  facet_grid(st~cond)

### ALT SL
#should probably add some comments to this frankensteins monster document

alt_sl <- sl %>% mutate(start = ifelse(vo == 0, start, vo))
td2 <- get_trackdata(db, alt_sl, ssffTrackName='f0')
td2[which(td2$T1 == 0),'T1'] <- NA

tdr2 <- get_trackdata(db, alt_sl, ssffTrackName='rf0')
tdr2[which(tdr2$T1 == -1),'T1'] <- NA

td2 <- td2 %>% rename(praat_f0 = T1)
td2$reaper_f0 <- tdr2$T1

td2 <- td2 %>% mutate(
  low_est = ifelse(reaper_f0 < praat_f0, reaper_f0, praat_f0),
  f0_diff = (reaper_f0-praat_f0)/low_est,
  f0 = case_when(
    is.na(f0_diff) ~ NA,
    abs(f0_diff) > 0.2 ~ NA,
    times_orig < vo ~ NA,
    TRUE ~ (praat_f0+reaper_f0)/2
  )) %>% group_by(session) %>%
  mutate(uppF0 = mean(f0, na.rm=T) + 3*sd(f0, na.rm=T),
         lowF0 = mean(f0, na.rm=T) - 3*sd(f0, na.rm=T),
         f0 = ifelse(f0 > uppF0 | f0 < lowF0, NA, f0))


td2 <- td2 %>% group_by(session) %>%
  mutate(zf0 = as.numeric(scale(f0))) %>%
  ungroup()

td2 %>%
  #filter(!session %in% c('_0008', '_0018', '_0022')) %>%
  ggplot() +
  aes(x=times_rel, y=zf0, color=voi) %>%
  #geom_line() +
  geom_smooth() +
  facet_wrap(st~cond)

td2 %>%
  filter(!session %in% c('_0008', '_0018', '_0022')) %>%
  ggplot() +
  aes(x=times_norm, y=zf0, color=voi) %>%
  #geom_line() +
  geom_smooth() +
  facet_grid(st~cond)

f0_peaks <- td %>%
  filter(times_norm > 0.5 & times_norm < 0.95) %>%
  group_by(sl_rowIdx) %>%
  filter(zf0 == max(zf0, na.rm=TRUE))

f0_peaks2 <- td2 %>%
  filter(times_norm > 0.5 & times_norm < 0.95) %>%
  group_by(sl_rowIdx) %>%
  filter(zf0 == max(zf0, na.rm=TRUE))

f0_peaks %>%
  group_by(voi) %>%
  summarize(peak_time_norm_mean = mean(times_norm),
            peak_time_rel_mean = mean(times_rel),
            peak_time_norm_sd = sd(times_norm),
            peak_time_rel_sd = sd(times_rel))

f0_peaks2 %>%
  group_by(voi) %>%
  summarize(peak_time_norm = mean(times_norm),
            peak_time_rel = mean(times_rel),
            peak_time_norm_sd = sd(times_norm),
            peak_time_rel_sd = sd(times_rel))

#More overlapping distributions of pitch peak location when taken relative to
#voicing onset.

f0_peaks %>%
  ggplot() +
  aes(x=times_rel, color=voi) +
  geom_histogram(bins=20) +
  facet_grid(voi ~ .)

f0_peaks2 %>%
  ggplot() +
  aes(x=times_rel, color=voi) +
  geom_histogram(bins=20) +
  facet_grid(voi ~ .)

#There's a positive correlation between pitch peak location and VOT when
#taken relative to the open phase. No correlation when taken relative to
#voicing onset.
#If the open phase is a relevant landmark for intonation alignment,
#we wouldn't expect a correlation between VOT and pitch peak.

f0_peaks %>%
  ggplot() +
  aes(x=opdur, y=times_rel, color=voi) +
  geom_point() +
  geom_smooth(method='lm')

f0_peaks2 %>%
  ggplot() +
  aes(x=opdur, y=times_rel, color=voi) +
  geom_point() +
  geom_smooth(method='lm')

#add_levelDefinition(db, 'peaks', type='EVENT')

#len <- nrow(f0_peaks)
#peaks <- data.frame(session = f0_peaks$session,
                    # bundle = f0_peaks$bundle,
                    # level = rep('peaks', len),
                    # start = f0_peaks$times_orig,
                    # attribute = rep('peaks', len),
                    # labels = rep('peak', len),
                    # db_uuid = db$UUID)
#create_itemsInLevel(db, peaks)
#set_levelCanvasesOrder(db, 'default', c('phone', 'landmarks', 'peaks'))
#db %>% serve

