cph <- c(120, 100, 80, 100, 120, 140, 160, 180, 140)
cph <- approx(seq(1, 100, length.out=9), cph, 1:100)$y
cph <- emuR::dct(cph, 10, fit=T)
jut <- c(150, 180, 160, 140, 120, 110, 100, 90, 80)
jut <- approx(seq(1, 100, length.out=9), jut, 1:100)$y
jut <- emuR::dct(jut, 10, fit=T)

plot(cph, type='l', yaxt = 'n', xaxt = 'n',
     ylab = 'Frequency', xlab = 'Time', ylim = c(70,180),
     col = 'blue', lwd = 2)
lines(jut, col = 'darkorange', lwd = 2)


### RQ1

par(mfrow = c(1,3), cex = 1, mar = c(3, 2, 3, 2))

h1a_p <- h1a_m <- rep(0, 100)
h1a_b <- c(emuR::dct(-50:0, m=2, fit = T)+4.34, rep(0, 50))

plot(h1a_m, type='l', yaxt = 'n', xaxt = 'n', ylim = c(-50,50),
     col = 'darkgrey', lwd = 2, main = expression('H1'[a]))
lines(h1a_p, col = 'darkorange', lwd = 2, lty = 'dashed')
lines(h1a_b, col = 'blue', lwd = 2)
lines(rep(50, 15), col = 'darkgrey', lwd = 2)
text(25, 50, 'm', font = 2)
lines(rep(45, 15), col = 'blue', lwd = 2)
text(25, 45, 'b', font = 2)
lines(rep(40, 15), col = 'darkorange', lwd = 2)
text(25, 40, 'p', font = 2)
mtext('Frequency', side = 2, line = 1)

h1b_b <- h1b_m <- rep(0, 100)
h1b_p <- c(emuR::dct(50:0, m=2, fit = T)-4.34, rep(0, 50))

plot(h1b_m, type='l', yaxt = 'n', xaxt = 'n', ylim = c(-50,50),
     col = 'darkgrey', lwd = 2, main = expression('H1'[b]))
lines(h1b_b, col = 'blue', lwd = 2, lty = 'dashed')
lines(h1b_p, col = 'darkorange', lwd = 2)
mtext('Time', side = 1, line = 1)

h1c_m <- rep(0, 100)
h1c_p <- c(emuR::dct(50:0, m=2, fit = T)-4.34, rep(0, 50))
h1c_b <- c(emuR::dct(seq(25, 0, length.out = 50)-2.16, m=2, fit = T),
           rep(0, 50))

plot(h1c_m, type='l', yaxt = 'n', xaxt = 'n', ylim = c(-50,50),
     col = 'darkgrey', lwd = 2, main = expression('H1'[c]))
lines(h1c_b, col = 'blue', lwd = 2)
lines(h1c_p, col = 'darkorange', lwd = 2)

### RQ2

par(mfrow = c(1,2), cex = 1, mar = c(3, 2, 3, 2))

cph <- c(120, 100, 80, 100, 120, 140, 160, 180, 140)
cph <- approx(seq(1, 100, length.out=9), cph, 1:100)$y
cph <- emuR::dct(cph, 10, fit=T)
jut <- c(160, 180, 160, 140, 120, 110, 100, 90, 80)
jut <- approx(seq(1, 100, length.out=9), jut, 1:100)$y
jut <- emuR::dct(jut, 10, fit=T)

h2a_jp <- c(seq(190, jut[15], length.out = 15), jut[16:100])
h2a_cp <- c(seq(125, cph[15], length.out = 15), cph[16:100])

plot(cph, type='l', ylim = c(60,190), yaxt = 'n', xaxt = 'n', lwd = 2,
     main = expression('H2'[a]), col = 'darkgrey')
lines(jut, lty = 'dashed', lwd = 2, col = 'darkgrey')
lines(h2a_cp, col = 'darkorange', lwd = 2)
lines(h2a_jp, col = 'darkorange', lty = 'dashed', lwd = 2)
lines(rep(60, 15), col = 'darkgrey', lwd = 2)
text(25, 60, 'm', font = 2)
lines(rep(70, 15), col = 'darkorange', lwd = 2)
text(25, 70, 'p', font = 2)
lines(x = 41:55, y = rep(60, 15), col = 'darkgrey', lwd = 2)
text(60, 60, 'Zealand', font = 2, adj = 0)
lines(x = 41:55, y = rep(70, 15), col = 'darkgrey', lwd = 2, lty = 'dashed')
text(60, 70, 'Jutland', font = 2, adj = 0)

mtext('Frequency', side = 2, line = 1)
mtext('Time', side = 1, line = 1)

h2b_jp <- c(seq(190, jut[15], length.out = 15), jut[16:100])
h2b_cp <- c(seq(145, cph[15], length.out = 15), cph[16:100])

plot(cph, type='l', ylim = c(60,190), yaxt = 'n', xaxt = 'n', lwd = 2,
     main = expression('H2'[b]), col = 'darkgrey')
lines(jut, lty = 'dashed', lwd = 2, col = 'darkgrey')
lines(h2b_cp, col = 'darkorange', lwd = 2)
lines(h2b_jp, col = 'darkorange', lty = 'dashed', lwd = 2)

### RQ3

par(mfrow = c(1,3), cex = 1, mar = c(3, 2, 3, 2))

h3a_sm <- seq(40, 0, length.out = 100)
h3a_mm <- rep(0, 100)
h3a_sp <- c(emuR::dct(seq(10, 0, length.out = 50)-0.868, m=2, fit = T),
            rep(0, 50)) + h3a_sm
h3a_mp <- c(emuR::dct(seq(5, 0, length.out = 50)-0.434, m=2, fit = T),
            rep(0, 50)) + h3a_mm

plot(h3a_sm, type='l', yaxt = 'n', xaxt = 'n',
     ylim = c(-10,50), col = 'darkgrey', lwd = 2,
     lty = 'dashed', main = expression('H3'[a]))
lines(h3a_mm, col = 'darkgrey', lwd = 2)
lines(h3a_sp, col = 'darkorange', lwd = 2, lty = 'dashed')
lines(h3a_mp, col = 'darkorange', lwd = 2)
lines(rep(-10, 15), col = 'darkgrey', lwd = 2)
text(25, -10, 'm', font = 2)
lines(rep(-5, 15), col = 'darkorange', lwd = 2)
text(25, -5, 'p', font = 2)
lines(x = 41:55, y = rep(-10, 15), col = 'darkgrey', lwd = 2)
text(60, -10, 'modal', font = 2, adj = 0)
lines(x = 41:55, y = rep(-5, 15), col = 'darkgrey', lwd = 2, lty = 'dashed')
text(60, -5, 'stød', font = 2, adj = 0)
mtext('Frequency', side = 2, line = 1)

h3b_sm <- seq(40, 0, length.out = 100)
h3b_mm <- rep(0, 100)
h3b_sp <- c(emuR::dct(seq(5, 0, length.out = 50)-0.434, m=2, fit = T),
            rep(0, 50)) + h3b_sm
h3b_mp <- c(emuR::dct(seq(10, 0, length.out = 50)-0.868, m=2, fit = T),
            rep(0, 50)) + h3b_mm

plot(h3b_sm, type='l', yaxt = 'n', xaxt = 'n',
     ylim = c(-10,50), col = 'darkgrey', lwd = 2,
     lty = 'dashed', main = expression('H3'[b]))
lines(h3b_mm, col = 'darkgrey', lwd = 2)
lines(h3b_sp, col = 'darkorange', lwd = 2, lty = 'dashed')
lines(h3b_mp, col = 'darkorange', lwd = 2)
mtext('Time', side = 1, line = 1)

h3c_sm <- seq(40, 0, length.out = 100)
h3c_mm <- rep(0, 100)
h3c_sp <- c(emuR::dct(seq(10, 0, length.out = 50)-0.868, m=2, fit = T),
            rep(0, 50)) + h3c_sm
h3c_mp <- c(emuR::dct(seq(10, 0, length.out = 50)-0.868, m=2, fit = T),
            rep(0, 50)) + h3c_mm

plot(h3c_sm, type='l', yaxt = 'n', xaxt = 'n',
     ylim = c(-10,50), col = 'darkgrey', lwd = 2,
     lty = 'dashed', main = expression('H3'[c]))
lines(h3c_mm, col = 'darkgrey', lwd = 2)
lines(h3c_sp, col = 'darkorange', lwd = 2, lty = 'dashed')
lines(h3c_mp, col = 'darkorange', lwd = 2)
