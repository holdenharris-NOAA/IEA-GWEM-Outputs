
















## Plot --------------------------------------------------------------------
col_obs = 'black'
col_sim = rgb(0.2, 0.7, .1, alpha = 0.9) ## rgb (red, green, blue, alpha)
col_spa = rgb(0.1, 0, 1, alpha = 0.8) 

## Initialize plot
x = years
min = min(obs_scaled, simB_scaled, spaB_scaled, na.rm=T) * 0.8
max = max(obs_scaled, simB_scaled, spaB_scaled, na.rm=T) * 1.2
plot(x, rep("", length(x)), type='b', 
     ylim = c(min, max), 
     xaxt = 'n', yaxt = 'n',
     xlab = '', ylab='', bty = 'n')

## Set up axes
num_breaks_x <- round((end_y - start_y) / 5) ## Break x-axis every 5 years
x_ticks <- pretty(x, n = num_breaks_x)
xlab = paste0("'", substring(as.character(x_ticks), nchar(as.character(x_ticks)) - 1))
axis(1, at = x_ticks, labels = xlab, cex.axis=0.9, las=2)
y_ticks = pretty(seq(min, max, by = (max-min)/10), n = 4)
axis(2, at = y_ticks, labels = y_ticks, las = 1, cex.axis = 0.9)
abline(h=1, col='lightgray')

## Plot outputs: Ecosim (green line), Ecospace (blue line), Observed (black dots)
lines(x, simB_scaled, lty=1, lwd = 2,  col=col_sim) ## Plot Ecosim
if(is.vector(spaB)) lines(x, spaB_scaled, lty=1, lwd = 2, col = col_spa) ## Plot Ecospace
if(length(obs)>0) points(years, obs_scaled, pch=16, cex=0.7, col = col_obs) ## Plot observed data, if it's present
if(is.matrix(spaB)) matlines(x, spaB_scaled, lty=1, lwd = 2, col=c(rep('lightblue', ncol(spaB)-1), col_spa)) ## Code for plotting multipole Ecospace projections, if available.

## Add legend and title
title(main = grp, line=-.6, cex.main=0.85)
if(i %in% seq(1, num_fg, by = ceiling(num_fg / num_plot_pages))) {
  legend('topleft', inset = 0.1, bg="gray90", box.lty=0,
         legend=c('Observed','Ecosim','Ecospace'),
         lty=c(NA,1,1), pch=c(16, NA, NA), 
         col=c(col_obs, col_sim, col_spa), cex = 0.7)
}
}
dev.off()   


## -----------------------------------------------------------------------------
##
## Plot and compare MONTHLY biomass 

pdf(paste0(dir_pdf_out, plot_name_xM, ".PDF"), onefile = TRUE)
## Set number of plots per page
set.mfrow = f.get_plot_dims(x=num_fg / num_plot_pages, round2=4)
par(mfrow=set.mfrow, mar=c(1.2, 2, 1.2, 2))

for(i in 1:num_fg){
  #i=68
  grp = fg_df$group_name[i]
  spaB = spaB_xM[,i] 
  simB = simB_xM[,i] 
  
  ## Check to see if observed data is available
  rm(obs_df)
  if(i %in% obsB.head$pool_code){
    obs.idx = which(obsB.head$pool_code==i)
    obs_df = data.frame(year_series, obsB = as.numeric(obsB[,obs.idx]))
    rm(obs.idx)
  }
  
  ## Scale to the average of a given timeframe
  spaB_scaled = spaB / mean(spaB[1:init_months_toscale], na.rm = TRUE)
  simB_scaled = simB / mean(simB[1:init_months_toscale], na.rm = TRUE)
  obsB_scaled = NULL
  if(exists('obs_df')) obsB_scaled = obs_df$obsB / mean(na.omit(obs_df$obsB)[1:init_years_toscale], na.rm = TRUE)
  
  ## Plot --------------------------------------------------------------------
  col_obs = 'black'
  col_sim = rgb(0.2, 0.7, .1, alpha = 0.8) ## rgb (red, green, blue, alpha)
  col_spa = rgb(0.1, 0, 1, alpha = 0.6) 
  
  ## Initialize plot
  x = date_series
  min = min(obs_scaled, simB_scaled, spaB_scaled, na.rm=T) * 0.8
  max = max(obs_scaled, simB_scaled, spaB_scaled, na.rm=T) * 1.2
  plot(x, rep("", length(x)), type='b', 
       ylim = c(min, max), 
       xaxt = 'n', yaxt = 'n',
       xlab = '', ylab='', bty = 'n')
  
  ## Set up axes
  num_breaks_x <- round((end_y - start_y) / 5) ## Break x-axis every 5 years
  x_ticks <- pretty(x, n = num_breaks_x)
  xlab = paste0("'", substring(format(x_ticks, "%Y"), nchar(format(x_ticks, "%Y")) - 1))
  axis(1, at = x_ticks, labels = xlab, cex.axis=0.9, las=2)
  y_ticks = pretty(seq(min, max, by = (max-min)/10), n = 4)
  axis(2, at = y_ticks, labels = y_ticks, las = 1, cex.axis = 0.9)
  abline(h=1, col='lightgray')
  
  ## Plot outputs: Ecosim (green line), Ecospace (blue line), Observed (black dots)
  lines(x, simB_scaled, lty=1, lwd = 1,  col=col_sim) ## Plot Ecosim
  if(is.vector(spaB)) lines(x, spaB_scaled, lty=1, lwd = 1, col = col_spa) ## Plot Ecospace
  if(length(obsB_scaled)>0) points(obs_df$year_series, obsB_scaled, pch=16, cex=0.6, col = col_obs) ## Plot observed data, if it's present
  if(is.matrix(spaB)) matlines(x, spaB_scaled, lty=1, lwd = 2, col=c(rep('lightblue', ncol(spaB)-1), col_spa)) ## Code for plotting multipole Ecospace projections, if available.
  
  ## Add legend and title
  title(main = grp, line=-.6, cex.main=0.85)
  if(i %in% seq(1, num_fg, by = ceiling(num_fg / num_plot_pages))) {
    legend('topleft', inset = 0.1, bg="gray90", box.lty=0,
           legend=c('Observed','Ecosim','Ecospace'),
           lty=c(NA,1,1), pch=c(16, NA, NA), 
           col=c(col_obs, col_sim, col_spa), cex = 0.7)
  }
}
dev.off()

## -----------------------------------------------------------------------------
##
## Plot and compare ANNUAL catches



par(mfrow=c(1,1))