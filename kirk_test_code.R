library(ndtv)


Sys.which('ffmpeg')

ani.options(outdir=getwd())

saveVideo({
  par(mar = c(3, 3, 1, 0.5), mgp = c(2, 0.5, 0), tcl = -0.3, cex.axis = 0.8, cex.lab = 0.8, 
      cex.main = 1)
  ani.options(interval = 0.05, nmax = 300)
  brownian.motion(pch = 21, cex = 5, col = "red", bg = "yellow")
}, video.name = "BM.mp4")  # higher bitrate, better quality
