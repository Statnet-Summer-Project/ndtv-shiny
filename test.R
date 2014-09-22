# TODO: Add comment
# 
# Author: kirk
###############################################################################


render.animation
function(net, render.par=list(tween.frames=10,show.time=TRUE,show.stats=NULL,extraPlotCmds=NULL,initial.coords=0),plot.par=list(bg='white'),ani.options=list(interval=0.1),render.cache=c('plot.list','none'), verbose=TRUE,...){
 if (!is.network(net)){
  stop("render.animation requires the first argument to be a network object")
 }
 
 
 # check render.par params
 if (is.null(render.par)){
  stop("render.animation is missing the 'render.par' argument (a list of rendering parameters).")
 }
 if (is.null(render.par$tween.frames)){
  render.par$tween.frames<-10 
 }
 if (is.null(render.par$show.time)){
  render.par$show.time<-TRUE
 }
 if (is.null(render.par$initial.coords)){
  render.par$initial.coords<-matrix(0,ncol=2,nrow=network.size(net))
 }
 
 #check if coordinates have already been computed
 if (!all(c("animation.x.active","animation.y.active") %in% list.vertex.attributes(net))){
  net <- compute.animation(net,verbose=verbose)
 }
 
 
 # temporary hard-coded param to work around plot issue in RStudio
 externalDevice<-FALSE
 doRStudioHack<-TRUE
 if(!is.null(render.par$'do_RStudio_plot_hack')){
  doRStudioHack<-render.par$'do_RStudio_plot_hack'
 }
 if (!is.function(options()$device)){
  if (names(dev.cur())=="RStudioGD" & doRStudioHack){
   message("RStudio's graphics device is not well supported by ndtv, attempting to open another type of plot window")
   # try to open a new platform-appropriate plot window
   if (.Platform$OS.type=='windows'){
    windows()
   } else if(length(grep(R.version$platform,pattern='apple'))>0)  # is it mac?
   {
    quartz()
   } else {  # must be unix
    x11()
   }
   externalDevice<-TRUE
  }
 }
 
 # make sure background color is not transparent unless set that way explicitly
 if (par("bg")=="transparent" & is.null(plot.par$'bg')){
  plot.par$'bg'<-'white'
 }
 # set high-level plot attributes (bg color, margins, etc)
 # and cache initial graphics par settings
 origPar<-par(plot.par) 
 
 # set animation options
 oopts <- ani.options(ani.options)
 
 #figure out what the slicing parameters were
 slice.par <- get.network.attribute(net,"slice.par")
 if (is.null(slice.par)){
  stop("render.animation can not locate the 'slice.par' list of parameters in the input network object")
 }
 
 
 # check plot caching params
 render.cache<-match.arg(render.cache)
 
 
 # cache plotting arguments 
 plot_params<-list(...)
 
 # define some defaults for ploting args
 # label defaults to vertex names
 if(is.null(plot_params$label)){
  plot_params$label<-function(slice){network.vertex.names(slice)}
 }
 # xlab defaults to time
 if(is.null(plot_params$xlab) & render.par$show.time){
  plot_params$xlab <- function(onset,terminus){ifelse(onset==terminus,paste("t=",onset,sep=''),paste("t=",onset,"-",terminus,sep=''))}
 }
 # but if show stats, use that instead 
 # TODO: deprecate show.stats in favor of passing in directly for evaluation?
 if(!is.null(render.par$show.stats) && render.par$show.stats!=FALSE){
  # evaluate a eqn string giving the stats formual
  # TODO: this requires that tergm be loaded! give informative warning if not
  if(render.par$show.time){
   # include the time string in the summary
   plot_params$xlab <- eval(parse(text=paste("function(slice,onset,terminus){stats<-summary.statistics.network(slice",render.par$show.stats,")\n return(paste('t=',onset,'-',terminus,' ',paste(rbind(names(stats),stats),collapse=':'),sep='')) }",sep='')))
  } else {
   plot_params$xlab <- eval(parse(text=paste("function(slice){stats<-summary.statistics.network(slice",render.par$show.stats,")\n return(paste(rbind(names(stats),stats),collapse=':')) }",sep='')))
  }
 }
 
 #disable jitter by default because it messes things up
 if(is.null(plot_params$jitter)){
  plot_params$jitter<-FALSE
 }
 
 #TODO: how are we doing interpolation?
 interp.fun<-coord.interp.smoothstep
 #interp.fun<-coord.interp.linear
 
 # compute lists of times that networks will be collapsed
 starts <- seq(from=slice.par$start,to=slice.par$end,by=slice.par$interval)
 ends <- seq(from=slice.par$start+slice.par$aggregate.dur,to=slice.par$end+slice.par$aggregate.dur,by=slice.par$interval)
 
 #compute coordinate ranges to know how to scale plots
 xmin <- aggregate.vertex.attribute.active(net,"animation.x",min)
 xmax <- aggregate.vertex.attribute.active(net,"animation.x",max)
 ymin <- aggregate.vertex.attribute.active(net,"animation.y",min)
 ymax <- aggregate.vertex.attribute.active(net,"animation.y",max)
 if (is.null(plot_params$xlim)){
  # deal with case of only one coord, so no range
  if(xmin==xmax){
   xmax<-xmin+1
   xmin<-xmin-1
  }
  plot_params$xlim<-c(xmin,xmax)
 }
 if(is.null(plot_params$ylim)){
  # deal with case of only one coord, so no range
  if(ymin==ymax){
   ymax<-ymin+1
   ymin<-ymin-1
  }
  plot_params$ylim<-c(ymin,ymax)
 }
 
 #set up default coords.  If not specified, default will be zero
 if(is.numeric(render.par$initial.coords)){
  coords<-matrix(render.par$initial.coords,ncol=2,nrow=network.size(net))
 }
 
 #compute some starting coords  
 slice <- network.collapse(net,starts[1],ends[1],rule=slice.par$rule,rm.time.info=FALSE) 
 activev <- is.active(net,starts[1],ends[1],v=seq_len(network.size(net)),rule=if(slice.par$rule!='all'){'any'})
 
 # start from the coords of the first slice
 if (length(slice)>0 & network.size(slice)>0){ 
  coords[activev,1] <-get.vertex.attribute(slice,"animation.x")
  coords[activev,2] <-get.vertex.attribute(slice,"animation.y")
  #need to update plot params with slice-specific values
#     evald_params<-.evaluate_plot_params(plot_params=plot_params,net=net,slice=slice,s=1,onset=starts[1],terminus=ends[1])
#     
#     
#     # set up arguments
#     plot_args<-list(x=slice,coord=coords[activev,,drop=FALSE])
#     plot_args<-c(plot_args,evald_params)
#     # cll the plotting function with appropriate args
#     do.call(plot.network, plot_args)
#                
#     # check if user has passed in extra plotting commands that need to be rendered
#     if (!is.null(render.par$extraPlotCmds)){
#       eval(render.par$extraPlotCmds)
#     }
  
 }# end slice > 0 block
 
 coords2 <- coords
 
 if (render.cache=='plot.list'){
  ani.record(reset=TRUE)
 }
 #move through frames to render them out
 for(s in 1:length(starts)){
  if (verbose){print(paste("rendering",render.par$tween.frames,"frames for slice",s-1))}
  slice <- network.collapse(net,starts[s],ends[s],rule=slice.par$rule,rm.time.info=FALSE)
  activev <- is.active(net,starts[s],ends[s],v=seq_len(network.size(net)),rule=if(slice.par$rule!='all'){'any'})
  
  #TODO: draw new slices for intermediate tween frames?
  #skip any empty networks
  if (length(slice)>0 & network.size(slice)>0){
   #need to update plot params with slice-specific values
   evald_params<-.evaluate_plot_params(plot_params=plot_params,net=net,slice=slice,s=s,onset=starts[s],terminus=ends[s])
   
   
   for(t in 1:render.par$tween.frames){
    #coords2[activev,1]<-get.vertex.attribute.active(slice,"animation.x",onset=starts[s],terminus=ends[s])
    #coords2[activev,2]<-get.vertex.attribute.active(slice,"animation.y",onset=starts[s],terminus=ends[s])
    coords2[activev,1]<-get.vertex.attribute(slice,"animation.x")
    coords2[activev,2]<-get.vertex.attribute(slice,"animation.y")
    # tweenCoords <- coords + ((coords2-coords)*(t/render.par$tween.frames))
    tweenCoords <- interp.fun(coords,coords2,t,render.par$tween.frames)
    
    #TODO:what if we want to include innactive nodes
    # set up arguments
    plot_args<-list(x=slice,coord=tweenCoords[activev,,drop=FALSE])
    plot_args<-c(plot_args,evald_params)
    # cll the plotting function with appropriate args
    do.call(plot.network, plot_args) 
    # check if user has passed in extra plotting commands that need to be rendered
    if (!is.null(render.par$extraPlotCmds)){
     eval(render.par$extraPlotCmds)
    }
    if (render.cache=='plot.list'){
     ani.record()
    }
   }
   coords<-coords2;
  } else { # end slice > 0 block
   # empty network causes plot problems
   # draw some blank frames while time passes
   evald_params<-.evaluate_plot_params(plot_params=plot_params,net=net,slice=slice,s=s,onset=starts[s],terminus=ends[s])
   if(render.par$show.time){
    xlab<-evald_params$xlab
   } else {
    xlab<-NULL
   }
   
   singlenet <-network.initialize(1)
   for(t in 1:render.par$tween.frames){
    plot.network(singlenet,
      vertex.cex=0,xlab=xlab)
    # check if user has passed in extra plotting commands that need to be rendered
    if (!is.null(render.par$extraPlotCmds)){
     eval(render.par$extraPlotCmds)
    }
    if (render.cache=='plot.list'){
     ani.record()
    }
   }
  } # end empty network block
 }
 
 # reset the graphics params back
 par(origPar)
 # turn off external device if using one
 if (externalDevice){
  dev.off()
 }
 
}




function(reset = FALSE, replay.cur = FALSE) {
 if (reset) .ani.env$.images = list() else {
  ## make sure a graphics device has been opened
  if (dev.cur() != 1) {
   n = length(.ani.env$.images)
   .ani.env$.images[[n + 1]] = recordPlot()
  } else warning('no current device to record from')
 }
 if (replay.cur) {
  tmp = recordPlot()
  dev.hold()
  replayPlot(tmp)
  dev.flush()
 }
 invisible(NULL)
}




