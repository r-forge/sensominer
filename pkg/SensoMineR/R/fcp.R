fcp <- function(X,group,scale=TRUE, ncp = 5, axes=c(1,2), name.group = NULL, level.conf = 0.95, nbsim=500, nbchoix=NULL, cex=1, color=NULL, title=NULL, new.plot=TRUE, graph=c("ind","var","ellipse")){

if (scale) type = rep("s",length(group))
if (!scale) type = rep("c",length(group))
res <- MFA(X,group=group,type=type,ncp=ncp,name.group=name.group,graph=FALSE)
if (new.plot) dev.new()
if ("ind"%in%graph) plot(res,cex=1,title=title,color=color,new.plot=FALSE)
if ("var"%in%graph){
  if (new.plot|("ind"%in%graph)) dev.new()
  plot(res,choix="var",cex=1,title=title,color=color,new.plot=FALSE,habillage="group")
}
if ("ellipse"%in%graph){
  if (new.plot|("ind"%in%graph)|("var"%in%graph)) dev.new()
  sim <- boot(X, method = "freechoice", axes = axes, scale = scale, ncp = ncp, group = group, nbsim = nbsim, level.conf = level.conf,nbchoix = 
    nbchoix,color = color,cex = cex, title = title, new.plot = TRUE)
}
result <- list()
result$mfa <- res
result$ellipse <- sim
return(result) 
}
