## ----setup, message=FALSE, echo=FALSE------------------------------------


## ----results='hide', message=FALSE, warning=FALSE------------------------
library("BacArena")

## ------------------------------------------------------------------------
data("Ec_core")

## ------------------------------------------------------------------------
bac <- Bac(Ec_core)

## ------------------------------------------------------------------------
arena <- Arena(n=20, m=20)

## ----results='hide', message=FALSE, warning=FALSE------------------------
arena

## ------------------------------------------------------------------------
arena <- addOrg(arena,bac,amount=20)

## ------------------------------------------------------------------------
arena <- addDefaultMed(arena, bac) 
arena <- addSubs(arena, smax=0.5, mediac="EX_glc(e)", unit="mM")

## ------------------------------------------------------------------------
arena

## ----results='hide', message=FALSE, warning=FALSE------------------------
eval <- simEnv(arena,time=12)

## ------------------------------------------------------------------------
getVarSubs(eval)

## ------------------------------------------------------------------------
getSubHist(eval, "EX_glc(e)")

## ------------------------------------------------------------------------
par(mfrow=c(1,2))
plotCurves2(eval, legendpos = "topleft")

## ------------------------------------------------------------------------
par(mfrow=c(2,5))
evalArena(eval, show_legend = FALSE, time=1:10)

## ------------------------------------------------------------------------
bac1 <- Bac(Ec_core,type="ecoli_wt")

## ------------------------------------------------------------------------
ecore_aux <- changeBounds(Ec_core, "EX_o2(e)",lb=0)
bac2 <- Bac(ecore_aux,type="ecoli_aux", setExInf=FALSE)

## ----results='hide', message=FALSE, warning=FALSE------------------------
arena <- Arena(n=30, m=30)
arena <- addOrg(arena,bac1,amount=20)
arena <- addOrg(arena,bac2,amount=20)
arena <- addDefaultMed(arena, bac1)
arena <- addSubs(arena, smax=0.5, mediac="EX_glc(e)", unit="mM")
eval <- simEnv(arena,time=10)

## ------------------------------------------------------------------------
plotCurves2(eval)

## ------------------------------------------------------------------------
par(mar=c(1,1,1,1))
evalArena(eval,c("Population","EX_glc(e)","EX_ac(e)","EX_o2(e)"),
          time=7)

## ------------------------------------------------------------------------
minePheno(eval)
pmat <- getPhenoMat(eval)
pmat[,which(colSums(pmat)>0)]

## ----results='hide', message=FALSE, warning=FALSE------------------------
library(parallel)
replicates <- 2
cores <- ifelse(detectCores()>=2, 2, 1)
cl <- makeCluster(cores, type="PSOCK")
clusterExport(cl, "Ec_core")
simlist <- parLapply(cl, 1:replicates, function(i){
  bac   <- BacArena::Bac(model=Ec_core)
  arena <- BacArena::Arena(n=20, m=20)
  arena <- BacArena::addOrg(arena, bac, amount=10)
  arena <- BacArena::addDefaultMed(arena, bac)
  sim   <- BacArena::simEnv(arena, time=5)
})

## ------------------------------------------------------------------------
p <- plotGrowthCurve(simlist)
p[[2]]

## ------------------------------------------------------------------------
p <- plotSubCurve(simlist)
p[[3]]

## ------------------------------------------------------------------------
data("sihumi_test")
eval <- sihumi_test

## ------------------------------------------------------------------------
p <- plotAbundance(eval)
p + ggplot2::scale_color_manual(values=colpal3)

## ------------------------------------------------------------------------
p <- plotSpecActivity(eval)
p[[2]]

## ------------------------------------------------------------------------
g <- findFeeding3(eval, time = 10, mets = c("EX_acald(e)", "EX_lac_D(e)") )

## ------------------------------------------------------------------------
p <- plotSubUsage(eval, subs = c("EX_sucr(e)", "EX_amylose300(e)", "EX_amylopect900(e)", 
                                 "EX_cellb(e)", "EX_hdca(e)", "EX_ocdca(e)"))
p[[2]]

## ------------------------------------------------------------------------
p <- plotShadowCost(eval, 6)
p[[2]]

