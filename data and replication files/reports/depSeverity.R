# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# The impact of school support on depression among adolescent orphans: a cluster‐randomized trial in Kenya
# Analysis file
# https://onlinelibrary.wiley.com/doi/abs/10.1111/jcpp.12955
# October 2017
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# Depression severity

# fit 2-way model, time as factor
  m1 <- lmer(depress.z ~ w1group + female + time2 + time3 + time4 + 
               w1group*time2 + w1group*time3 + w1group*time4 +
               stratum +
               (1 | w1schoolid) + (1 + time | id),
             data=datL)
  
  # linear combination to get effect at time 2
  m1.lh2 <- confint(glht(m1, 
                         linfct=c("w1group +
                                  w1group:time2 = 0")))
  m1.lh2Est <- m1.lh2$confint[1]   # estimate
  m1.lh2L95 <- m1.lh2$confint[2]   # lower 95% CI
  m1.lh2U95 <- m1.lh2$confint[3]   # upper 95% CI
  m1.time2 <- paste0(rd2(m1.lh2Est),
                     " (", 
                     rd2(m1.lh2L95),
                     " to ",
                     rd2(m1.lh2U95),
                     ")")  

# linear combination to get effect at time 3
  m1.lh3 <- confint(glht(m1, 
                         linfct=c("w1group +
                                  w1group:time3 = 0")))
  m1.lh3Est <- m1.lh3$confint[1]   # estimate
  m1.lh3L95 <- m1.lh3$confint[2]   # lower 95% CI
  m1.lh3U95 <- m1.lh3$confint[3]   # upper 95% CI
  m1.time3 <- paste0(rd2(m1.lh3Est),
                     " (", 
                     rd2(m1.lh3L95),
                     " to ",
                     rd2(m1.lh3U95),
                     ")")  

# linear combination to get effect at time 4
  m1.lh4 <- confint(glht(m1, 
                         linfct=c("w1group +
                                  w1group:time4 = 0")))
  m1.lh4Est <- m1.lh4$confint[1]   # estimate
  m1.lh4L95 <- m1.lh4$confint[2]   # lower 95% CI
  m1.lh4U95 <- m1.lh4$confint[3]   # upper 95% CI
  m1.time4 <- paste0(rd2(m1.lh4Est),
                     " (", 
                     rd2(m1.lh4L95),
                     " to ",
                     rd2(m1.lh4U95),
                     ")")

# fit 3-way model, time as factor
  m2 <- lmer(depress.z ~ w1group + female + time2 + time3 + time4 + 
               w1group*time2 + w1group*time3 + w1group*time4 +
               female*time2 + female*time3 + female*time4 + 
               w1group*female*time2 + w1group*female*time3 + 
               w1group*female*time4 +
               stratum +
               (1 | w1schoolid) + (1 + time | id),
             data=datL)

# remove w1group:female and re-fit
  m2 <- update(m2, .~. - w1group:female)   
  
  # linear combination to get effect for females at time 2
  m2.lhF2 <- confint(glht(m2, 
                          linfct=c("w1group+
                                   w1group:time2 + 
                                   w1group:female:time2 = 0")))
  m2.lhF2Est <- m2.lhF2$confint[1]   # estimate
  m2.lhF2L95 <- m2.lhF2$confint[2]   # lower 95% CI
  m2.lhF2U95 <- m2.lhF2$confint[3]   # upper 95% CI
  m2.female2 <- paste0(rd2(m2.lhF2Est), 
                       " (", 
                       rd2(m2.lhF2L95),
                       " to ",
                       rd2(m2.lhF2U95),
                       ")")

# linear combination to get effect for females at time 3
  m2.lhF3 <- confint(glht(m2, 
                          linfct=c("w1group+
                                   w1group:time3 + 
                                   w1group:female:time3 = 0")))
  m2.lhF3Est <- m2.lhF3$confint[1]   # estimate
  m2.lhF3L95 <- m2.lhF3$confint[2]   # lower 95% CI
  m2.lhF3U95 <- m2.lhF3$confint[3]   # upper 95% CI
  m2.female3 <- paste0(rd2(m2.lhF3Est), 
                       " (", 
                       rd2(m2.lhF3L95),
                       " to ",
                       rd2(m2.lhF3U95),
                       ")")

# linear combination to get effect for females at time 4
  m2.lhF4 <- confint(glht(m2, 
                          linfct=c("w1group+
                                   w1group:time4 + 
                                   w1group:female:time4 = 0")))
  m2.lhF4Est <- m2.lhF4$confint[1]   # estimate
  m2.lhF4L95 <- m2.lhF4$confint[2]   # lower 95% CI
  m2.lhF4U95 <- m2.lhF4$confint[3]   # upper 95% CI
  m2.female4 <- paste0(rd2(m2.lhF4Est), 
                       " (", 
                       rd2(m2.lhF4L95),
                       " to ",
                       rd2(m2.lhF4U95),
                       ")")

# linear combination to get effect for males at time 2
  m2.lhM2 <- confint(glht(m2, 
                          linfct=c("w1group+
                                   w1group:time2 = 0")))
  m2.lhM2Est <- m2.lhM2$confint[1]   # estimate
  m2.lhM2L95 <- m2.lhM2$confint[2]   # lower 95% CI
  m2.lhM2U95 <- m2.lhM2$confint[3]   # upper 95% CI
  m2.male2 <- paste0(rd2(m2.lhM2Est), 
                     " (", 
                     rd2(m2.lhM2L95),
                     " to ",
                     rd2(m2.lhM2U95),
                     ")")

# linear combination to get effect for males at time 3
  m2.lhM3 <- confint(glht(m2, 
                          linfct=c("w1group+
                                   w1group:time3 = 0")))
  m2.lhM3Est <- m2.lhM3$confint[1]   # estimate
  m2.lhM3L95 <- m2.lhM3$confint[2]   # lower 95% CI
  m2.lhM3U95 <- m2.lhM3$confint[3]   # upper 95% CI
  m2.male3 <- paste0(rd2(m2.lhM3Est), 
                     " (", 
                     rd2(m2.lhM3L95),
                     " to ",
                     rd2(m2.lhM3U95),
                     ")")

# linear combination to get effect for males at time 4
  m2.lhM4 <- confint(glht(m2, 
                          linfct=c("w1group+
                                   w1group:time4 = 0")))
  m2.lhM4Est <- m2.lhM4$confint[1]   # estimate
  m2.lhM4L95 <- m2.lhM4$confint[2]   # lower 95% CI
  m2.lhM4U95 <- m2.lhM4$confint[3]   # upper 95% CI
  m2.male4 <- paste0(rd2(m2.lhM4Est), 
                     " (", 
                     rd2(m2.lhM4L95),
                     " to ",
                     rd2(m2.lhM4U95),
                     ")")

# fit 3-way model, time as factor
  m3 <- lmer(depress.z ~ w1group + dep16Base + time2 + time3 + time4 + 
               w1group*time2 + w1group*time3 + w1group*time4 +
               dep16Base*time2 + dep16Base*time3 + dep16Base*time4 + 
               w1group*dep16Base*time2 + w1group*dep16Base*time3 + 
               w1group*dep16Base*time4 +
               stratum +
               (1 | w1schoolid) + (1 + time | id),
             data=datL)

# remove w1group:dep16Base and re-fit
  m3 <- update(m3, .~. - w1group:dep16Base) 

# linear combination to get effect for depressed at time 2
  m3.lhD2 <- confint(glht(m3, 
                          linfct=c("w1group+
                                   w1group:time2 + 
                                   w1group:dep16Base:time2 = 0")))
  m3.lhD2Est <- m3.lhD2$confint[1]   # estimate
  m3.lhD2L95 <- m3.lhD2$confint[2]   # lower 95% CI
  m3.lhD2U95 <- m3.lhD2$confint[3]   # upper 95% CI
  m3.depressed2 <- paste0(rd2(m3.lhD2Est), 
                          " (", 
                          rd2(m3.lhD2L95),
                          " to ",
                          rd2(m3.lhD2U95),
                          ")")

# linear combination to get effect for depressed at time 3
  m3.lhD3 <- confint(glht(m3, 
                          linfct=c("w1group+
                                   w1group:time3 + 
                                   w1group:dep16Base:time3 = 0")))
  m3.lhD3Est <- m3.lhD3$confint[1]   # estimate
  m3.lhD3L95 <- m3.lhD3$confint[2]   # lower 95% CI
  m3.lhD3U95 <- m3.lhD3$confint[3]   # upper 95% CI
  m3.depressed3 <- paste0(rd2(m3.lhD3Est), 
                          " (", 
                          rd2(m3.lhD3L95),
                          " to ",
                          rd2(m3.lhD3U95),
                          ")")

# linear combination to get effect for depressed at time 4
  m3.lhD4 <- confint(glht(m3, 
                          linfct=c("w1group+
                                   w1group:time4 + 
                                   w1group:dep16Base:time4 = 0")))
  m3.lhD4Est <- m3.lhD4$confint[1]   # estimate
  m3.lhD4L95 <- m3.lhD4$confint[2]   # lower 95% CI
  m3.lhD4U95 <- m3.lhD4$confint[3]   # upper 95% CI
  m3.depressed4 <- paste0(rd2(m3.lhD4Est), 
                          " (", 
                          rd2(m3.lhD4L95),
                          " to ",
                          rd2(m3.lhD4U95),
                          ")")

# linear combination to get effect for non-depressed at time 2
  m3.lhN2 <- confint(glht(m3, 
                          linfct=c("w1group+
                                   w1group:time2 = 0")))
  m3.lhN2Est <- m3.lhN2$confint[1]   # estimate
  m3.lhN2L95 <- m3.lhN2$confint[2]   # lower 95% CI
  m3.lhN2U95 <- m3.lhN2$confint[3]   # upper 95% CI
  m3.notdepressed2 <- paste0(rd2(m3.lhN2Est), 
                             " (", 
                             rd2(m3.lhN2L95),
                             " to ",
                             rd2(m3.lhN2U95),
                             ")")

# linear combination to get effect for non-depressed at time 3
  m3.lhN3 <- confint(glht(m3, 
                          linfct=c("w1group+
                                   w1group:time3 = 0")))
  m3.lhN3Est <- m3.lhN3$confint[1]   # estimate
  m3.lhN3L95 <- m3.lhN3$confint[2]   # lower 95% CI
  m3.lhN3U95 <- m3.lhN3$confint[3]   # upper 95% CI
  m3.notdepressed3 <- paste0(rd2(m3.lhN3Est), 
                             " (", 
                             rd2(m3.lhN3L95),
                             " to ",
                             rd2(m3.lhN3U95),
                             ")")

# linear combination to get effect for non-depressed at time 4
  m3.lhN4 <- confint(glht(m3, 
                          linfct=c("w1group+
                                   w1group:time4 = 0")))
  m3.lhN4Est <- m3.lhN4$confint[1]   # estimate
  m3.lhN4L95 <- m3.lhN4$confint[2]   # lower 95% CI
  m3.lhN4U95 <- m3.lhN4$confint[3]   # upper 95% CI
  m3.notdepressed4 <- paste0(rd2(m3.lhN4Est), 
                             " (", 
                             rd2(m3.lhN4L95),
                             " to ",
                             rd2(m3.lhN4U95),
                             ")")

# create dataframe of estimates
  estTF <- data.frame(matrix(NA, nrow = 0, ncol = 4))
  estTF[1,1] <- "Year 2"
  estTF[1,2] <- m1.lh2Est
  estTF[1,3] <- m1.lh2L95
  estTF[1,4] <- m1.lh2U95
  estTF[2,1] <- "Year 2, Females"
  estTF[2,2] <- m2.lhF2Est
  estTF[2,3] <- m2.lhF2L95
  estTF[2,4] <- m2.lhF2U95
  estTF[3,1] <- "Year 2, Males"
  estTF[3,2] <- m2.lhM2Est
  estTF[3,3] <- m2.lhM2L95
  estTF[3,4] <- m2.lhM2U95
  estTF[4,1] <- "Year 2, Depressed baseline"
  estTF[4,2] <- m3.lhD2Est
  estTF[4,3] <- m3.lhD2L95
  estTF[4,4] <- m3.lhD2U95
  estTF[5,1] <- "Year 2, Not depressed baseline"
  estTF[5,2] <- m3.lhN2Est
  estTF[5,3] <- m3.lhN2L95
  estTF[5,4] <- m3.lhN2U95
  
  estTF[6,1] <- "Year 3"
  estTF[6,2] <- m1.lh3Est
  estTF[6,3] <- m1.lh3L95
  estTF[6,4] <- m1.lh3U95
  estTF[7,1] <- "Year 3, Females"
  estTF[7,2] <- m2.lhF3Est
  estTF[7,3] <- m2.lhF3L95
  estTF[7,4] <- m2.lhF3U95
  estTF[8,1] <- "Year 3, Males"
  estTF[8,2] <- m2.lhM3Est
  estTF[8,3] <- m2.lhM3L95
  estTF[8,4] <- m2.lhM3U95
  estTF[9,1] <- "Year 3, Depressed baseline"
  estTF[9,2] <- m3.lhD3Est
  estTF[9,3] <- m3.lhD3L95
  estTF[9,4] <- m3.lhD3U95
  estTF[10,1] <- "Year 3, Not depressed baseline"
  estTF[10,2] <- m3.lhN3Est
  estTF[10,3] <- m3.lhN3L95
  estTF[10,4] <- m3.lhN3U95
  
  estTF[11,1] <- "Year 4"
  estTF[11,2] <- m1.lh4Est
  estTF[11,3] <- m1.lh4L95
  estTF[11,4] <- m1.lh4U95
  estTF[12,1] <- "Year 4, Females"
  estTF[12,2] <- m2.lhF4Est
  estTF[12,3] <- m2.lhF4L95
  estTF[12,4] <- m2.lhF4U95
  estTF[13,1] <- "Year 4, Males"
  estTF[13,2] <- m2.lhM4Est
  estTF[13,3] <- m2.lhM4L95
  estTF[13,4] <- m2.lhM4U95
  estTF[14,1] <- "Year 4, Depressed baseline"
  estTF[14,2] <- m3.lhD4Est
  estTF[14,3] <- m3.lhD4L95
  estTF[14,4] <- m3.lhD4U95
  estTF[15,1] <- "Year 4, Not depressed baseline"
  estTF[15,2] <- m3.lhN4Est
  estTF[15,3] <- m3.lhN4L95
  estTF[15,4] <- m3.lhN4U95
  
  names(estTF) <- c("Comparison", "Estimate", "Lower", "Upper")

# plot values
  estTFPlot <- 
    structure(list(
      mean  = c(NA, estTF$Estimate), 
      lower = c(NA, estTF$Lower),
      upper = c(NA, estTF$Upper)),
      .Names = c("Estimate", "Lower", "Upper"), 
      row.names = c(NA, -16L), 
      class = "data.frame")

# table values
  estTFText <- cbind(
    c(NA, estTF$Comparison),
    c("Estimate", 
      paste0(rd2(estTF$Estimate),
             " [",
             rd2(estTF$Lower),
             " to ",
             rd2(estTF$Upper),
             "]")))
  maxchar <- max(nchar(estTFText[,2])) # get max characters
  estTFText[,2] <- stringr::str_pad(estTFText[,2], 
                                    maxchar, 
                                    "left") # add leading spaces

# break into separate tables/plots
  estTFText2 <- estTFText[c(1:6),]
  estTFText3 <- estTFText[c(7:11),]
  estTFText4 <- estTFText[c(12:16),]
  
  estTFPlot2 <- estTFPlot[c(1:6),]
  estTFPlot3 <- estTFPlot[c(7:11),]
  estTFPlot4 <- estTFPlot[c(12:16),]

# plot
# https://cran.r-project.org/web/packages/forestplot/vignettes/forestplot.html
  png(here("data and replication files", "output", "TF.png"), width=8, height=5, units="in", res=300)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(3, 1)))
  
  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
  forestplot(estTFText2, 
             estTFPlot2,
             new_page = FALSE,
             is.summary=rep(FALSE,15),
             clip=c(-1,1), 
             xlog=FALSE, 
             col=fpColors(box="black",line="black"),
             boxsize=0.1,
             vertices = TRUE,
             cex=0.5,
             #xlab="",
             xticks = c(-1, -.5, -.25, 0, .25, .5, 1),
             #xticks = 0,
             grid = structure(estTFPlot2$Estimate[2],
                              gp = gpar(lty = 2, col = "#CCCCFF")),
             txt_gp = fpTxtGp(label = list(gpar(fontfamily = "Inconsolata",
                                                cex=.75),
                                           gpar(fontfamily = "Inconsolata",
                                                cex = .75)),
                              xlab = gpar(fontfamily = "Inconsolata", 
                                          cex = .75),
                              ticks = gpar(fontfamily = "Inconsolata", 
                                           cex = .55)))
  popViewport()
  
  pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1))
  forestplot(estTFText3, 
             estTFPlot3,
             new_page = FALSE,
             is.summary=rep(FALSE,15),
             clip=c(-1,1), 
             xlog=FALSE, 
             col=fpColors(box="black",line="black"),
             boxsize=0.1,
             vertices = TRUE,
             cex=0.5,
             #xlab="",
             xticks = c(-1, -.5, -.25, 0, .25, .5, 1),
             grid = structure(estTFPlot3$Estimate[1],
                              gp = gpar(lty = 2, col = "#CCCCFF")),
             txt_gp = fpTxtGp(label = list(gpar(fontfamily = "Inconsolata",
                                                cex=.75),
                                           gpar(fontfamily = "Inconsolata",
                                                cex = .75)),
                              xlab = gpar(fontfamily = "Inconsolata", 
                                          cex = .75),
                              ticks = gpar(fontfamily = "Inconsolata", 
                                           cex = .55)))
  popViewport(1)
  
  pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 1))
  forestplot(estTFText4, 
             estTFPlot4,
             new_page = FALSE,
             is.summary=rep(FALSE,15),
             clip=c(-1,1), 
             xlog=FALSE, 
             col=fpColors(box="black",line="black"),
             boxsize=0.1,
             vertices = TRUE,
             cex=0.5,
             #xlab="Standardized effect sizes, depression severity",
             xticks = c(-1, -.5, -.25, 0, .25, .5, 1),
             grid = structure(estTFPlot4$Estimate[1],
                              gp = gpar(lty = 2, col = "#CCCCFF")),
             txt_gp = fpTxtGp(label = list(gpar(fontfamily = "Inconsolata",
                                                cex=.75),
                                           gpar(fontfamily = "Inconsolata",
                                                cex = .75)),
                              xlab = gpar(fontfamily = "Inconsolata", 
                                          cex = .75),
                              ticks = gpar(fontfamily = "Inconsolata", 
                                           cex = .55)))
  popViewport(2)
  dev.off()