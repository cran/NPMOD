#' @title Non-Parametric Module
#'
#' @author Juan Pablo Paulino-Arce, Nery Sofia Huerta-Pacheco, Victor Manuel Aguirre-Torres
#'
#' @description Non-Parametric Module (NPMOD) is a graphical interface dedicated to the testing of non-parametric data for educational purposes.
#'
#' You can learn more about this package at:
#' http://www.uv.mx/personal/nehuerta/npmod/
#'
#' @details This graphic interface was developed to provide techniques of non-parametric statistics for educational
#' purposes of easy management. On the other hand, this module contains a list of 17 non-parametric techniques that
#' yields results and graphics obtained from data.
#'
#' Note: NPMOD is free software and comes with ABSOLUTELY NO WARRANTY.
#'
#' @return NPMOD is a graphic interface
#'
#' @examples \dontrun{
#' ##Install package
#' library(NPMOD)
#' ##call the package
#' NPMOD()
#' }
#'
#' @references
#' Alan T. Arnholt. BSDA: Basic Statistics and Data Analysis, 2012.
#' R package version 1.01.
#'
#' M Castillo, Arturo Ojeda, Mario Miguel Arturo Castillo, and Mario
#' Miguel Ojeda. Principios de estadistica no parametrica. Number 519.
#' 54 C3P7. 1994.
#'
#' Yosef Cohen and Jeremiah Y Cohen. Statistics and Data with R: An
#' applied approach through examples. John Wiley & Sons, 2008.
#'
#' Jean Dickinson Gibbons and Subhabrata Chakraborti. Nonparametric
#' statistical inference. Springer, 2011.
#'
#' Castor Guisande Gonzalez. Tratamiento de datos. Ediciones Diaz de
#' Santos, 2006.
#'
#' Juergen Gross and Uwe Ligges. nortest: Tests for Normality, 2015.
#' R package version 1.0-4.
#'
#' Maxime Herve. RVAideMemoire: Diverse Basic Statistical and Graphical
#' Functions, 2017. R package version 0.9-63.
#'
#' Hans-Michael Kaltenbach. A concise guide to statistics. Springer Science
#' & Business Media, 2011.
#'
#' Paul H Kvam and Brani Vidakovic. Nonparametric statistics with applications
#' to science and engineering, volume 653. John Wiley & Sons, 2007.
#'
#' J Lopez. Introduccion al analisis de datos con r y r commander en psicologia
#' y educacion, 2012.
#'
#' John H McDonald. Handbook of biological statistics, volume 2. Sparky House
#' Publishing Baltimore, MD, 2009.
#'
#' John Verzani. Based on the iwidgets code of Simon Urbanek, suggestions by Simon Urbanek,
#' Philippe Grosjean, and Michael Lawrence. gWidgets: gWidgets API for building toolkit-independent,
#' interactive GUIs, 2014. R package version 0.0-54.
#'
#' C Perez. Tecnicas estadisticas con spss 12. Aplicaciones al analisis de datos.
#' madrid. editorial pearson prentice hall, 2005.
#'
#' R Core Team. R: A Language and Environment for Statistical Computing. R Foundation
#' for Statistical Computing, Vienna, Austria, 2017.
#'
#' Myra L Samuels, Jeffrey A Witmer, and Andrew Schaffner. Statistics for the life sciences.
#' Pearson education, 2012.
#'
#' Petr Savicky. pspearman: Spearman’s rank correlation test, 2014. R package version 0.3-0.
#'
#' Venkatraman E. Seshan. clinfun: Clinical Trial Design and Data Analysis Functions, 2016.
#' R package version 1.0.13.
#'
#' John Verzani. gWidgetstcltk: Toolkit implementation of gWidgets for tcltk package, 2014.
#' R package version 0.0-55.
#'
#' Dennis D Mendenhall Wackerly, William Scheaffer, Richard L Romo Munoz, et al.
#' Estadistica matematica con aplicaciones. Number 519.5 W3. 2010.
#'
#' Ronald M Weiers. Introduccion a la estadıstica para negocios.
#' Cengage Learning Editores, 2006.
#'
#' Hadley Wickham. readxl: Read Excel Files, 2016. R package version 0.1.1.
#'
#' Dirk Eddelbuettel with contributions by Antoine Lucas, Jarek Tuszynski,
#' Henrik Bengtsson, Simon Urbanek, Mario Frasca, Bryan Lewis, Murray Stokely,
#' Hannes Muehleisen, Duncan Murdoch, Jim Hester, Wush Wu, Qiang Kou, Thierry
#' Onkelinx, Michel Lang, and Viliam Simko. digest: Create Compact Hash Digests
#' of R Objects, 2017. R package version 0.6.12.
#'
#' @export NPMOD
#' @import graphics
#' @import grDevices
#' @import utils
#' @import tcltk
#' @import gWidgets
#' @import readxl
#' @import nortest
#' @import BSDA
#' @import clinfun
#' @import rlang
#' @import pspearman
#' @importFrom RVAideMemoire cochran.qtest
#' @importFrom stats chisq.test cor.test dbinom fisher.test friedman.test kruskal.test ks.test mcnemar.test median qqline qqnorm sd shapiro.test wilcox.test

NPMOD<-function(){

  ent<- new.env()
  options("guiToolkit"="tcltk")

  ##Screen
  x<- gwindow("NPMOD",visible=FALSE,width = 700,height= 610)
  g<- ggroup(horizontal=FALSE, spacing=0, container= x)

  nt <- gnotebook(container=g,width = 700,height= 610)
  g0<- ggroup(horizontal=FALSE, spacing=0, container= nt,label = "NPMOD")
  g1<- ggroup(horizontal=FALSE, spacing=0, container= nt,label = "Result")

  #Global variables
  assign("gdata",NULL, envir =ent)
  assign("Data",NULL, envir =ent)
  assign("Prob",NULL, envir =ent)
  assign("YY1", NULL, envir = ent)
  assign("YY2", NULL, envir = ent)

  #MENU-OPEN
  abrircsv<-function(h,...){
    data<-tk_choose.files()
    data1<-read.csv(data)
    assign("gdata",data1, envir =ent)
  }

  openex<-function(h,...){
    data<-tk_choose.files()
    xlsx<-read_excel(data, sheet=1, col_names=TRUE)
    data2<-as.data.frame(xlsx)
    assign("gdata", data2, envir=ent)
  }

  #VIEW
  ver<-function(h,...){
    gdata<-get("gdata",envir =ent)
    fix(gdata)
  }

  #RESTART
  inicio<-function(h,...){
    dispose(x)
    NPMOD()
  }

  #CLOSE
  close<-function(h,...){
    dispose(x)
  }

  leer1 <- function(h, ...) {
    w1 <- tktoplevel()
    tkwm.title(w1, "Value")
    frame <- ttkframe(w1, padding = c(5, 5, 30, 30))
    tkpack(frame, expand = TRUE, fill = "both")
    nested_frame <- ttkframe(frame)
    tkpack(nested_frame)
    label <- ttklabel(nested_frame, text = "Value:")
    tkpack(label, side = "left")
    text_var <- tclVar("")
    entry <- ttkentry(nested_frame, textvariable = text_var)
    tkpack(entry)
    button_frame <- ttkframe(frame)
    tkpack(button_frame, anchor = "ne")
    button <- ttkbutton(button_frame, text = "Save")
    button1 <- ttkbutton(button_frame, text = "Ok", command = function() tkdestroy(w1))
    sapply(list(button, button1), tkpack)
    yyy <- function() {
      msg <- tclvalue(text_var)
      assign("YY1", msg, envir = ent)
    }
    tkconfigure(button, command = yyy)
    tkconfigure(button1, underline = 0)
  }

  leer2 <- function(h, ...) {
    w1 <- tktoplevel()
    tkwm.title(w1, "Value")
    frame <- ttkframe(w1, padding = c(5, 5, 30, 30))
    tkpack(frame, expand = TRUE, fill = "both")
    nested_frame <- ttkframe(frame)
    tkpack(nested_frame)
    label <- ttklabel(nested_frame, text = "Value:")
    tkpack(label, side = "left")
    text_var <- tclVar("")
    entry <- ttkentry(nested_frame, textvariable = text_var)
    tkpack(entry)
    button_frame <- ttkframe(frame)
    tkpack(button_frame, anchor = "ne")
    button <- ttkbutton(button_frame, text = "Save")
    button1 <- ttkbutton(button_frame, text = "Ok", command = function() tkdestroy(w1))
    sapply(list(button, button1), tkpack)
    yyy <- function() {
      msg <- tclvalue(text_var)
      assign("YY2", msg, envir = ent)
    }
    tkconfigure(button, command = yyy)
    tkconfigure(button1, underline = 0)
  }

  # 1 Sample
  chiaj<-function(h,...){

    w1<- gwindow("Parameters",visible=FALSE,width = 400,height= 400)
    tbl <- glayout(container=w1, horizontal=FALSE)
    gdata<-get("gdata",envir =ent)
    var<- colnames(gdata)
    n1<-ncol(gdata)

    tbl[1,1] <- "Data"
    tbl[1,2] <- (cb1 <- gcombobox(var, container=tbl))
    tbl[2,1] <- "Probability"
    tbl[2,2] <- (cb2 <- gcombobox(var, container=tbl))
    tbl[5,3] <- (a <- gbutton("Cancel", container=tbl))
    tbl[5,4] <- (b <- gbutton("Ok", container=tbl))

    addHandlerClicked(a, handler=function(h,...) {
      dispose(w1)
    })

    addHandlerClicked(b, handler=function(h,...) {
      Data<-svalue(cb1)

      vta<-Data
      for(v in 1:n1){
        if(vta==colnames(gdata[v])){
          a<-gdata[,v]
          c1<-v
        }
      }

      Prob<-svalue(cb2)

      vtb<-Prob
      for(v in 1:n1){
        if(vtb==colnames(gdata[v])){
          a<-gdata[,v]
          c2<-v
        }
      }

      assign("Data",Data, envir =ent)
      assign("Prob",Prob, envir =ent)


      name<-("Chi-squared test -  One sample")
      dev.new()
      hist(gdata[,c1],main="Histogram",xlab="")

      tbl<-glayout(container=g1)
      gseparator(horizontal=TRUE, container=g1)
      outputArea <- gtext(container=g1, expand=TRUE,width = 600,height= 100)
      out <- capture.output(chisq.test(gdata[,c1], p=gdata[,c2]))
      dispose(outputArea)
      if(length(out)>0)
        add(outputArea, out)

      dispose(w1)
    })
    visible(w1) <- TRUE
  }

  kolm<-function(h,...){

    w1<- gwindow("Parameters",visible=FALSE,width = 400,height= 400)
    tbl <- glayout(container=w1, horizontal=FALSE)
    gdata<-get("gdata",envir =ent)
    mean<-get("YY1",envir =ent)
    variance<-get("YY2",envir =ent)
    var<- colnames(gdata)
    n1<-ncol(gdata)

    tbl[1,1] <- "Data"
    tbl[1,2] <- (cb1 <- gcombobox(var, container=tbl))
    tbl[3,1] <- "Mean"
    tbl[3,2] <- (cb2 <- gbutton("Assign", container=tbl,handler=leer1))
    tbl[3,3] <- "Variance"
    tbl[3,4] <- (cb3 <- gbutton("Assign", container=tbl,handler=leer2))
    tbl[5,3] <- (a <- gbutton("Cancel", container=tbl))
    tbl[5,4] <- (b <- gbutton("Ok", container=tbl))

    addHandlerClicked(a, handler=function(h,...) {
      dispose(w1)
    })

    addHandlerClicked(b, handler=function(h,...) {

      Data<-svalue(cb1)
      YY1<-get("YY1",envir =ent)
      mean<-as.numeric(YY1)
      YY2<-get("YY2",envir =ent)
      variance<-as.numeric(YY2)

      vta<-Data
      for(v in 1:n1){
        if(vta==colnames(gdata[v])){
          a<-gdata[,v]
          c1<-v
        }
      }

      assign("Data",Data, envir =ent)
      name<-("Kolmogorov-Smirnov test - One-sample")
      dev.new()
      qqnorm(gdata[,c1])
      qqline(gdata[,c1])
      x1<-round(mean(gdata[,c1]), digits=2)
      x2<-round(sd(gdata[,c1]), digits=2)
      x3<-min(gdata[,c1])
      x4<-max(gdata[,c1])
      legend("bottomright",legend =paste0("Mean:"," ",c(x1)," "," "," ","Sd:"," ",c(x2)," "," "," ","Min:"," ",c(x3)," "," "," ","Max:"," ",c(x4)))

      tbl<-glayout(container=g1)
      gseparator(horizontal=TRUE, container=g1)
      outputArea <- gtext(container=g1, expand=TRUE,width = 600,height= 100)
      out <- capture.output(ks.test(gdata[,c1],"pnorm",mean,variance))
      dispose(outputArea)
      if(length(out)>0)
        add(outputArea, out)

      dispose(w1)
    })
    visible(w1) <- TRUE
  }

  lili<-function(h,...){
    w1<- gwindow("Parameters",visible=FALSE,width = 400,height= 400)
    tbl <- glayout(container=w1, horizontal=FALSE)
    gdata<-get("gdata",envir =ent)
    var<- colnames(gdata)
    n1<-ncol(gdata)

    tbl[1,1] <- "Data"
    tbl[1,2] <- (cb1 <- gcombobox(var, container=tbl))
    tbl[5,3] <- (a <- gbutton("Cancel", container=tbl))
    tbl[5,4] <- (b <- gbutton("Ok", container=tbl))

    addHandlerClicked(a, handler=function(h,...) {
      dispose(w1)
    })

    addHandlerClicked(b, handler=function(h,...) {

      Data<-svalue(cb1)

      vta<-Data
      for(v in 1:n1){
        if(vta==colnames(gdata[v])){
          a<-gdata[,v]
          c1<-v
        }
      }

      assign("Data",Data, envir =ent)
      name<-("Lilliefors-Kolmogorov-Smirnov test - Normality test")
      dev.new()
      qqnorm(gdata[,c1])
      qqline(gdata[,c1])
      x1<-round(mean(gdata[,c1]), digits=2)
      x2<-round(sd(gdata[,c1]), digits=2)
      x3<-min(gdata[,c1])
      x4<-max(gdata[,c1])
      legend("bottomright",legend =paste0("Mean:"," ",c(x1)," "," "," ","Sd:"," ",c(x2)," "," "," ","Min:"," ",c(x3)," "," "," ","Max:"," ",c(x4)))

      tbl<-glayout(container=g1)
      gseparator(horizontal=TRUE, container=g1)
      outputArea <- gtext(container=g1, expand=TRUE,width = 600,height= 100)
      out <- capture.output(lillie.test(gdata[,c1]))
      dispose(outputArea)
      if(length(out)>0)
        add(outputArea, out)

      dispose(w1)
    })
    visible(w1) <- TRUE
  }

  shapiro<-function(h,...){

    w1<- gwindow("Parameters",visible=FALSE,width = 400,height= 400)
    tbl <- glayout(container=w1, horizontal=FALSE)
    gdata<-get("gdata",envir =ent)
    var<- colnames(gdata)
    n1<-ncol(gdata)

    tbl[1,1] <- "Data"
    tbl[1,2] <- (cb1 <- gcombobox(var, container=tbl))
    tbl[5,3] <- (a <- gbutton("Cancel", container=tbl))
    tbl[5,4] <- (b <- gbutton("Ok", container=tbl))

    addHandlerClicked(a, handler=function(h,...) {
      dispose(w1)
    })

    addHandlerClicked(b, handler=function(h,...) {

      Data<-svalue(cb1)

      vta<-Data
      for(v in 1:n1){
        if(vta==colnames(gdata[v])){
          a<-gdata[,v]
          c1<-v
        }
      }

      assign("Data",Data, envir =ent)
      name<-("Shapiro Wilk test - One sample")
      dev.new()
      qqnorm(gdata[,c1])
      x1<-round(mean(gdata[,c1]), digits=2)
      x2<-round(sd(gdata[,c1]), digits=2)
      x3<-min(gdata[,c1])
      x4<-max(gdata[,c1])
      legend("bottomright",legend =paste0("Mean:"," ",c(x1)," "," "," ","Sd:"," ",c(x2)," "," "," ","Min:"," ",c(x3)," "," "," ","Max:"," ",c(x4)))

      tbl<-glayout(container=g1)
      gseparator(horizontal=TRUE, container=g1)
      outputArea <- gtext(container=g1, expand=TRUE,width = 600,height= 100)
      out <- capture.output(shapiro.test(gdata[,c1]))
      dispose(outputArea)
      if(length(out)>0)
        add(outputArea, out)

      dispose(w1)
    })
    visible(w1) <- TRUE
  }

  signo<-function(h,...){

    w1<- gwindow("Parameters",visible=FALSE,width = 400,height= 400)
    tbl <- glayout(container=w1, horizontal=FALSE)
    gdata<-get("gdata",envir =ent)
    var<- colnames(gdata)
    n1<-ncol(gdata)
    al<-c("greater","less","two.sided")

    tbl[1,1] <- "Data"
    tbl[1,2] <- (cb1 <- gcombobox(var, container=tbl))
    tbl[2,1] <- "Median"
    tbl[2,2] <- (median<- gbutton("Assign", container=tbl,handler=leer1))
    tbl[1,3] <- "Alternative"
    tbl[1,4] <- (alt <- gcombobox(al, container=tbl))
    tbl[2,3] <- "Confidence level"
    tbl[2,4] <- (cflv <- gbutton("Assign", container=tbl,handler=leer2))
    tbl[5,3] <- (a <- gbutton("Cancel", container=tbl))
    tbl[5,4] <- (b <- gbutton("Ok", container=tbl))

    addHandlerClicked(a, handler=function(h,...) {
      dispose(w1)
    })

    addHandlerClicked(b, handler=function(h,...) {
      Data<-svalue(cb1)
      alter<-svalue(alt)
      MD<-get("YY1",envir =ent)
      CON<-get("YY2",envir =ent)
      md<-as.numeric(MD)
      cl<-as.numeric(CON)

      vta<-Data
      for(v in 1:n1){
        if(vta==colnames(gdata[v])){
          a<-gdata[,v]
          c1<-v
        }
      }

      assign("Data",Data, envir =ent)

      tbl<-glayout(container=g1)
      gseparator(horizontal=TRUE, container=g1)
      outputArea <- gtext(container=g1, expand=TRUE,width = 600,height= 250)
      out <- capture.output(SIGN.test(gdata[,c1], md = md, alternative = alter, conf.level=cl))

      dispose(outputArea)
      if(length(out)>0)
        add(outputArea, out)

      dispose(w1)
    })
    visible(w1) <- TRUE
  }

  # 2 Sample

  macnemardos<-function(h,...){

    w1<- gwindow("Parameters",visible=FALSE,width = 400,height= 400)
    tbl <- glayout(container=w1, horizontal=FALSE)
    gdata<-get("gdata",envir =ent)
    var<- colnames(gdata)
    n1<-ncol(gdata)

    tbl[1,1] <- "Data"
    tbl[1,2] <- (cb1 <- gcombobox(var, container=tbl))
    tbl[5,3] <- (a <- gbutton("Cancel", container=tbl))
    tbl[5,4] <- (b <- gbutton("Ok", container=tbl))

    addHandlerClicked(a, handler=function(h,...) {
      dispose(w1)
    })

    addHandlerClicked(b, handler=function(h,...) {

      Data<-svalue(cb1)

      vta<-Data
      for(v in 1:n1){
        if(vta==colnames(gdata[v])){
          a<-gdata[,v]
          c1<-v
        }
      }
      mat1<-matrix(gdata[,c1],ncol=2, nrow=2)

      assign("Data",Data, envir =ent)

      name<-("McNemar's Chi-squared -  Two sample")

      tbl<-glayout(container=g1)
      gseparator(horizontal=TRUE, container=g1)
      outputArea <- gtext(container=g1, expand=TRUE,width = 600,height= 150)
      out <- capture.output(mcnemar.test(mat1, y=NULL, correct=FALSE))
      dispose(outputArea)
      if(length(out)>0)
        add(outputArea, out)

      dispose(w1)
    })
    visible(w1) <- TRUE
  }

  signosdos<-function(h,...){


    w1<- gwindow("Parameters",visible=FALSE,width = 400,height= 400)
    tbl <- glayout(container=w1, horizontal=FALSE)
    gdata<-get("gdata",envir =ent)
    var<- colnames(gdata)
    n1<-ncol(gdata)
    al<-c("greater","less","two.sided")

    tbl[1,1] <- "Data 1"
    tbl[1,2] <- (cb1 <- gcombobox(var, container=tbl))
    tbl[1,3] <- "Data 2"
    tbl[1,4] <- (cb2 <- gcombobox(var, container=tbl))
    tbl[3,1] <- "Median"
    tbl[3,2] <- (median<- gbutton("Assign", container=tbl,handler=leer1))
    tbl[2,1] <- "Alternative"
    tbl[2,2] <- (alt <- gcombobox(al, container=tbl))
    tbl[3,3] <- "Confidence level"
    tbl[3,4] <- (cflv <- gbutton("Assign", container=tbl,handler=leer2))
    tbl[5,3] <- (a <- gbutton("Cancel", container=tbl))
    tbl[5,4] <- (b <- gbutton("Ok", container=tbl))

    addHandlerClicked(a, handler=function(h,...) {
      dispose(w1)
    })

    addHandlerClicked(b, handler=function(h,...) {

      Data<-svalue(cb1)
      y<-svalue(cb2)
      alter<-svalue(alt)
      MD<-get("YY1",envir =ent)
      CON<-get("YY2",envir =ent)
      md<-as.numeric(MD)
      cl<-as.numeric(CON)

      vta<-Data
      for(v in 1:n1){
        if(vta==colnames(gdata[v])){
          a<-gdata[,v]
          c1<-v
        }
      }

      y<-svalue(cb2)

      vtb<-y
      for(v in 1:n1){
        if(vtb==colnames(gdata[v])){
          a<-gdata[,v]
          c2<-v
        }
      }

      assign("Data",Data, envir =ent)
      assign("y",y, envir =ent)

      name<-("Sign Test Dependent -  Two sample")

      tbl<-glayout(container=g1)
      gseparator(horizontal=TRUE, container=g1)
      outputArea <- gtext(container=g1, expand=TRUE,width = 600,height= 250)
      out <- capture.output(SIGN.test(gdata[,c1], y=gdata[,c2], md = md, alternative = alter, conf.level=cl))

      dispose(outputArea)
      if(length(out)>0)
        add(outputArea, out)

      dispose(w1)
    })
    visible(w1) <- TRUE
  }

  wilcoxon<-function(h,...){

    w1<- gwindow("Parameters",visible=FALSE,width = 400,height= 400)
    tbl <- glayout(container=w1, horizontal=FALSE)
    gdata<-get("gdata",envir =ent)
    var<- colnames(gdata)
    n1<-ncol(gdata)
    al<-c("greater","less","two.sided")

    tbl[1,1] <- "Data 1"
    tbl[1,2] <- (cb1 <- gcombobox(var, container=tbl))
    tbl[1,3] <- "Data 2"
    tbl[1,4] <- (cb2 <- gcombobox(var, container=tbl))
    tbl[2,1] <- "Alternative"
    tbl[2,2] <- (alt <- gcombobox(al, container=tbl))
    tbl[5,3] <- (a <- gbutton("Cancel", container=tbl))
    tbl[5,4] <- (b <- gbutton("Ok", container=tbl))

    addHandlerClicked(a, handler=function(h,...) {
      dispose(w1)
    })

    addHandlerClicked(b, handler=function(h,...) {

      Data<-svalue(cb1)
      alter<-svalue(alt)
      y<-svalue(cb2)

      vta<-Data
      for(v in 1:n1){
        if(vta==colnames(gdata[v])){
          a<-gdata[,v]
          c1<-v
        }
      }

      vtb<-y
      for(v in 1:n1){
        if(vtb==colnames(gdata[v])){
          a<-gdata[,v]
          c2<-v
        }
      }

      assign("Data",Data, envir =ent)
      assign("y",y, envir =ent)

      dev.new()
      boxplot(gdata[,c1],gdata[,c2])
      x1<-round(median(gdata[,c1]), digits=2)
      x2<-round(median(gdata[,c2]), digits=2)
      x3<-min(gdata[,c1])
      x4<-min(gdata[,c2])
      x5<-max(gdata[,c1])
      x6<-max(gdata[,c2])
      legend("bottomright",legend =paste0(c(1,2)," - ","Median:"," ",c(x1,x2)," "," "," ","Min:"," ",c(x3,x4)," "," "," ","Max:"," ",c(x5,x6)),cex=0.8)

      tbl<-glayout(container=g1)
      gseparator(horizontal=TRUE, container=g1)
      outputArea <- gtext(container=g1, expand=TRUE,width = 600,height= 150)
      out <- capture.output(wilcox.test(gdata[,c1], y=gdata[,c2], alternative = alter, paired = TRUE, exact=FALSE))
      dispose(outputArea)
      if(length(out)>0)
        add(outputArea, out)

      dispose(w1)
    })
    visible(w1) <- TRUE
  }

  fisherdos<-function(h,...){

    w1<- gwindow("Parameters",visible=FALSE,width = 400,height= 400)
    tbl <- glayout(container=w1, horizontal=FALSE)
    gdata<-get("gdata",envir =ent)
    var<- colnames(gdata)
    n1<-ncol(gdata)
    al<-c("greater","less","two.sided")
    #Entrada de parametros
    tbl[1,1] <- "Data 1"
    tbl[1,2] <- (cb1 <- gcombobox(var, container=tbl))
    tbl[2,1] <- "Alternative"
    tbl[2,2] <- (alt <- gcombobox(al, container=tbl))
    tbl[5,3] <- (a <- gbutton("Cancel", container=tbl))
    tbl[5,4] <- (b <- gbutton("Ok", container=tbl))

    addHandlerClicked(a, handler=function(h,...) {
      dispose(w1)
    })

    addHandlerClicked(b, handler=function(h,...) {

      Data<-svalue(cb1)
      alter<-svalue(alt)

      vta<-Data
      for(v in 1:n1){
        if(vta==colnames(gdata[v])){
          a<-gdata[,v]
          c1<-v
        }
      }

      mat1<-matrix(gdata[,c1],ncol=2, nrow=2)

      assign("Data",Data, envir =ent)

      tbl<-glayout(container=g1)
      gseparator(horizontal=TRUE, container=g1)
      outputArea <- gtext(container=g1, expand=TRUE,width = 600,height= 150)
      out <- capture.output(fisher.test(mat1, y=NULL, alternative=alter))
      dispose(outputArea)
      if(length(out)>0)
        add(outputArea, out)

      dispose(w1)
    })
    visible(w1) <- TRUE
  }

  jidos<-function(h,...){

    w1<- gwindow("Parameters",visible=FALSE,width = 400,height= 400)
    tbl <- glayout(container=w1, horizontal=FALSE)
    gdata<-get("gdata",envir =ent)
    var<- colnames(gdata)
    n1<-ncol(gdata)

    tbl[1,1] <- "Data"
    tbl[1,2] <- (cb1 <- gcombobox(var, container=tbl))
    tbl[2,1] <- "Number of Columns"
    tbl[2,2] <- (nc<- gbutton("Assign", container=tbl,handler=leer1))
    tbl[2,3] <- "Number of Rows"
    tbl[2,4] <- (nr<- gbutton("Assign", container=tbl,handler=leer2))
    tbl[5,3] <- (a <- gbutton("Cancel", container=tbl))
    tbl[5,4] <- (b <- gbutton("Ok", container=tbl))

    addHandlerClicked(a, handler=function(h,...) {
      dispose(w1)
    })

    addHandlerClicked(b, handler=function(h,...) {

      Data<-svalue(cb1)
      nc<-get("YY1",envir =ent)
      nco<-as.numeric(nc)
      nr<-get("YY2",envir =ent)
      nro<-as.numeric(nr)

      vta<-Data
      for(v in 1:n1){
        if(vta==colnames(gdata[v])){
          a<-gdata[,v]
          c1<-v
        }
      }
      mat1<-matrix(gdata[,c1],ncol=nco, nrow=nro)

      assign("Data",Data, envir =ent)

      chisq.test(mat1, y=NULL)


      tbl<-glayout(container=g1)
      gseparator(horizontal=TRUE, container=g1)
      outputArea <- gtext(container=g1, expand=TRUE,width = 600,height= 200)
      out <- capture.output(chisq.test(mat1, y=NULL))
      dispose(outputArea)
      if(length(out)>0)
        add(outputArea, out)

      dispose(w1)
    })
    visible(w1) <- TRUE
  }

  mann<-function(h,...){

    w1<- gwindow("Parameters",visible=FALSE,width = 400,height= 400)
    tbl<- glayout(container=w1, horizontal=FALSE)
    gdata<-get("gdata",envir =ent)
    var<- colnames(gdata)
    n1<-ncol(gdata)
    al<-c("greater","less","two.sided")

    tbl[1,1] <- "Data 1"
    tbl[1,2] <- (cb1 <- gcombobox(var, container=tbl))
    tbl[1,3] <- "Data 2"
    tbl[1,4] <- (cb2 <- gcombobox(var, container=tbl))
    tbl[2,1] <- "Alternative"
    tbl[2,2] <- (alt <- gcombobox(al, container=tbl))
    tbl[5,2] <- (a <- gbutton("Cancel", container=tbl))
    tbl[5,3] <- (b <- gbutton("Ok", container=tbl))

    addHandlerClicked(a, handler=function(h,...) {
      dispose(w1)
    })

    addHandlerClicked(b, handler=function(h,...) {

      Data<-svalue(cb1)
      y<-svalue(cb2)
      alter<-svalue(alt)

      vta<-Data
      for(v in 1:n1){
        if(vta==colnames(gdata[v])){
          a<-gdata[,v]
          c1<-v
        }
      }

      vtb<-y
      for(v in 1:n1){
        if(vtb==colnames(gdata[v])){
          a<-gdata[,v]
          c2<-v
        }
      }

      assign("Data",Data, envir =ent)
      assign("y",y, envir =ent)

      dev.new()
      boxplot(gdata[,c1],gdata[,c2])
      x1<-round(median(gdata[,c1]), digits=2)
      x2<-round(median(gdata[,c2]), digits=2)
      x3<-min(gdata[,c1])
      x4<-min(gdata[,c2])
      x5<-max(gdata[,c1])
      x6<-max(gdata[,c2])
      legend("bottomright",legend =paste0(c(1,2)," - ","Median:"," ",c(x1,x2)," "," "," ","Min:"," ",c(x3,x4)," "," "," ","Max:"," ",c(x5,x6)),cex=.8)

      tbl<-glayout(container=g1)
      gseparator(horizontal=TRUE, container=g1)
      outputArea <- gtext(container=g1, expand=TRUE,width = 600,height= 150)
      out <- capture.output(wilcox.test(gdata[,c1], y=gdata[,c2], alternative=alter, correct=FALSE, mu=0))
      dispose(outputArea)
      if(length(out)>0)
        add(outputArea, out)

      dispose(w1)
    })
    visible(w1) <- TRUE
  }

  # Measures of Association

  spearman<-function(h,...){

    w1<- gwindow("Parameters",visible=FALSE,width = 400,height= 400)
    tbl <- glayout(container=w1, horizontal=FALSE)
    gdata<-get("gdata",envir =ent)
    var<- colnames(gdata)
    n1<-ncol(gdata)
    al<-c("greater","less","two.sided")

    tbl[1,1] <- "Data 1"
    tbl[1,2] <- (cb1 <- gcombobox(var, container=tbl))
    tbl[1,3] <- "Data 2"
    tbl[1,4] <- (cb2 <- gcombobox(var, container=tbl))
    tbl[2,1] <- "Alternative"
    tbl[2,2] <- (alt <- gcombobox(al, container=tbl))
    tbl[5,3] <- (a <- gbutton("Cancel", container=tbl))
    tbl[5,4] <- (b <- gbutton("Ok", container=tbl))

    addHandlerClicked(a, handler=function(h,...) {
      dispose(w1)
    })

    addHandlerClicked(b, handler=function(h,...) {

      Data<-svalue(cb1)
      y<-svalue(cb2)
      alter<-svalue(alt)

      vta<-Data
      for(v in 1:n1){
        if(vta==colnames(gdata[v])){
          a<-gdata[,v]
          c1<-v
        }
      }

      vtb<-y
      for(v in 1:n1){
        if(vtb==colnames(gdata[v])){
          a<-gdata[,v]
          c2<-v
        }
      }

      assign("Data",Data, envir =ent)
      assign("y",y, envir =ent)

      na1<-Data
      na2<-y
      dev.new()
      plot(gdata[,c1], gdata[,c2],xlab=na1,ylab=na2)

      tbl<-glayout(container=g1)
      gseparator(horizontal=TRUE, container=g1)
      outputArea <- gtext(container=g1, expand=TRUE,width = 600,height= 150)
      out <- capture.output(cor.test(gdata[,c1], y=gdata[,c2], alternative=alter, method="spearman"))
      dispose(outputArea)
      if(length(out)>0)
        add(outputArea, out)

      dispose(w1)
    })
    visible(w1) <- TRUE
  }

  kendall<-function(h,...){

    w1<- gwindow("Parameters",visible=FALSE,width = 400,height= 400)
    tbl <- glayout(container=w1, horizontal=FALSE)
    gdata<-get("gdata",envir =ent)
    var<- colnames(gdata)
    n1<-ncol(gdata)
    al<-c("greater","less","two.sided")

    tbl[1,1] <- "Data 1"
    tbl[1,2] <- (cb1 <- gcombobox(var, container=tbl))
    tbl[2,1] <- "Data 2"
    tbl[2,2] <- (cb2 <- gcombobox(var, container=tbl))
    tbl[5,2] <- (a <- gbutton("Cancel", container=tbl))
    tbl[5,3] <- (b <- gbutton("Ok", container=tbl))

    addHandlerClicked(a, handler=function(h,...) {
      dispose(w1)
    })

    addHandlerClicked(b, handler=function(h,...) {

      Data<-svalue(cb1)
      y<-svalue(cb2)

      vta<-Data
      for(v in 1:n1){
        if(vta==colnames(gdata[v])){
          a<-gdata[,v]
          c1<-v
        }
      }

      vtb<-y
      for(v in 1:n1){
        if(vtb==colnames(gdata[v])){
          a<-gdata[,v]
          c2<-v
        }
      }

      assign("Data",Data, envir =ent)
      assign("y",y, envir =ent)

      na1<-Data
      na2<-y

      dev.new()
      plot(gdata[,c1], gdata[,c2],xlab =na1 ,ylab =na2 )

      tbl<-glayout(container=g1)
      gseparator(horizontal=TRUE, container=g1)
      outputArea <- gtext(container=g1, expand=TRUE,width = 600,height= 150)
      out <- capture.output(cor.test(gdata[,c1], y=gdata[,c2], method="kendall"))
      dispose(outputArea)
      if(length(out)>0)
        add(outputArea, out)

      dispose(w1)
    })
    visible(w1) <- TRUE
  }

  # K Sample

  wallis<-function(h,...){

    w1<- gwindow("Parameters",visible=FALSE,width = 400,height= 400)
    tbl <- glayout(container=w1, horizontal=FALSE)
    gdata<-get("gdata",envir =ent)
    var<- colnames(gdata)
    n1<-ncol(gdata)

    tbl[1,1] <- "Data"
    tbl[1,2] <- (cb1 <- gcombobox(var, container=tbl))
    tbl[2,1] <- "Treatment"
    tbl[2,2] <- (cb2 <- gcombobox(var, container=tbl))
    tbl[5,2] <- (a <- gbutton("Cancel", container=tbl))
    tbl[5,3] <- (b <- gbutton("Ok", container=tbl))

    addHandlerClicked(a, handler=function(h,...) {
      dispose(w1)
    })

    addHandlerClicked(b, handler=function(h,...) {

      Data<-svalue(cb1)
      g<-svalue(cb2)

      vta<-Data
      for(v in 1:n1){
        if(vta==colnames(gdata[v])){
          a<-gdata[,v]
          c1<-v
        }
      }

      vtb<-g
      for(v in 1:n1){
        if(vtb==colnames(gdata[v])){
          a<-gdata[,v]
          c2<-v
        }
      }

      assign("Data",Data, envir =ent)
      assign("g",g, envir =ent)

      dev.new()
      boxplot(gdata[,c1]~gdata[,c2])

      tbl<-glayout(container=g1)
      gseparator(horizontal=TRUE, container=g1)
      outputArea <- gtext(container=g1, expand=TRUE,width = 600,height= 150)
      out <- capture.output(kruskal.test(gdata[,c1], g=gdata[,c2]))
      dispose(outputArea)
      if(length(out)>0)
        add(outputArea, out)

      dispose(w1)
    })
    visible(w1) <- TRUE
  }

  terpstra<-function(h,...){

    w1<- gwindow("Parameters",visible=FALSE,width = 400,height= 400)
    tbl <- glayout(container=w1, horizontal=FALSE)
    gdata<-get("gdata",envir =ent)
    var<- colnames(gdata)
    n1<-ncol(gdata)
    al<- c("two.sided", "increasing","decreasing")

    tbl[1,1] <- "Data"
    tbl[1,2] <- (cb1 <- gcombobox(var, container=tbl))
    tbl[1,3] <- "Group"
    tbl[1,4] <- (cb2 <- gcombobox(var, container=tbl))
    tbl[2,1] <- "Alternative"
    tbl[2,2] <- (alt <- gcombobox(al, container=tbl))
    tbl[2,3] <- "Permutations"
    tbl[2,4] <- (per<- gbutton("Assign", container=tbl,handler=leer1))
    tbl[5,3] <- (a <- gbutton("Cancel", container=tbl))
    tbl[5,4] <- (b <- gbutton("Ok", container=tbl))

    addHandlerClicked(a, handler=function(h,...) {
      dispose(w1)
    })

    addHandlerClicked(b, handler=function(h,...) {

      Data<-svalue(cb1)
      g<-svalue(cb2)
      alter<-svalue(alt)
      per<-get("YY1",envir =ent)
      perg<-as.numeric(per)

      vta<-Data
      for(v in 1:n1){
        if(vta==colnames(gdata[v])){
          a<-gdata[,v]
          c1<-v
        }
      }

      vtb<-g
      for(v in 1:n1){
        if(vtb==colnames(gdata[v])){
          a<-gdata[,v]
          c2<-v
        }
      }

      assign("Data",Data, envir =ent)
      assign("g",g, envir =ent)

      tbl<-glayout(container=g1)
      gseparator(horizontal=TRUE, container=g1)
      outputArea <- gtext(container=g1, expand=TRUE,width = 600,height= 150)
      out <- capture.output(jonckheere.test(gdata[,c1], g=gdata[,c2], alternative=alter, nperm=perg))
      dispose(outputArea)
      if(length(out)>0)
        add(outputArea, out)

      dispose(w1)
    })
    visible(w1) <- TRUE
  }

  fri<-function(h,...){

    w1<- gwindow("Parameters",visible=FALSE,width = 400,height= 400)
    tbl <- glayout(container=w1, horizontal=FALSE)
    gdata<-get("gdata",envir =ent)
    var<- colnames(gdata)
    n1<-ncol(gdata)

    tbl[1,1] <- "Data"
    tbl[1,2] <- (cb1 <- gcombobox(var, container=tbl))
    tbl[2,1] <- "Group"
    tbl[2,2] <- (cb2 <- gcombobox(var, container=tbl))
    tbl[3,1] <- "Blocks"
    tbl[3,2] <- (cb3 <- gcombobox(var, container=tbl))
    tbl[4,1] <- (a <- gbutton("Cancel", container=tbl))
    tbl[4,2] <- (b <- gbutton("Ok", container=tbl))

    addHandlerClicked(a, handler=function(h,...) {
      dispose(w1)
    })

    addHandlerClicked(b, handler=function(h,...) {

      Data<-svalue(cb1)
      g<-svalue(cb2)
      b<-svalue(cb3)

      vta<-Data
      for(v in 1:n1){
        if(vta==colnames(gdata[v])){
          a<-gdata[,v]
          c1<-v
        }
      }

      vtb<-g
      for(v in 1:n1){
        if(vtb==colnames(gdata[v])){
          a<-gdata[,v]
          c2<-v
        }
      }

      vtb<-b
      for(v in 1:n1){
        if(vtb==colnames(gdata[v])){
          a<-gdata[,v]
          c3<-v
        }
      }

      assign("Data",Data, envir =ent)
      assign("g",g, envir =ent)
      assign("b",b, envir=ent)

      friedman.test(gdata[,c1], g=gdata[,c2], b=gdata[,c3])

      tbl<-glayout(container=g1)
      gseparator(horizontal=TRUE, container=g1)
      outputArea <- gtext(container=g1, expand=TRUE,width = 600,height= 150)
      out <- capture.output (friedman.test(gdata[,c1], g=gdata[,c2], b=gdata[,c3]))
      dispose(outputArea)
      if(length(out)>0)
        add(outputArea, out)

      dispose(w1)
    })
    visible(w1) <- TRUE
  }

  ccro<-function(h,...){

    w1<- gwindow("Parameters",visible=FALSE,width = 400,height= 400)
    tbl <- glayout(container=w1, horizontal=FALSE)
    gdata<-get("gdata",envir =ent)
    var<- colnames(gdata)
    n1<-ncol(gdata)

    tbl[1,1] <- "Data"
    tbl[1,2] <- (cb1 <- gcombobox(var, container=tbl))
    tbl[2,1] <- "Group"
    tbl[2,2] <- (cb2 <- gcombobox(var, container=tbl))
    tbl[3,1] <- "Blocks"
    tbl[3,2] <- (cb3 <- gcombobox(var, container=tbl))
    tbl[4,1] <- (a <- gbutton("Cancel", container=tbl))
    tbl[4,2] <- (b <- gbutton("Ok", container=tbl))

    addHandlerClicked(a, handler=function(h,...) {
      dispose(w1)
    })

    addHandlerClicked(b, handler=function(h,...) {

      Data<-svalue(cb1)
      g<-svalue(cb2)
      b<-svalue(cb3)

      vta<-Data
      for(v in 1:n1){
        if(vta==colnames(gdata[v])){
          a<-gdata[,v]
          c1<-v
        }
      }

      vtb<-g
      for(v in 1:n1){
        if(vtb==colnames(gdata[v])){
          a<-gdata[,v]
          c2<-v
        }
      }

      vtb<-b
      for(v in 1:n1){
        if(vtb==colnames(gdata[v])){
          a<-gdata[,v]
          c3<-v
        }
      }

      assign("Data",Data, envir =ent)
      assign("g",g, envir =ent)
      assign("b",b, envir=ent)
      X<-factor(gdata[,c1])
      G<-factor(gdata[,c2])
      B<-factor(gdata[,c3])

      cochran.qtest(X~G|B)

      tbl<-glayout(container=g1)
      gseparator(horizontal=TRUE, container=g1)
      outputArea <- gtext(container=g1, expand=TRUE,width = 600,height= 150)
      out <- capture.output(cochran.qtest(X~G|B))
      dispose(outputArea)
      if(length(out)>0)
        add(outputArea, out)

      dispose(w1)
    })
    visible(w1) <- TRUE
  }

  # Text

  xone<-function(h,...){
    w1<- gwindow("Definition",visible=TRUE,width = 400,height= 400)
    tbl <- glayout(container=w1, horizontal=TRUE)

    text<-"  ONE SAMPLE

    1) Sign test

    The sign test is one of the oldest non-parametric techniques that has been used. It was reported in the literature from 1710 (Arbuthnott).
    Is called test of the sign to a sample that can be transformed to a set of more signs and minus signs. To test use of the magnitude of the results or the difference between each pair of them, but just your sign or its meaning is not. The sign test uses a normal distribution of p.

    2) Kolmogorov Smirnov test

    Kolmogorov test is a nonparametric test used to test the degree of concordance between the empirical data of the sample distribution and some specific theoretical distribution. This is a contrast of fit of a given a given continuous distribution sample distribution. This test is an alternative to the Chi-square where is occupied for more than 30 large samples.

    3) Shapiro Wilk test

    The Shapiro-Wilk test is one of the most efficient and used to verify the normality of a variable. This test provides an alternative to try if there is or not a normal distribution in the data, comparing the variance of the data with the expected variance of a normal distribution.

    4) Lilliefors test

    The test of Lilliefors, named after Hubert Lilliefors (Professor of statistics at George Washington University), is a test of normality based on the Kolmogorov-Smirnov test. Used to test the null hypothesis that the data come from a normally distributed population when the null hypothesis does not specify that normal distribution is does not specify the expected value and the variance of the distribution.

    5) Chi squared test

    The Chi-square statistical which has probability distribution of the same name, serves to put to test hypotheses concerning distributions of frequencies. In general terms, this test contrasts frequencies observed with frequencies expected according to the null hypothesis. This article describes the use of the statistical Chi-square to test the association between two variables using a hypothetical situation and simulated data. Then describes its use to evaluate how good can be a theoretical distribution, when it is intended to represent the actual distribution of the data of a given sample. This is called to evaluate the goodness of fit. Test the goodness of fit is to see to what extent the observed data conform to expected or theoretical distribution.

    "
    cb1 <- gtext(text,container=w1)
    size(cb1)<-c(350,350)

  }

  xtwo<-function(h,...){
    w1<- gwindow("Definicion",visible=TRUE,width = 400,height= 400)
    tbl <- glayout(container=w1, horizontal=TRUE)

    text<-"  TWO SAMPLE

    1) Sign test

    The sign two-sample test, they are from normal populations of small samples and that also are dependent on each other, using t-student test. But when determining a case of two dependent, small sample sizes but not normal populations samples, paired t-student used this test, which is equivalent to the test.

    2) Wilcoxon signed rank test

    The Wilcoxon ranks test is a non-parametric method to compare two paired samples. This test is more powerful than the proof of the signs. Like the signs test, Wilcoxon test does not require that data belong to a population with a normal distribution.

    3) McNemar's test

    The McNemar test is applied in 2 x 2 tables to find significance between changes in before and after, cases where each individual (case control) perform you measurements in nominal or ordinal scale.

    4) Mann Whitney U test

    It is a test for independent samples where compares the central measure of each in order to find differences. Samples which are not necessarily used must be of the same size.
    Conditions for the test, Mann-Whitney U is that should be two independent samples and are also on a ordinal scale.

    5) Fisher's exact test

    Fisher's exact test is used when you have two variables on a nominal scale, represented in a table of R x C, where R is the number of rows and C the number of columns. This test is more accurate that Chi-squared test for contingency tables when the expected values are small. Commonly used for 2 x 2 contingency tables.

    6) Chi squared test

    The Chi-square test is the most common technique to contrast the hypothesis of independence. The data used must find an entry double table where rows and columns contain the frequencies of each of the categories to analyze. In order to use this test, the expected values should be greater than five, if instead the Yates correction should be applied.

    "
    cb1 <- gtext(text,container=w1)
    size(cb1)<-c(350,350)

  }

  xk<-function(h,...){
    w1<- gwindow("Definition",visible=TRUE,width = 400,height= 400)
    tbl <- glayout(container=w1, horizontal=TRUE)

    text<-"  K SAMPLE

    1) Jonckheere Terpstra test

    The Kruskal-Wallis test, the hypothesis for k groups showing if such groups are equal or at least one is different. But sometimes the researcher may want a more specific result for example, an experiment where analyzed different doses of drugs in a learning task, wanting to test whether the effect is the same, or there is an ascending or descending effect. That is, making Jonckheere-Terpstra test for ordered levels, since it proves the hypothesis that groups are sorted in a specific way either upward, downward or no.

    2) Kruskal Wallis Test

    This test is similar to the analysis of univariate variance only that in the nonparametric case, where the main objective is to know whether it exists or not statistically significant differences between k treatments (groups) independent, with respect to a variable. This test is not required the fulfilment of normality in the data, as well as the homogeneity of variances in groups. Therefore, it is recommended to use this test when you cannot use the ANOVA.

    3) Friedman test

    The Friedman test is used for when you want to test the hypothesis that the probability of k distributions treatments are identical against the alternative that are different. That is, every combination of treatment, block are the same or different. This test is equivalent to the design in complete randomized blocks, with the difference that is not required the fulfilment of assumptions of normality and homogeneity. In addition, there is no repetition. One of the assumptions of this test is that treatments are assigned randomly to the experimental units within each block. Being that the number of blocks (b) or the number of treatments (k) exceed five.

    4) Cochran's Q test

    Cochran test is essentially a random complete block design, with a characteristic in particular, that the answer is dichotomous and contains only replicates. That is, every combination of treatment with block has a 0 or 1 as a response.

    "
    cb1 <- gtext(text,container=w1)
    size(cb1)<-c(350,350)
  }

  xaso<-function(h,...){
    w1<- gwindow("Definition",visible=TRUE,width = 400,height= 400)
    tbl <- glayout(container=w1, horizontal=TRUE)

    text<-"  MEASURES OF ASSOCIATION

    1) Spearman's rank correlation coefficient

    Spearman's rank correlation coefficient is a measure similar to the Pearson correlation coefficient, only that the difference is that it is based on the association between ranges while the Spearman's rank is based on the Association of each variable scores. This test is used for small sample sizes, although there is an approximation to the normal for large samples, moreover, it is in the range - 1 to 1, with 1 being perfect positive correlation, - 1 the perfect negative correlation and 0 zero correlation. This coefficient is used for data that are ordinal scale.

    2) Kendall rank correlation coefficient

    The Kendall tau coefficient is similar to Spearman, only with the difference that can be used for cases of large samples. As in the Spearman is the range of values between - 1 and 1, where the absolute value of this value represents the probability that the two variables are in the same order.

    "
    cb1 <- gtext(text,container=w1)
    size(cb1)<-c(350,350)

  }

  C41<-list(u1=gaction("Spearman's rank correlation coefficient",handler=spearman),u2=gaction("Kendall rank correlation coefficient",handler=kendall))
  C4<-list(Ordinal=C41)
  C332<-list(u1=gaction("Cochran's Q test",handler=ccro))
  C331<-list(u1=gaction("Friedman test",handler=fri))
  C321<-list(Ordinal=C331,Nominal=C332)
  C312<-list(u1=gaction("Jonckheere Terpstra test",handler=terpstra),u2=gaction("Kruskal Wallis Test",handler=wallis))
  C31<-list(Ordinal=C312)
  C3<-list(Independent=C31,Dependent=C321)
  C232<-list(u1=gaction("Fisher's exact test",handler=fisherdos),u2=gaction("Chi squared test",handler=jidos))
  C231<-list(u1=gaction("Mann Whitney U test",handler=mann))
  C23<-list(Ordinal=C231,Nominal=C232)
  C222<-list(u1=gaction("McNemar's test",handler=macnemardos))
  C221<-list(u1=gaction("Sign test",handler=signosdos),u2=gaction("Wilcoxon signed rank test ",handler=wilcoxon))
  C21<-list(Ordinal=C221,Nominal=C222)
  C2<-list(Dependent=C21,Independent=C23)
  C12<-list(u1=gaction("Chi squared test", handler=chiaj))
  C11<-list(u1=gaction("Sign test",handler=signo),u2=gaction("Kolmogorov Smirnov test",handler=kolm),u3=gaction("Shapiro Wilk test",handler=shapiro),u4=gaction("Lilliefors test",handler=lili))
  C1<-list(Ordinal=C11,Nominal=C12)

  C5<-list(u1=gaction("One Sample", handler=xone),u2=gaction("Two Samples", handler=xtwo),u3=gaction("K Samples", handler=xk),u4=gaction("Measures of Association", handler=xaso))
  A1<-list(u1=gaction("csv",handler=abrircsv),u3=gaction("xlsx",handler=openex))
  alista<-list(Open=A1,u3=gaction("View",handler=ver),u4=gaction("Refresh",handler=inicio),u5=gaction("Close",handler=close))
  clista<-list(One.Sample.Test=C1,Two.Samples.Test=C2,K.Samples.Test=C3,Measures.of.Association=C4)
  dlista<-list(Description=C5)

  mb_list<-list(File=alista,Statistics=clista,Help=dlista)
  gmenu(mb_list, container=g)

  tmp1 <- gframe("", container=g0, expand=TRUE,horizontal=FALSE)
  tg<-glabel("                    Non parametric Module                  ",container=tmp1)
  font(tg) <- list(weight="bold",size= 25 ,family="sans",align ="center",spacing = 5)

  tg <- glabel("                                                                  ",
               container= tmp1)
  tg <- glabel("                                                                  ",
               container= tmp1)
  tg <- glabel("                                                                  ",
               container= tmp1)
  tg <- glabel("                        ITAM                           UV        ",
               container= tmp1)
  font(tg) <- list(weight = "bold", size = 24, family = "sans", align = "center", spacing = 5)
  tg <- glabel("                             Statistics Deparment                   Universidad Veracruzana             ", container= tmp1)
  font(tg) <- list(weight = "bold", size = 12, family = "sans", align = "center", spacing = 5)

  visible(x) <- TRUE
}
