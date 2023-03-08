#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DiagrammeR)
library(DiagrammeRsvg)
library(dashboardthemes)
library(rsvg)
library(htmlwidgets)
library(shinyjs)
library(shinyWidgets)
library(rintrojs)
library(foreach)

call.subgrapgh.rv <- function(numgp, label, color,item) {
    mydiv <- div(
        tabPanel(
            title= paste("Subgraph", numgp),
            fluidRow(column(4, textInputIcon(paste0("label",numgp), paste0("Label for Subgraph ", numgp,":"), value = label, icon = icon("tag"))),
                     column(4, colourpicker::colourInput(paste0("col",numgp), paste0("Select Color for ", numgp,":"), value = color))
            ),
            fluidRow(
                column(4, textAreaInput(paste0("subtext",numgp),paste0("Type items for Subgraph1 ", numgp,":"),value = item, width = '100%', height = '120px'))
            )

        )
    )
    return(mydiv)
}

call.subgrapgh <- function(numgp) {
    mydiv <- div(
        tabPanel(
            title= paste("Subgraph", numgp),
            fluidRow(column(4, textInputIcon(paste0("label",numgp), paste0("Label for Subgraph ", numgp,":"),  icon = icon("tag"))),
                     column(4, colourpicker::colourInput(paste0("col",numgp), paste0("Select Color for ", numgp,":") ))
            ),
            fluidRow(
                column(6, textAreaInput(paste0("subtext",numgp),paste0("Type items for Subgraph1 ", numgp,":"),width = '120%', height = '120px'))
            )

        )
    )
    return(mydiv)
}

call.linkage <- function(numl, items, don, rec, tag) {
    mydiv <- div(
        fluidRow(
            column(4, selectizeInput(paste0("don",numl), paste0(numl,") From: "), selected = don,
                                  choices= items)),
            column(4, selectizeInput(paste0("rec",numl), paste0(numl,") To: "), selected = rec,
                                  choices= items)),
            column(4, textInput(paste0("tag",numl), paste0(numl,") Tag: "), value = tag))
        )
    )
    return(mydiv)
}
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({
        #print(input$numsubg)
        req(input$col1)

       # print(rv$items())



        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = input$col1, border = 'white')

    })

    output$graph <- renderGrViz({
        req(rv$graphtext)
        print(rv$items)

    d <- DiagrammeR::grViz(rv$graphtext )
    d
    })

    output$linkagecontrol <- renderUI({
        req(rv$link)

        adddiv <- div(fluidRow(column(4, actionButton("updatelink","Update Selection")),
            column(4, actionButton("addlink","Add Linkage")),
            column(4, actionButton("deletelink","Delete Linkage"))
            ))
        if(rv$link > 0 ) {

            mydiv <-  tagList(
              #  tabPanel(
              #      title="Linkage",
                    div(adddiv,
                        do.call("div",foreach::foreach(a=1:rv$link) %do% call.linkage(a,
                                                                                      rv$items,
                                                                                      don=rv$linkinfo$don[a],
                                                                                      rec=rv$linkinfo$rec[a],
                                                                                      tag=rv$linkinfo$tag[a])))
              #  )
            )
        }else{
            mydiv <-  tagList(
           #     tabPanel(
                    div(adddiv)
          #      )
            )
        }
    })

    output$subgraphcontrol <- renderUI({

            #  print("2")
             # print(rv$info)
              mydiv <-  tagList(
                  do.call("tabBox",
                          c(foreach::foreach(i=1:length(rv$info$subg)) %do% do.call("tabPanel",
                                                                           list(paste("Subgraph", rv$info$subg[i]),
                                                                                call.subgrapgh.rv(rv$info$subg[i],
                                                                                                  label=rv$info$label[i],
                                                                                                  color=rv$info$color[i],
                                                                                                  item = rv$info$item[i]))),
                            width = 5)
                  )
              )

        mydiv
    })

    output$delete <- renderUI({
        mydiv <- tagList(
            fluidRow(
               column(4, selectInput("del","Select subgraphs to remove",
                                     selected = NULL,
                                     choices= rv$graphstoselect,
                                     multiple = T)),
               actionButton("rm", "REMOVE")
            )
        )
    })

    rv <- reactiveValues(graphnum = 1,
                         link=0,
                         items="init",
                         info=list(label=c(""), color=("#B87272"), item=c("") ,subg=1),
                         linkinfo=list(don=c(""),rec=c(""),tag=c("")))

    rv$colorlist <- reactive({
        foreach::foreach(i=rv$graphlist,.combine = "c") %do% input[[paste0("col",i)]]
    })

    rv$labellist <- reactive({
        foreach::foreach(i=rv$graphlist,.combine = "c") %do% input[[paste0("label",i)]]
    })

    rv$itemlist <- reactive({
        foreach::foreach(i=rv$graphlist, .combine = "c") %do% input[[paste0("subtext",i)]]
       # rv$items <- foreach::foreach(i=rv$itemlist, .combine = "c") %do% unlist(strsplit(i, split = "\n"))
           # strsplit(input[[paste0("subtext",i)]],split = "\n")
    })

    observeEvent(input$updatelink,{
        print(rv$itemlist())
        rv$items <-  foreach::foreach(i=rv$itemlist(), .combine = "c") %do% unlist(strsplit(i, split = "\n"))
        print(rv$items)
    })

    rv$donlist <- reactive({
        foreach::foreach(i=1:rv$link,.combine = "c") %do% input[[paste0("don",i)]]
    })

    rv$reclist <- reactive({
        foreach::foreach(i=1:rv$link,.combine = "c") %do% input[[paste0("rec",i)]]
    })

    rv$taglist <- reactive({
        foreach::foreach(i=1:rv$link,.combine = "c") %do% input[[paste0("tag",i)]]
    })

    observe({
        req(rv$graphlist)
        rv$graphstoselect <-  foreach::foreach(a=rv$graphlist, .combine = "c") %do% paste("Subgraph",a)
    })

    observeEvent(input$addlink,{
        rv$link <- isolate(rv$link+1)

        print(paste("linkage number:",rv$link))

        rv$linkinfo <- isolate(list(don=rv$donlist(),
                                    rec=rv$reclist(),
                                    tag=rv$taglist()))

        print(rv$linkinfo)
    })

    observeEvent(input$deletelink,{
        req(rv$link >0)
        rv$link <- isolate(rv$link-1)

        print(paste("linkage number:",rv$link))

        rv$linkinfo <- isolate(list(don=rv$donlist(),
                                    rec=rv$reclist(),
                                    tag=rv$taglist()))

        print(rv$linkinfo)
    })

    observeEvent(input$refresh, {
        #rv$graphnum <- 0

        if(length(rv$del) >=1) {
            newl <- isolate(rv$graphlist)
            print("newl")
            print(newl)
            print(rv$del)
            todel <- rv$del[rv$del %in% newl]
            if(length(todel) > 0 ) {
                rv$graphlist <- c(newl[-match(todel,newl)], rv$graphnum +1)
            }else{
                rv$graphlist <- c(newl, rv$graphnum +1)
            }

        }else{
            rv$graphlist <- isolate(1:(rv$graphnum +1))
        }

        rv$graphnum <- isolate(rv$graphnum +1)

        print("graphlist")
        print(rv$graphlist)


        rv$info <- isolate(list(color=rv$colorlist(),
                                label= rv$labellist(),
                                item = rv$itemlist(),
                                subg=rv$graphlist))

    })

    observeEvent(input$rm, {
        req(input$del)

       # print(input$del)

        myl <- foreach::foreach(a=input$del, .combine = "c") %do% as.numeric(unlist(strsplit(a, split = " "))[2])
       # print(myl)

        newl <- rv$graphlist[-match(myl,rv$graphlist)]
        rv$graphlist <- isolate(newl)
        rv$del <- isolate(myl)

        rv$info <- isolate(list(color=rv$colorlist(),
                                label= rv$labellist(),
                                item = rv$itemlist(),
                                subg=rv$graphlist))

    })

    observeEvent(input$update,{
        rv$info <- isolate(list(color=rv$colorlist(),
                                label= rv$labellist(),
                                item = rv$itemlist(),
                                subg=rv$graphlist))

        rv$linkinfo <- isolate(list(don=rv$donlist(),
                                    rec=rv$reclist(),
                                    tag=rv$taglist()))
    })

    observe({
        req(rv$info)

        subgtlist <- list("subgraph cluster_0 ","{node [shape=box] ", "'",rv$info$item, "'","label=", "'",rv$info$label,"'", "color=","'", rv$info$color,"'", "}")
        subtext <- do.call("paste",subgtlist)

        subglist <- foreach::foreach(a=1:length(rv$info$subg)) %do% do.call("paste",list(paste0("subgraph cluster_",a ),
                                                                                         "{node [shape=box] ",
                                                                                         do.call("paste",foreach::foreach(b=strsplit(rv$info$item[a],split = "\n")[[1]]) %do% paste0("'",b,"'")),
                                                                                         "label=",
                                                                                         "'",
                                                                                         rv$info$label[a],
                                                                                         "'",
                                                                                         "color=",
                                                                                         "'",
                                                                                         rv$info$color[a],
                                                                                         "'", "}"))

        if(rv$link >0) {
            linktext <- foreach::foreach(a=1:rv$link) %do% do.call("paste0", list("'",rv$linkinfo$don[a],"'",
                                                                                  "->",
                                                                                  "'",rv$linkinfo$rec[a],"'",
                                                                                  "[label=","'",rv$linkinfo$tag[a],"'","]"
                                                                                  ))
            print(do.call("paste",linktext))
        }else{
            linktext <- list("")
            }


        #print(do.call("paste", subglist))
        rv$graphtext <- do.call("paste", list("digraph {graph [rankdir = LR]  ",
                                              do.call("paste", subglist),
                                              do.call("paste",linktext),
                                              "}"))

        print(rv$graphtext)


    })

})
