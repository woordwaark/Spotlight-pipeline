# libcurl4-openssl-dev
# libxml2-dev
# libssl-dev
# libpoppler-cpp-dev
# poppler-utils

library(shiny)
library(shinyBS)
library(shinyWidgets)
library(shinyjqui)
library(fresh)
library(stringr)
library(readr)
library(readtext)
library(openxlsx)
library(shinyFeedback)
library(data.table)
library(shinybusy)
library(tokenizers)
library(DataCombine)
library(rhandsontable)
library(RCurl)
library(emayili)
library(magrittr)

################################################################################

addResourcePath("docs", "../grunnegs/docs")

################################################################################

ui <- tagList(
  includeCSS("www/styles.css"), tags$script(src = "extend.js"), use_googlefont("Dosis"), useShinyFeedback(),

  div(style='margin-top: -20px;', titlePanel(title = NULL, windowTitle = "WoordWaark")),
  
  tags$head(
    tags$link(rel="icon", href="vlagWT.png"),
    
    tags$meta(charset="UTF-8"),
    tags$meta(name   ="description", content=""),
    tags$meta(name   ="viewport"   , content="width=device-width, initial-scale=1, user-scalable=no")
  ),
  
  navbarPage
  (
    title=NULL, id = "navBar", collapsible = TRUE,

    tabPanel
    (
      title = HTML("<span id='login' class='menu'>Login</span>"),
      value = "login",
      
      fluidPage
      (
        style = "border: 1px solid silver; border-radius: 6px; background-image: url('background.jpg'); background-size:cover",
        class = "minHeight0",
        
        br(),
        HTML("<div style='text-align: center'><span class='title'>Login</span></div>"),
        br(),
        
        fluidPage(
          style = "margin-left: -15px; margin-right: -15px;",
          cellArgs = list(style = "padding: 6px"),
          align = "center",
          
          wellPanel(
            style = "padding-left: 50px; padding-right: 50px; width: 500px;",
            
            br(),
            splitLayout(cellWidths = c("90px", "300px"), p(strong("Username:")),     textInput("Username",NULL)),
            splitLayout(cellWidths = c("90px", "300px"), p(strong("Password:")), passwordInput("Password",NULL))
          )
        )
      ),
      
      br()
    ),
    
    tabPanel
    (
      title = HTML("<span id='bron' class='menu'>Selecteer bron</span>"),
      value = "bron",

      fluidPage
      (
        style = "border: 1px solid silver; border-radius: 6px; background-image: url('background.jpg'); background-size:cover",
        class = "minHeight0",
        
        br(),
        HTML("<div style='text-align: center'><span class='title'>Selecteer bron</span></div>"),
        br(),
        
        fluidPage
        (
          style = "max-width: 500px; height: calc(100vh - 296px); border: 1px solid silver; border-radius: 6px; background-color: #ffffff; letter-spacing: 1px; line-height: 1.7",
          
          br(),
          
          fluidPage
          (
            style = "max-width: 330px",
            align = "center",
            
            uiOutput('listFiles')
          ),

          uiOutput('verderLemTag'),
          br(),
        ),
        
        br(),
        
        fluidPage(
          align = "center",
          radioButtons("status", NULL, c("te bewerken", "in bewerking", "bewerking voltooid"), selected = "te bewerken", inline=T)
        )
      ),
      
      br()
    ),

    tabPanel
    (
      title = HTML("<span id='lemtag' class='menu'>Controleer tabel</span>"),
      value = "lemtag",

      fluidPage
      (
        style = "border: 1px solid silver; border-radius: 6px; background-image: url('background.jpg'); background-size:cover; line-height: 1.7",
        class = "minHeight0",
        
        fluidPage
        (
          style = "margin-left: calc(50vw - 569px); margin-right: calc(50vw - 569px);",

          br(),
          
          rHandsontableOutput('resultTAB'),
          
          br(),
          
          splitLayout(
            align = "center",
            actionButton('corrigeerlemmas' , "Corrigeer lemma's"),
            actionButton('zoekvervanglemma', "Zoek en vervang een lemma"  ),
            actionButton('zoekvervangpos'  , "Zoek en vervang een POS-tag")
          )
        ),
        
        br(),
        
      ),

      br()
    ),

    tabPanel
    (
      title = HTML("<span id='xml' class='menu'>Bewerking afsluiten</span>"),
      value = "xml",

      fluidPage
      (
        style = "border: 1px solid silver; border-radius: 6px; background-image: url('background.jpg'); background-size:cover; line-height: 1.7",
        class = "minHeight0",
        
        br(),
        uiOutput("showButton"),
        br(),
        uiOutput("resultXML")
      ),
      
      br()
    ),

    tabPanel
    (
      title = HTML("<span id='contact' class='menu'>Contact</span>"),
      value = "contact",

      fluidPage
      (
        style = "border: 1px solid silver; border-radius: 6px; background-image: url('background.jpg'); background-size:cover",
        class = "minHeight0",

        br(),
        HTML("<div style='text-align: center'><span class='title'>Vragen? Stuur een bericht</span></div>"),
        br(),

        fluidPage
        (
          style = "max-width: 600px; border: 1px solid silver; border-radius: 6px; background-color: #ffffff; line-height: 1.3; text-align: center",

          br(),

          fluidPage(
            style = 'width: 90%; text-align: left; border-radius: 0px; font-family: Helvetica, Arial, sans-serif; font-size: 14px;',

            textInput('cNaam', 'Naam:', placeholder = 'invullen is verplicht'),
            br(),
            textInput('cEmailadres', 'E-mailadres:', placeholder = 'invullen is verplicht'),
            br(),
            textInput('cOnderwerp', 'Onderwerp:', placeholder = ''),
            br(),
            textAreaInput('cBericht', 'Bericht:', height = "150px", resize = "none")
          ),

          br(),

          fluidPage
          (
            align = "center",
            shiny::actionButton("verstuur", "Verstuur!"),
            br(),
          ),

          br()
        ),

        br(),
      ),

      br()
    ),
    
    tabPanel
    (
      title = HTML("<span id='logout' class='menu'>Logout</span>"),
      value = "logout"
    )
  ),

  tags$footer
  (
    tags$table(style = "width:100%",
      tags$tr
      (
        tags$td(tags$a(tags$div(style='margin-top: 5px;', tags$img(src="CGTC.png", style = "height: 35px; margin-top: -1px; margin-left: 20px;"), HTML("<span style='font-family: Dosis; color: #1391ab; font-weight: bold;'>&nbsp;Centrum Groninger Taal & Cultuur</span>")),
                       href    = "http://www.cgtc.nl/"),
                       style   = "width: 40%; text-align: left;",
                       class   = "balk",
                       onclick = "window.open('http://www.cgtc.nl/', '_blank'); return false;",
                       target  = "_blank"),
        tags$td(tags$a(tags$img(src="RUG.png", style = "height: 35px; margin-top: 2px; margin-right: 22px;"),
                       href    = "https://www.rug.nl/research/research-let/expertisecentra/groningertaalencultuur/"),
                       style   = "width: 40%; text-align: right;",
                       class   = "balk",
                       onclick = "window.open('https://www.rug.nl/research/research-let/expertisecentra/groningertaalencultuur/', '_blank'); return false;",
                       target  = "_blank")
      )
    )
  )
)

################################################################################

server <- function(input, output, session)
{
  observeEvent(input$navBar, 
  {
    if (getUrlHash() == paste0("#", input$navBar)) return()
    updateQueryString(paste0("#", input$navBar), mode = "push")
  })
  
  observeEvent(getUrlHash(),
  {
    Hash <- getUrlHash()
    if (Hash == paste0("#", input$navBar)) return()
    Hash <- gsub("#", "", Hash)
    updateNavbarPage(session, "navBar", selected=Hash)
  })

  ################################################################################

  observe(
  {
    shinyjs::hide(selector = "#navBar li a[data-value=bron]"    )
    shinyjs::hide(selector = "#navBar li a[data-value=lemtag]"  )
    shinyjs::hide(selector = "#navBar li a[data-value=xml]"     )
    shinyjs::hide(selector = "#navBar li a[data-value=contact]" )
    shinyjs::hide(selector = "#navBar li a[data-value=logout]"  )
    
    updateNavbarPage(session, "navBar", selected="login")
  })

  loginOK <- reactive(
  {
    Gebruiker <- read.delim("auth/data.csv")

    Id.username <- which(Gebruiker$Gebruikersnaam == input$Username)
    Id.password <- which(Gebruiker$Wachtwoord     == input$Password)
    
    if (((length(Id.username) > 0) & (length(Id.password) > 0)) && (Id.username == Id.password))
      return(T)
    else
      return(F)
  })
  
  observe(
  {
    if (loginOK())
    {
      shinyjs::hide(selector = "#navBar li a[data-value=login]"   )
      shinyjs::show(selector = "#navBar li a[data-value=bron]"    )
      shinyjs::show(selector = "#navBar li a[data-value=lemtag]"  )
      shinyjs::show(selector = "#navBar li a[data-value=xml]"     )
      shinyjs::show(selector = "#navBar li a[data-value=contact]" )
      shinyjs::show(selector = "#navBar li a[data-value=logout]"  )

      updateNavbarPage(session, "navBar", selected="bron")
    }
  })

  ################################################################################
  
  listFiles <- eventReactive(input$status,
  {
    listFiles1 <- str_extract(list.files(path = "docs", pattern = "(*.tsv)|(*.TSV)", recursive = F, full.names = F), paste0("[:graph:]+(?=\\.(tsv|TSV))"))  
    listFiles2 <- str_extract(list.files(path = "docs", pattern = "(*.bak)|(*.BAK)", recursive = F, full.names = F), paste0("[:graph:]+(?=\\.(bak|BAK))"))
    listFiles3 <- str_extract(list.files(path = "docs", pattern = "(*.xml)|(*.XML)", recursive = F, full.names = F), paste0("[:graph:]+(?=\\.(xml|XML))"))
    
    if (input$status=="te bewerken")
    {
      listFiles <- listFiles1
      listFiles <- setdiff(listFiles, listFiles2)
      listFiles <- setdiff(listFiles, listFiles3)
    }
    else
      
    if (input$status=="in bewerking")
    {
      listFiles <- listFiles2
      listFiles <- setdiff(listFiles, listFiles3)
    }
    else
      
    if (input$status=="bewerking voltooid")
    {
      listFiles <- listFiles3
    }
    else {}

    if (length(listFiles))
      return(sort(listFiles))
    else
      return(c())
  })

  global <- reactiveValues(
    TSV       = NULL,
    resultTAB = NULL,
    success   = NULL
  )

  ##############################################################################
  
  output$listFiles <- renderUI(
  {
    selectInput('listSources', NULL, listFiles(), multiple=FALSE, selectize=FALSE, selected = character(0), size=16, width="300px")
  })

  output$verderLemTag <- renderUI(
  {
    req(input$listSources)
    return(fluidPage(align="center", br(style='content: " "; display: block; margin: 10px 0;'), actionLink('verderLemTag', 'Controleer lemma\'s en POS-tags')))
  })
  
  observeEvent(input$verderLemTag,
  {
    updateNavbarPage(session, "navBar", selected="lemtag")
  })

  ##############################################################################
  
  observe(
  {
    global$TSV <- NULL
    
    if (!is.null(input$listSources))
    {
      if (file.exists(paste0("docs/", input$listSources,".tsv")))
        global$TSV <- paste0("docs/", input$listSources,".tsv")
      else
        
      if (file.exists(paste0("docs/", input$listSources,".TSV")))
        global$TSV <- paste0("docs/", input$listSources,".TSV")
      else
        global$TSV <- NULL
    }
  })
  
  observeEvent(input$listSources,
  {
    global$resultTAB   <- read.delim(global$TSV, quote="")
    
    if (!("check" %in% colnames(global$resultTAB)))
    {
      global$resultTAB$check <- ""
    }
    
    global$resultTAB[] <- lapply(global$resultTAB, as.character)
  })

  output$resultTAB <- renderRHandsontable(
  {
    req(global$resultTAB)

    searchFUN <- "function (key, options) {
                    var srch = prompt('Search criteria');
                   
                    this.search.query(srch);
                    this.render();
                  }"

    hot_col(hot        = rhandsontable(global$resultTAB, width = 1010, height = input$height - 270, colWidths = c(120, 370, 270, 117, 60), rowHeaders=T, search = TRUE),
            col        = "line",
            format     = "0a",
            customOpts = list(search = list(name = "Search", callback = htmlwidgets::JS(searchFUN))))
  })
  
  observeEvent(input$resultTAB,
  {
    req(global$resultTAB)
    req( input$resultTAB)

    temp   <- gsub("(.tsv$|.TSV$)", ".tmp", global$TSV)
    backup <- gsub("(.tsv$|.TSV$)", ".bak", global$TSV)

    write.table(hot_to_r(input$resultTAB), temp, quote = F, sep="\t", row.names = F)
    
    if (!file.exists(backup))
    {
      old <- read_file(global$TSV)
      new <- read_file(temp)
      
      if (old!=new)
        system(paste0("cp ", global$TSV, " ", backup))
    }
    
    system(paste0("mv ", temp, " ", global$TSV))
  })

  ##############################################################################
  
  customDraggableModalDialog <- function(..., title = NULL, footer = shiny::modalButton("Dismiss"), size = c("m", "s", "l"), easyClose = FALSE, fade = TRUE) 
  {
    size <- match.arg(size)
    cls <- if (fade) { "modal fade" } else { "modal" }
    
    shiny::div(
      id = "shiny-modal",
      class = cls,
    # tabindex = "-1" tabindex attribuut verwijderd
      `data-backdrop` = if (!easyClose) { "static" } ,
      `data-keyboard` = if (!easyClose) { "false" } ,
      
      shiny::div(
        class = "modal-dialog",
        class = switch(size, s = "modal-sm", m = NULL, l = "modal-lg"),
        
        jqui_draggable(shiny::div(
          class = "modal-content",
          if (!is.null(title)) 
          {
            shiny::div(
              class = "modal-header",
              shiny::tags$h4(class = "modal-title",  title)
            )
          },
          
          shiny::div(class = "modal-body", ...),
          if (!is.null(footer)) 
          {
            shiny::div(class = "modal-footer", footer)
          }
        ))
      ),
      shiny::tags$script("$('#shiny-modal').modal().focus();")
    )
  }

  observeEvent(input$corrigeerlemmas,
  {
    show_modal_spinner(text = HTML("<br>De lemma's worden gecorrigeerd, dit kan even duren.<br>Sluit de pagina niet af."))
    
    listFiles3 <- str_extract(list.files(path = "docs", pattern = "(*.xml)|(*.XML)", recursive = F, full.names = F), paste0("[:graph:]+(?=\\.(xml|XML))"))
    
    done <- data.frame()
    
    for (i in 1:length(listFiles3))
    {
      done <- rbind(done, read.delim(paste0("docs/", listFiles3[i], ".tsv"), quote=""))
    }
    
    done$line <- NULL
    done <- unique(done)
    
    # # #
    
    WB <- read_delim("WB.csv", 
                     delim = "\t", 
                     escape_double = FALSE, 
                     trim_ws = TRUE,
                     show_col_types = FALSE)
    
    WB <- na.omit(data.frame(lemmaGN = WB$lemmaGN, lemmaNL = WB$lemmaNL))
    
    WB <- unique(subset(WB, !grepl(";", WB$lemmaNL)))
    
    # # #
    
    df <- hot_to_r(input$resultTAB)
    df$check <- ""

    for (i in 1:nrow(df))
    {
      if (!grepl("^\\$", df$token_GN[i]) & !grepl("^#", df$token_GN[i]))
      {
        subDone <- unique(subset(done, (token_GN==df$token_GN[i]) & (upos==df$upos[i])))
        
        if (nrow(subDone)== 1)
        {
          subWB <- unique(subset(WB, WB$lemmaGN==df$token_GN[i]))
          
          if (nrow(subWB)==1)
          {
            df$lemma_NL[i] <- as.character(subDone$lemma_NL[1])
            df$check[i] <- "🗸"
          }
        }
      }
    }  

    global$resultTAB <- df

    remove_modal_spinner()
    
    nr <- nrow(subset(df, check=="🗸"))
    showNotification(paste(nr, " lemma's gecorrigeerd"), type = "message", duration = 60)  
  })

  observeEvent(input$zoekvervanglemma,
  {
    showModal(customDraggableModalDialog(easyClose = FALSE, fade = FALSE,
       style = "background-color: #f9f7fc",
       title = "Zoek en vervang een lemma",
                                                      
       fluidPage(
          style = "font-family: Helvetica, Arial, sans-serif; font-size: 14px;",
          align = "left",
                                                        
          textInput(   'findToken', 'Token', ""),
                                                    
          br(),
                                                        
          textInput(   'findLemma', 'Huidige lemma', ""),
                                                        
          br(),
                                                        
          textInput('replaceLemma', 'Vervang lemma door:', ""),
                                                        
          br()
       ),
                                                      
       footer = fluidPage(align="center", actionButton("searchreplacelemma", label = "OK"))
    ))
  })

  observeEvent(input$zoekvervangpos,
  {
    showModal(customDraggableModalDialog(easyClose = FALSE, fade = FALSE,
       style = "background-color: #f9f7fc",
       title = "Zoek en vervang een POS-tag",
                                                
       fluidPage(
         style = "font-family: Helvetica, Arial, sans-serif; font-size: 14px;",
         align = "left",
                                                 
         textInput(   'findToken', 'Token', ""),
                                                  
         br(),
                                                  
         textInput(   'findPOS'  , 'Huidige POS-tag', ""),
                                                  
         br(),
                                                  
         textInput('replacePOS'  , 'Vervang POS-tag door:', ""),
                                                  
         br()
       ),
       
       footer = fluidPage(align="center", actionButton("searchreplacepos", label = "OK"))
    ))
  })
  
  observeEvent(input$searchreplacelemma, 
  {
    removeModal()
                 
    nr <- nrow(subset(global$resultTAB, (tolower(token_GN)==tolower(input$findToken)) & (tolower(lemma_NL)==tolower(input$findLemma))))
       
    global$resultTAB <- hot_to_r(input$resultTAB)
              
    global$resultTAB$lemma_NL[((tolower(global$resultTAB$token_GN)==tolower(input$findToken)) & 
                                       (global$resultTAB$lemma_NL ==        input$findLemma))] <- input$replaceLemma
                 
    showNotification(paste(nr, " lemma's vervangen"), type = "message", duration = 60)
  })

  observeEvent(input$searchreplacepos, 
  {
    removeModal()
    
    nr <- nrow(subset(global$resultTAB, (tolower(token_GN)==tolower(input$findToken)) & (tolower(upos)==tolower(input$findPOS))))

    global$resultTAB <- hot_to_r(input$resultTAB)
    
    global$resultTAB$upos    [((tolower(global$resultTAB$token_GN)==tolower(input$findToken)) & 
                                       (global$resultTAB$upos     ==        input$findPOS  ))] <- input$replacePOS

    showNotification(paste(nr, " POS-tags vervangen"), type = "message", duration = 60)
  })

  ##############################################################################
  
  output$showButton <- renderUI(
  {
    req(global$resultTAB)
    req( input$resultTAB)

    return(
      div(
        style = "padding: 6px; text-align: center;",
        actionButton("xmlButton", "Sla resultaten definitief op")
      )
    )
  })

  output$thanks <- renderUI(
  {
    fluidPage(
      style = "max-width: 500px; border: 1px solid silver; border-radius: 6px; background-color: #ffffff; letter-spacing: 1px; line-height: 1.7",
      align = "center",
      
      br(),
      
      p(style='font-size: 36px;', "Stief bedankt!"),

      div(align = "center", img(src = 'kaart.jpg', style='height: 350px;')),
      br(),
    )
  })
  
  output$resultXML <- renderUI(
  {
    req(global$resultTAB)
    req( input$resultTAB)
    req(resultXML())
    
    uiOutput('thanks')
  })

  write.xml <- function(resultTAB, fileName)
  {
    show_modal_spinner(text = HTML("<br>Resultaten worden opgeslagen, dit kan even duren.<br>Sluit de pagina niet af."))

    resultTAB$line     <- as.character(resultTAB$line)
    resultTAB$token_GN <- as.character(resultTAB$token_GN)
    resultTAB$lemma_NL <- as.character(resultTAB$lemma_NL)
    resultTAB$upos     <- as.character(resultTAB$upos)
    
    cat('<?xml version="1.0" encoding="UTF-8"?>\n', file=fileName, append=F)
    cat('\n', file=fileName, append=T)
    cat('<document>\n', file=fileName, append=T)

    firstSrc <- TRUE
    firstArt <- TRUE

    if ((is.na(resultTAB$token_GN[1])) || (substr(resultTAB$token_GN[1], 1, 1) == ""))
      i <- 1     
    else
      i <- 0

    while (i < nrow(resultTAB))
    {
      i <- i + 1
      
      if (firstSrc & firstArt & (!is.na(resultTAB$token_GN[i]) && 
         ((substr(resultTAB$token_GN[i], 1, 1) != "") & (substr(resultTAB$token_GN[i], 1, 1) != "$") & (substr(resultTAB$token_GN[i], 1, 1) != "#"))))
      {
        cat('  <source>\n'   , file=fileName, append=T)
        cat('    <article>\n', file=fileName, append=T)
        cat('      <text>\n' , file=fileName, append=T)
        cat('        <p>\n'  , file=fileName, append=T)
          
        firstSrc <- FALSE
        firstArt <- FALSE
        
        if ((!is.na(resultTAB$token_GN[i+1])) && (substr(resultTAB$token_GN[i+1], 1, 1) != ""))
          i <- i - 1
      }
      else 

      if ((!is.na(resultTAB$token_GN[i])) && (substr(resultTAB$token_GN[i], 1, 1) == "$"))
      {
        if (firstSrc)
          firstSrc <- FALSE
        else
        {
          cat('        </p>\n'  , file=fileName, append=T)
          cat('      </text>\n' , file=fileName, append=T)
          cat('    </article>\n', file=fileName, append=T)
          cat('  </source>\n\n' , file=fileName, append=T)
        }
        
        cat('  <source>\n', file=fileName, append=T)
        cat('    <metadata>\n', file=fileName, append=T)
        cat("      <meta name='redacteurOFauteur'>"      , substr(resultTAB$token_GN[i+ 2], 3, nchar(resultTAB$token_GN[i+ 2])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("      <meta name='titel'>"                  , substr(resultTAB$token_GN[i   ], 3, nchar(resultTAB$token_GN[i   ])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("      <meta name='bron'>"                   , substr(resultTAB$token_GN[i+ 1], 3, nchar(resultTAB$token_GN[i+ 1])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("      <meta name='deelOFvolume'>"           , substr(resultTAB$token_GN[i+ 4], 3, nchar(resultTAB$token_GN[i+ 4])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("      <meta name='jaar'>"                   , substr(resultTAB$token_GN[i+ 5], 3, nchar(resultTAB$token_GN[i+ 5])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("      <meta name='nummerOFaflevering'>"     , substr(resultTAB$token_GN[i+ 6], 3, nchar(resultTAB$token_GN[i+ 6])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("      <meta name='plaatsVanUitgave'>"       , substr(resultTAB$token_GN[i+ 9], 3, nchar(resultTAB$token_GN[i+ 9])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("      <meta name='uitgeverij'>"             , substr(resultTAB$token_GN[i+10], 3, nchar(resultTAB$token_GN[i+10])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("      <meta name='editieOFdruk'>"           , substr(resultTAB$token_GN[i+12], 3, nchar(resultTAB$token_GN[i+12])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("      <meta name='adresWebsite'>"           , substr(resultTAB$token_GN[i+14], 3, nchar(resultTAB$token_GN[i+14])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("      <meta name='datumRaadplegingWebsite'>", substr(resultTAB$token_GN[i+15], 3, nchar(resultTAB$token_GN[i+15])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("      <meta name='opmerkingen'>"            , substr(resultTAB$token_GN[i+16], 3, nchar(resultTAB$token_GN[i+16])), "</meta>\n", file=fileName, sep = "", append=T)
        cat('    </metadata>\n\n', file=fileName, append=T)

        i <- i + 16
        
        if ((is.na(resultTAB$token_GN[i+1])) || (substr(resultTAB$token_GN[i+1], 1, 1) == ""))
        {
          i <- i + 1
          
          if (!is.na(resultTAB$token_GN[i+1]) && (substr(resultTAB$token_GN[i+1], 1, 1) != "#"))
          {
            cat('    <article>\n', file=fileName, append=T)
            cat('      <text>\n' , file=fileName, append=T)
            cat('        <p>\n'  , file=fileName, append=T)
            
            firstArt <- FALSE
          }
          else
            firstArt <- TRUE    
        }
        else
        {
          if (!is.na(resultTAB$token_GN[i+1]) && (substr(resultTAB$token_GN[i+1], 1, 1) != "#"))
          {
            cat('    <article>\n', file=fileName, append=T)
            cat('      <text>\n' , file=fileName, append=T)
            cat('        <p>\n'  , file=fileName, append=T)
            
            firstArt <- FALSE
          }
          else
            firstArt <- TRUE    
        }
      }
      else
        
      if ((!is.na(resultTAB$token_GN[i])) && (substr(resultTAB$token_GN[i], 1, 1) == "#"))
      {
        if (firstArt)
          firstArt <- FALSE
        else
        {
          cat('        </p>\n'    , file=fileName, append=T)
          cat('      </text>\n'   , file=fileName, append=T)
          cat('    </article>\n\n', file=fileName, append=T)
        }
      
        cat('    <article>\n'   , file=fileName, append=T)
        cat('      <metadata>\n', file=fileName, append=T)
        cat("        <meta name='auteur'>"               , substr(resultTAB$token_GN[i   ], 3, nchar(resultTAB$token_GN[i   ])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("        <meta name='titel'>"                , substr(resultTAB$token_GN[i+ 1], 3, nchar(resultTAB$token_GN[i+ 1])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("        <meta name='genre'>"                , substr(resultTAB$token_GN[i+ 2], 3, nchar(resultTAB$token_GN[i+ 2])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("        <meta name='taal1'>"                , substr(resultTAB$token_GN[i+ 3], 3, nchar(resultTAB$token_GN[i+ 3])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("        <meta name='taal2'>"                , substr(resultTAB$token_GN[i+ 4], 3, nchar(resultTAB$token_GN[i+ 5])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("        <meta name='opmerkingen'>"          , substr(resultTAB$token_GN[i+ 5], 3, nchar(resultTAB$token_GN[i+ 5])), "</meta>\n", file=fileName, sep = "", append=T)
        cat('      </metadata>\n\n', file=fileName, append=T)
        cat('      <text>\n'       , file=fileName, append=T)
        cat('        <p>\n'        , file=fileName, append=T)
        
        i <- i + 14
        
        if ((is.na(resultTAB$token_GN[i+1])) || (substr(resultTAB$token_GN[i+1], 1, 1) == ""))
          i <- i + 1
      }
      else
       
      if ((!is.na(resultTAB$token_GN[i])) && (substr(resultTAB$token_GN[i], 1, 1) != ""))
      {
        cat('          <w lemma="', resultTAB$lemma_NL[i], '" pos="', resultTAB$upos[i], '">', resultTAB$token_GN[i], '</w>\n', file=fileName, sep = "", append=T) 
      }
      else
        
      if ((is.na(resultTAB$token_GN[i])) || (substr(resultTAB$token_GN[i], 1, 1) == ""))
      {
        if ((!is.na(resultTAB$token_GN[i-1]) && (substr(resultTAB$token_GN[i-1], 1, 1) != "$") & (substr(resultTAB$token_GN[i-1], 1, 1) != "#")) &
            (!is.na(resultTAB$token_GN[i+1]) && (substr(resultTAB$token_GN[i+1], 1, 1) != "$") & (substr(resultTAB$token_GN[i+1], 1, 1) != "#")))
        {
          cat('        </p>\n\n', file=fileName, append=T)
          cat('        <p>\n'   , file=fileName, append=T)
        }
      }
      else {}
    }

    cat('        </p>\n'  , file=fileName, append=T)
    cat('      </text>\n' , file=fileName, append=T)
    cat('    </article>\n', file=fileName, append=T)
    cat('  </source>\n'   , file=fileName, append=T)
    cat('</document>\n'   , file=fileName, append=T)
    
    remove_modal_spinner()
  }

  resultXML <- eventReactive(input$xmlButton,
  {
    req(global$resultTAB)
    req( input$resultTAB)
    
    fileName <- gsub("(.tsv$|.TSV$)", ".xml" , global$TSV)
    write.xml(as.data.frame(hot_to_r(input$resultTAB)), fileName)
    return(paste(readLines(fileName), collapse="\n"))
  }, ignoreInit = TRUE)
 
  ##############################################################################
  
  observeEvent(input$verstuur,
  {
    cBericht <- paste0("Naam:\n", input$cNaam, "\n\n", "Bericht:\n", input$cBericht)

    email <- envelope() %>% from("woordwaark@rug.nl") %>% to("woordwaark@rug.nl") %>% reply(input$cEmailadres) %>% subject(input$cOnderwerp) %>% text(cBericht)
    smtp <- emayili::server(host = "smtpextern.rug.nl", port = 25)

    tryCatch(
    {
      smtp(email, verbose = TRUE)
      global$success <- TRUE
    },
    error   = function(something)
    {
      global$success <- FALSE
    },
    warning = function(something)
    {
      global$success <- FALSE
    })

    if (global$success)
    {
      showModal(div(class="thanks", modalDialog(id='thanks1', easyClose = TRUE, fade = FALSE,
        title = HTML(paste0("<span style='font-weight: bold; color: #c5313d; font-size: 120%;'>Bedankt!</span>")),
                HTML(paste0("<span style='font-size: 14px;'>Uw bericht is verstuurd. U onvangt zo spoedig mogelijk een reactie van ons.</span>")),
        footer = modalButton("OK")
      )))

      updateTextInput    (session, 'cNaam'      , value = "")
      updateTextInput    (session, 'cEmailadres', value = "")
      updateTextInput    (session, 'cOnderwerp' , value = "")
      updateTextAreaInput(session, 'cBericht'   , value = "")
    }
    else
    {
      showModal(div(class="thanks", modalDialog(id='thanks2', easyClose = TRUE, fade = FALSE,
        title = HTML(paste0("<span style='font-weight: bold; color: #c5313d; font-size: 120%;'>Sorry!</span>")),
                HTML(paste0("<span style='font-size: 14px;'>Uw bericht kon niet verstuurd worden. Is uw e-mailadres correct ingevoerd?</span>")),
        footer = modalButton("OK")
      )))
    }
  })
  
  ##############################################################################

  observe(
  {
    if (input$navBar=="logout")
    {
      updateTextInput(session, "Username", value="")
      updateTextInput(session, "Password", value="")

      shinyjs::show(selector = "#navBar li a[data-value=login]"   )
      shinyjs::hide(selector = "#navBar li a[data-value=bron]"    )
      shinyjs::hide(selector = "#navBar li a[data-value=lemtag]"  )
      shinyjs::hide(selector = "#navBar li a[data-value=xml]"     )
      shinyjs::hide(selector = "#navBar li a[data-value=contact]" )
      shinyjs::hide(selector = "#navBar li a[data-value=logout]"  )

      updateNavbarPage(session, "navBar", selected="login")
    }
  })
}

################################################################################

shinyApp(ui = ui, server = server)
