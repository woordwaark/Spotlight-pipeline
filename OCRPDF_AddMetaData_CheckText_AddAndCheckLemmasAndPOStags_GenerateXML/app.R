# sudo apt update
# sudo apt install libcurl4-openssl-dev libxml2-dev libssl-dev libpoppler-cpp-dev poppler-utils

# install OCRmyPDF
# sudo apt install ocrmypdf

# get list of languages
# apt-cache search tesseract-ocr

# install Dutch
# sudo apt-get install tesseract-ocr-nld

library(shiny)
library(shinyBS)
library(shinyWidgets)
library(shinyjs)
library(shinyjqui)
library(fresh)
library(stringr)
library(readr)
library(readtext)
library(openxlsx)
library(shinyFeedback)
library(rclipboard)
library(data.table)
library(shinybusy)
library(tokenizers)
library(udpipe)
library(DataCombine)
library(rhandsontable)
library(RCurl)
library(emayili)
library(magrittr)

################################################################################

addResourcePath("www" , "www" )
addResourcePath("auth", "auth")
addResourcePath("docs", "docs")

bronnen <- read.xlsx("db.xlsx")
bronnen[is.na(bronnen)] <- ""

################################################################################

ui <- tagList(
  useShinyjs(), use_googlefont("Dosis"), useShinyFeedback(), rclipboardSetup(),
  includeCSS("www/styles.css"), extendShinyjs(script = "www/extend.js", functions = 'getSize'),

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

          uiOutput('verderMeta'),
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
      title = HTML("<span id='metadata' class='menu'>Voer metadata in</span>"),
      value = "metadata",

      fluidPage
      (
        style = "border: 1px solid silver; border-radius: 6px; background-image: url('background.jpg'); background-size:cover",
        class = "minHeight0",

        splitLayout
        (
          style = "border: 1px inherit;",
          cellWidths = c("50%", "50%"),
          cellArgs = list(style = "padding: 6px"),

          column
          (
            width = 12,

            uiOutput("pdfWindow" ),
            br(),
            uiOutput("buttonPane")
          ),

          column
          (
            align = "center",
            width = 12,

            uiOutput("textEditor"),
            br(),
            uiOutput("fontSize")
          )
        )
      ),

      br()
    ),

    tabPanel
    (
      title = HTML("<span id='lemtag' class='menu'>Voeg woordsoorten toe</span>"),
      value = "lemtag",

      fluidPage
      (
        style = "border: 1px solid silver; border-radius: 6px; background-image: url('background.jpg'); background-size:cover; line-height: 1.7",
        class = "minHeight0",

        fluidPage
        (
          br(),
          rHandsontableOutput('resultUD'),
          br(),
          uiOutput("showButtons")
        )
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
    shinyjs::hide(selector = "#navBar li a[data-value=metadata]")
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
      shinyjs::show(selector = "#navBar li a[data-value=metadata]")
      shinyjs::show(selector = "#navBar li a[data-value=lemtag]"  )
      shinyjs::show(selector = "#navBar li a[data-value=xml]"     )
      shinyjs::show(selector = "#navBar li a[data-value=contact]" )
      shinyjs::show(selector = "#navBar li a[data-value=logout]"  )

      updateNavbarPage(session, "navBar", selected="bron")
    }
  })

  ################################################################################

  observe(
  {
    listFiles1 <- str_extract(list.files(path = "docs", pattern = "(*.pdf)|(*.PDF)", recursive = F, full.names = F), paste0("[:graph:]+(?=\\.(pdf|PDF))"))
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
      global$listFiles <- sort(listFiles)
    else
      global$listFiles <- c()
  })

  global <- reactiveValues(
    listFiles         = str_extract(list.files(path = "docs", pattern = "(*.pdf)|(*.PDF)" , recursive = F, full.names = F), paste0("[:graph:]+(?=\\.(pdf|PDF))")),

    PDF               = NULL,
    OCR               = NULL,

    fontSize          = 16,

    Brontype          = read_delim("www/fields/brontype.txt", "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, show_col_types = FALSE)$X1,
    Plaats            = read_delim("www/fields/plaats.txt"  , "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, show_col_types = FALSE)$X1,
    Genre             = read_delim("www/fields/genre.txt"   , "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, show_col_types = FALSE)$X1,
    Taal1             = read_delim("www/fields/taal1.txt"   , "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, show_col_types = FALSE)$X1,
    Taal2             = read_delim("www/fields/taal2.txt"   , "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, show_col_types = FALSE)$X1,

    srcRedacteur      = "",
    srcTitel          = "",
    srcBrontype       = "",
    srcDeel           = "",
    srcJaar           = "",
    srcNummer         = "",
    srcPlaats         = "",
    srcUitgeverij     = "",
    srcEditie         = "",
    srcWebsite        = "",
    srcRaadpleging    = "",
    srcOpmerkingen    = "",

    artAuteur         = "",
    artTitel          = "",
    artGenre          = "",
    artTaal1          = "",
    artTaal2          = "",
    artOpmerkingen    = "",

    Src               = "",
    Art               = "",

    resultUD          = NULL,

    success           = NULL
  )

  ##############################################################################

  output$listFiles <- renderUI(
  {
    selectInput('listSources', NULL, global$listFiles, multiple=FALSE, selectize=FALSE, selected = character(0), size=16, width="300px")
  })

  output$verderMeta <- renderUI(
  {
    req(input$listSources)
    return(fluidPage(align="center", br(style='content: " "; display: block; margin: 10px 0;'), actionLink('verderMeta', 'Voer metadata in')))
  })

  observeEvent(input$verderMeta,
  {
    updateNavbarPage(session, "navBar", selected="metadata")
  })

  ##############################################################################

  observe(
  {
    if (!is.null(input$listSources))
    {
      if (file.exists(paste0("docs/", input$listSources,".pdf")))
        global$PDF <- paste0("docs/", input$listSources,".pdf")
      else

      if (file.exists(paste0("docs/", input$listSources,".PDF")))
        global$PDF <- paste0("docs/", input$listSources,".PDF")
      else
        global$PDF <- NULL
    }
  })

  observe(
  {
    if (!is.null(input$listSources))
    {
      if (file.exists(paste0("docs/", input$listSources,".txt")))
        global$OCR <- paste0("docs/", input$listSources,".txt")
      else

      if (file.exists(paste0("docs/", input$listSources,".TXT")))
        global$OCR <- paste0("docs/", input$listSources,".TXT")
      else

      {
        system(paste0("pdftotext docs/", input$listSources, ".pdf"))

        t <- readtext(paste0("docs/", input$listSources,".txt"))$text

        if ((t!="\f") &
            (t!="\f\f") &
            (t!="\f\f\f") &
            (t!="\f\f\f\f") &
            (t!="\f\f\f\f\f") &
            (t!="\f\f\f\f\f\f") &
            (t!="\f\f\f\f\f\f\f") &
            (t!="\f\f\f\f\f\f\f\f") &
            (t!="\f\f\f\f\f\f\f\f\f") &
            (t!="\f\f\f\f\f\f\f\f\f\f"))
        {
          global$OCR <- paste0("docs/", input$listSources,".txt")
        }
        else
        {
          show_modal_spinner(text = HTML("<br>De PDF wordt verwerkt,<br>dit kan even duren."))
          system(paste0("ocrmypdf -c -l nld docs/", input$listSources, ".pdf docs/temp.pdf"), intern = F)

          if (file.exists("docs/temp.pdf"))
          {
            system(paste0("mv docs/temp.pdf docs/", input$listSources, ".pdf"))
          }

          system(paste0("pdftotext docs/", input$listSources, ".pdf"))
          remove_modal_spinner()

          global$OCR <- paste0("docs/", input$listSources,".txt")
        }
      }
    }
  })

  output$pdfWindow <- renderUI(
  {
    req(global$PDF)
    tags$iframe(style="height: calc(100vh - 240px); width:100%", src=global$PDF)
  })

  clearSrc <- function()
  {
    global$srcRedacteur      <- ""
    global$srcTitel          <- ""
    global$srcBrontype       <- ""
    global$srcSerie          <- ""
    global$srcJaar           <- ""
    global$srcNummer         <- ""
    global$srcPlaats         <- ""
    global$srcUitgeverij     <- ""
    global$srcEditie         <- ""
    global$srcWebsite        <- ""
    global$srcRaadpleging    <- ""
    global$srcOpmerkingen    <- ""
  }

  clearArt <- function()
  {
    global$artAuteur         <- ""
    global$artTitel          <- ""
    global$artGenre          <- ""
    global$artTaal1          <- "Gronings"
    global$artTaal2          <- "geen"
    global$artOpmerkingen    <- ""
  }

  observeEvent(input$listSources,
  {
    clearSrc()
    clearArt()

    bestandsnaam <- input$listSources
    index <- which(bronnen$bestandsnaam==bestandsnaam)

    if (length(index)>0)
    {
      global$srcRedacteur      <- bronnen$auteurs    [index]
      global$srcTitel          <- bronnen$titel      [index]
      global$srcBrontype       <- "boek"
      global$srcJaar           <- bronnen$jaar       [index]
      global$srcPlaats         <- bronnen$plaats     [index]
      global$srcUitgeverij     <- bronnen$uitgever   [index]
      global$srcOpmerkingen    <- bronnen$opmerkingen[index]

      global$artAuteur         <- bronnen$auteurs    [index]
      global$artTitel          <- bronnen$titel      [index]
    }
  })

  shinyInputLabel <- function(inputId, label = NULL)
  {
    tags$label(
      label,
      class = "control-label",
      class = if (is.null(label)) "shiny-label-null",
      id = paste0(inputId, "-label"),
      `for` = inputId
    )
  }

  customTextAreaInput <- function(inputId, label, value = "", width = NULL, height = NULL, cols = NULL, rows = NULL, placeholder = NULL, resize = NULL)
  {
    value <- restoreInput(id = inputId, default = value)

    if (!is.null(resize))
    {
      resize <- match.arg(resize, c("both", "none", "vertical", "horizontal"))
    }

    style <- htmltools::css(
      width = if (!is.null(width)) "width: 100%;",
      height = validateCssUnit(height),
      resize = resize
    )

    div(class = "form-group shiny-input-container",
      shinyInputLabel(inputId, label),
      style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),

      tags$textarea(
        id = inputId,
        class = "form-control",
        placeholder = placeholder,
        style = style,
        autocomplete = "off",
        autocorrect = "off",
        autocapitalize = "off",
        spellcheck = "false",
        rows = rows,
        cols = cols,
        value
      )
    )
  }

  output$textEditor <- renderUI(
  {
    req(global$OCR)

    span(
      style=paste0("font-size: ", global$fontSize, "px;"),

      customTextAreaInput(
        inputId = 'textEditor',
        label   = NULL,
        value   = readtext(global$OCR)$text,
        height  = NULL,
        resize  = "none"
      )
    )
  })

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

  observeEvent(input$buttonBron,
  {
    showModal(customDraggableModalDialog(easyClose = FALSE, fade = FALSE,
      style = "background-color: #f9f7fc;",
      title = "",

      fluidPage(
        style = "font-family: Helvetica, Arial, sans-serif; font-size: 14px",

        splitLayout(
          cellWidths = c("340px", "50px"),
          textAreaInput ('srcRedacteur', 'redacteur(s) / auteur(s)', value=global$srcRedacteur, placeholder = '', rows = 1),
          bsButton("helpRedacteur", label = NULL, icon = icon("info"), style = "info", size = "extra-small")
        ),
        bsPopover(
          id = "helpRedacteur", title = NULL,
          content = "Vul de naam of de namen van de redacteur(s) in. Voorbeeld: Willem Diemer, Jan Niehoff, Simon van Wattum.",
          placement = "left",
          trigger = "click",  options = NULL
        ),

        splitLayout(
          cellWidths = c("340px", "50px"),
          textAreaInput ('srcTitel', 'titel', value=global$srcTitel, placeholder = ''),
          bsButton("helpTitle", label = NULL, icon = icon("info"), style = "info", size = "extra-small")
        ),
        bsPopover(
          id = "helpTitle", title = NULL,
          content = "Vul de titel in.",
          placement = "left",
          trigger = "click",  options = NULL
        ),

        splitLayout(
          cellWidths = c("340px", "50px"),
          selectizeInput('srcBrontype', 'brontype', c(global$Brontype, ""), selected=global$srcBrontype, options = list(create=TRUE, placeholder = '', plugins = list('restore_on_backspace'))),
          bsButton("helpBrontype", label = NULL, icon = icon("info"), style = "info", size = "extra-small")
        ),
        bsPopover(
          id = "helpBrontype", title = NULL,
          content = "Selecteer een brontype.",
          placement = "left",
          trigger = "click",  options = NULL
        ),

        conditionalPanel(
          condition = "input.srcBrontype == 'boek'",
          tagList(
            splitLayout(
              cellWidths = c("340px", "50px"),
              textInput('srcDeel', 'deel / volume', value=global$srcDeel, placeholder = ''),
              bsButton("helpDeel", label = NULL, icon = icon("info"), style = "info", size = "extra-small")
            ),
            bsPopover(
              id = "helpDeel", title = NULL,
              content = "Vul deel of volume in indien van toepassing.",
              placement = "left",
              trigger = "click",  options = NULL
            )
          )
        ),

        splitLayout(
          cellWidths = c("340px", "50px"),
          textInput('srcJaar', 'jaar', value=global$srcJaar, placeholder = 'Geef jaar enkel als een jaartal van vier cijfers'),
          bsButton("helpJaar", label = NULL, icon = icon("info"), style = "info", size = "extra-small")
        ),
        bsPopover(
          id = "helpJaar", title = NULL,
          content = "Geef jaar enkel als een jaartal van vier cijfers.",
          placement = "left",
          trigger = "click",  options = NULL
        ),

        conditionalPanel(
          condition = "input.srcBrontype == 'tijdschrift'",
          tagList(
            splitLayout(
              cellWidths = c("340px", "50px"),
              textInput('srcNummer', 'nummer / aflevering', value=global$srcNummer, placeholder = ''),
              bsButton("helpNummer", label = NULL, icon = icon("info"), style = "info", size = "extra-small")
            ),
            bsPopover(
              id = "helpNummer", title = NULL,
              content = "Vul het nummer van het tijdschrift in.",
              placement = "left",
              trigger = "click",  options = NULL
            )
          )
        ),

        conditionalPanel(
          condition = "input.srcBrontype == 'krant'",
          tagList(
            splitLayout(
              cellWidths = c("340px", "50px"),
              textInput('srcNummer', 'nummer / aflevering', value=global$srcNummer, placeholder = ''),
              bsButton("helpNummer", label = NULL, icon = icon("info"), style = "info", size = "extra-small")
            ),
            bsPopover(
              id = "helpNummer", title = NULL,
              content = "Vul het nummer van de krant in.",
              placement = "left",
              trigger = "click",  options = NULL
            )
          )
        ),

        splitLayout(
          cellWidths = c("340px", "50px"),
          textInput('srcPlaats', 'plaats van uitgave', value=global$srcPlaats, placeholder = ''),
          bsButton("helpPlaats", label = NULL, icon = icon("info"), style = "info", size = "extra-small")
        ),
        bsPopover(
          id = "helpPlaats", title = NULL,
          content = "Geef de plaats van uitgave.",
          placement = "left",
          trigger = "click",  options = NULL
        ),

        splitLayout(
          cellWidths = c("340px", "50px"),
          textInput('srcUitgeverij', 'uitgeverij', value=global$srcUitgeverij, placeholder = ''),
          bsButton("helpUitgeverij", label = NULL, icon = icon("info"), style = "info", size = "extra-small")
        ),
        bsPopover(
          id = "helpUitgeverij", title = NULL,
          content = "Geef de naam van de uitgeverij.",
          placement = "left",
          trigger = "click",  options = NULL
        ),

        conditionalPanel(
          condition = "input.srcBrontype == 'boek'",
          tagList(
            splitLayout(
              cellWidths = c("340px", "50px"),
              textInput('srcEditie', 'editie / druk', value=global$srcEditie, placeholder = ''),
              bsButton("helpEditie", label = NULL, icon = icon("info"), style = "info", size = "extra-small")
            ),
            bsPopover(
              id = "helpEditie", title = NULL,
              content = "Geef de editie, indien van toepassing. Bijvoorbeeld: 3e editie, of: 5e druk.",
              placement = "left",
              trigger = "click",  options = NULL
            )
          )
        ),

        splitLayout(
          cellWidths = c("340px", "50px"),
          textAreaInput('srcWebsite', 'website', value=global$srcWebsite, placeholder = 'Geef adres van de website', rows = 1),
          bsButton("helpWebsite", label = NULL, icon = icon("info"), style = "info", size = "extra-small")
        ),
        bsPopover(
          id = "helpWebsite", title = NULL,
          content = "Als de bron op het internet staat, geef dan het internetadres. Bijvoorbeeld: https://oader.nl/nina-werkman/.",
          placement = "left",
          trigger = "click",  options = NULL
        ),

        conditionalPanel(
          condition = "input.srcWebsite != ''",
          tagList(
            splitLayout(
              cellWidths = c("340px", "50px"),
              textInput('srcRaadpleging', 'datum raadpleging website', value=global$srcRaadpleging, placeholder = 'Geef datum als dd-mm-jjjj'),
              bsButton("helpRaadpleging", label = NULL, icon = icon("info"), style = "info", size = "extra-small")
            ),
            bsPopover(
              id = "helpRaadpleging", title = NULL,
              content = "Geef datum als dd-mm-jjjj.",
              placement = "left",
              trigger = "click",  options = NULL
            )
          )
        ),

        splitLayout(
          cellWidths = c("340px", "50px"),
          textAreaInput('srcOpmerkingen', 'opmerkingen', value=global$srcOpmerkingen, placeholder = '', rows = 3),
          bsButton("helpOpmerkingen", label = NULL, icon = icon("info"), style = "info", size = "extra-small")
        ),
        bsPopover(
          id = "helpOpmerkingen", title = NULL,
          content = "Opmerkingen kunt u hier kwijt. Echter voor vragen waar u snel een antwoord op moet hebben kunt u het beste het contactformulier gebruiken.",
          placement = "left",
          trigger = "click",  options = NULL
        ),

        br(),

        tags$script(src = "extend1.js")
      ),

      footer = fluidPage(align="center", actionButton("closeSrc", label = "OK"))
    ))
  })

  observeEvent(input$srcJaar,
  {
    if (input$srcJaar!="")
    {
      feedback(
        "srcJaar",
         show = !grepl("[1-2][0-9][0-9][0-9]", input$srcJaar),
         color = "red",
         text = NULL
      )
    }
  })

  observeEvent(input$srcRaadpleging,
  {
    if (input$srcRaadpleging!="")
    {
      feedback(
        "srcRaadpleging",
        show = !grepl("[1-3][0-9]-[0-1][0-9]-[1-2][0-9][0-9][0-9]", input$srcRaadpleging),
        color = "red",
        text = NULL
      )
    }
  })

  observeEvent(input$buttonArtikel,
  {
    showModal(customDraggableModalDialog(easyClose = FALSE, fade = FALSE,
      style = "background-color: #f9f7fc",
      title = "",

      fluidPage(
        style = "font-family: Helvetica, Arial, sans-serif; font-size: 14px;",

        splitLayout(
          cellWidths = c("340px", "50px"),
          textAreaInput('artAuteur', 'auteur(s)', value=global$artAuteur, placeholder = '', rows = 1),
          bsButton("helpAuteur", label = NULL, icon = icon("info"), style = "info", size = "extra-small")
        ),
        bsPopover(
          id = "helpAuteur", title = NULL,
          content = "Vul de naam of de namen van de auteur(s) in. Voorbeeld: Frederikus Schreiber,  Kunny Luchtenberg.",
          placement = "left",
          trigger = "click",  options = NULL
        ),

        splitLayout(
          cellWidths = c("340px", "50px"),
          textAreaInput('artTitel', 'titel', value=global$artTitel, placeholder = '', rows = 1),
          bsButton("helpTitel", label = NULL, icon = icon("info"), style = "info", size = "extra-small")
        ),
        bsPopover(
          id = "helpTitel", title = NULL,
          content = "Vul de titel in.",
          placement = "left",
          trigger = "click",  options = NULL
        ),

        splitLayout(
          cellWidths = c("340px", "50px"),
          selectizeInput('artGenre', 'genre', c(global$Genre, ""), selected=global$artGenre, options = list(create=TRUE, placeholder = '', plugins = list('restore_on_backspace'))),
          bsButton("helpGenre", label = NULL, icon = icon("info"), style = "info", size = "extra-small")
        ),
        bsPopover(
          id = "helpGenre", title = NULL,
          content = "Selecteer een genre.",
          placement = "left",
          trigger = "click",  options = NULL
        ),

        splitLayout(
          cellWidths = c("340px", "50px"),
          selectizeInput('artTaal1', 'taal1', c(global$Taal1, ""), selected=global$artTaal1, options = list(create=TRUE, placeholder = '', plugins = list('restore_on_backspace'))),
          bsButton("helpTaal1", label = NULL, icon = icon("info"), style = "info", size = "extra-small")
        ),
        bsPopover(
          id = "helpTaal1", title = NULL,
          content = "Als de tekst niet in het Gronings geschreven is, selecteert u dan hier de taal waarin de tekst geschreven is.",
          placement = "left",
          trigger = "click",  options = NULL
        ),

        splitLayout(
          cellWidths = c("340px", "50px"),
          selectizeInput('artTaal2', 'taal2', c(global$Taal2, ""), selected=global$artTaal2, options = list(create=TRUE, placeholder = '', plugins = list('restore_on_backspace'))),
          bsButton("helpTaal2", label = NULL, icon = icon("info"), style = "info", size = "extra-small")
        ),
        bsPopover(
          id = "helpTaal2", title = NULL,
          content = "Als er in de tekst naast het Gronings nog een andere taal gebruikt wordt, selecteert u die dan hier.",
          placement = "left",
          trigger = "click",  options = NULL
        ),

        splitLayout(
          cellWidths = c("340px", "50px"),
          textAreaInput('artOpmerkingen', 'opmerkingen', value=global$artOpmerkingen, placeholder = '', rows = 3),
          bsButton("helpOpmerkingen", label = NULL, icon = icon("info"), style = "info", size = "extra-small")
        ),
        bsPopover(
          id = "helpOpmerkingen", title = NULL,
          content = "Opmerkingen kunt u hier kwijt. Echter voor vragen waar u snel een antwoord op moet hebben kunt u het beste het contactformulier gebruiken.",
          placement = "left",
          trigger = "click",  options = NULL
        ),

        br(),

        p("Klaar met het invullen? Klik op OK en plaats de muis in de tekst vlak voor het begin van het artikel. Rechtsklik met de muis en kies 'plakken' of doe Ctrl-v op het toetsenbord."),

        br(),

        tags$script(src = "extend2.js")
      ),

      footer = fluidPage(align="center", actionButton("closeArt", label = "OK"))
    ))
  })

  check <- function(item, g)
  {
    if (!is.null(item) && (item!="") && !is.element(item,g))
      return(c(g, item))
    else
      return(g)
  }

  closeSrc <- function()
  {
    removeModal()

    global$srcRedacteur      <- input$srcRedacteur
    global$srcTitel          <- input$srcTitel
    global$srcBrontype       <- input$srcBrontype
    global$srcDeel           <- input$srcDeel
    global$srcJaar           <- input$srcJaar
    global$srcNummer         <- input$srcNummer
    global$srcPlaats         <- input$srcPlaats
    global$srcUitgeverij     <- input$srcUitgeverij
    global$srcEditie         <- input$srcEditie
    global$srcWebsite        <- input$srcWebsite
    global$srcRaadpleging    <- input$srcRaadpleging
    global$srcOpmerkingen    <- input$srcOpmerkingen

    shinyjs::click("copyBron")

    global$Brontype          <- check(input$srcBrontype, global$Brontype)
    global$Plaats            <- check(input$srcPlaats  , global$Plaats)
  }

  closeArt <- function()
  {
    removeModal()

    global$artAuteur         <- input$artAuteur
    global$artTitel          <- input$artTitel
    global$artGenre          <- input$artGenre
    global$artTaal1          <- input$artTaal1
    global$artTaal2          <- input$artTaal2
    global$artOpmerkingen    <- input$artOpmerkingen

    shinyjs::click("copyArtikel")

    global$Genre             <- check(input$artGenre   , global$Genre)
    global$Taal1             <- check(input$artTaal    , global$Taal1)
    global$Taal2             <- check(input$artTaal2   , global$Taal2)
  }

  textSrc <- function()
  {
    return(paste0(
      "$ ", input$srcRedacteur  , "\n",
      "$ ", input$srcTitel      , "\n",
      "$ ", input$srcBrontype   , "\n",
      "$ ", input$srcDeel       , "\n",
      "$ ", input$srcJaar       , "\n",
      "$ ", input$srcNummer     , "\n",
      "$ ", input$srcPlaats     , "\n",
      "$ ", input$srcUitgeverij , "\n",
      "$ ", input$srcEditie     , "\n",
      "$ ", input$srcWebsite    , "\n",
      "$ ", input$srcRaadpleging, "\n",
      "$ ", input$srcOpmerkingen, "\n\n"
    ))
  }

  textArt <- function()
  {
    return(paste0(                "\n",
      "# ", input$artAuteur     , "\n",
      "# ", input$artTitel      , "\n",
      "# ", input$artGenre      , "\n",
      "# ", input$artTaal1      , "\n",
      "# ", input$artTaal2      , "\n",
      "# ", input$artOpmerkingen, "\n\n"
    ))
  }

  observeEvent(input$closeSrc,
  {
    closeSrc()

    if (substr(input$textEditor, 1, 1)!="$")
    {
      Value <- paste0(textSrc(), input$textEditor)
    }
    else
    {
      Table <- read.table(text=input$textEditor,
                          sep              ="\n",
                          quote            = NULL,
                          check.names      = F,
                          blank.lines.skip = F,
                          comment.char     = "")

      Table <- Table[14:nrow(Table),]
      Text  <- paste(Table, collapse = "\n")
      Value <- paste0(textSrc(), Text)
    }

    updateTextAreaInput(
      session = session,
      inputId = 'textEditor',
      label   = NULL,
      value   = Value
    )
  })

  observeEvent(input$closeArt,
  {
    closeArt()
  })

  output$buttonPane <- renderUI(
  {
    req(global$PDF)
    req(global$OCR)

    splitLayout(
      cellWidths = c("21%", "0%", "20%", "20%", "20%", "0%", "19%"),
      align="center",

      div(),
      actionButton("buttonBron"   , label = "Bron"         , width = "60px"),
      rclipButton( "copyBron"     , label = "Copy bron"    , clipText=textSrc(), icon = NULL, width = NULL, modal = F),
      div(),
      actionButton("buttonArtikel", label = "Artikel"      , width = "60px"),
       rclipButton(  "copyArtikel", label = "Copy artikel" , clipText=textArt(), icon = NULL, width = NULL, modal = F),
      div()
    )
  })

  observeEvent(global$Brontype, { fwrite(list(global$Brontype), file = "www/fields/brontype.txt") })
  observeEvent(global$Plaats  , { fwrite(list(global$Plaats)  , file = "www/fields/plaats.txt"  ) })
  observeEvent(global$Genre   , { fwrite(list(global$Genre)   , file = "www/fields/genre.txt"   ) })
  observeEvent(global$Taal1   , { fwrite(list(global$Taal1)   , file = "www/fields/taal1.txt"   ) })
  observeEvent(global$Taal2   , { fwrite(list(global$Taal2)   , file = "www/fields/taal2.txt"   ) })

  output$fontSize <- renderUI(
  {
    req(global$PDF)
    req(global$OCR)

    splitLayout(
      style = 'margin-top: 5px;',
      align = "center",

      cellWidths = c("100px", "80px", "40px", "130px"),

      p(strong("Lettergrootte: ")),
      numericInput("fontSize", label=NULL, value=isolate(global$fontSize)),

      div(),

      actionButton("buttonSave", label = "Wijzigingen opslaan", width = "130px")
    )
  })

  observeEvent(input$fontSize,
  {
    global$fontSize <- input$fontSize
  })







  observeEvent(input$textEditor,
  {
    req(input$textEditor)

    backup <- gsub("(.txt$|.TXT$)", ".bak", global$OCR)

    if (!file.exists(backup))
    {
      old <- readtext(global$OCR)$text

      if (old!=input$textEditor)
        system(paste0("cp ", global$OCR, " ", backup))
    }

    write.table(x = input$textEditor, file = global$OCR, sep = "", row.names = F,  col.names = F, quote = F)
  })

  ##############################################################################

  observeEvent(input$listSources,
  {
    global$resultUD <- NULL
  })

  output$showButtons <- renderUI(
  {
    req(global$OCR)
    fileName <- gsub("(.txt$|.TXT$)", ".xlsx", global$OCR)

    if (!file.exists(fileName))
      return(
        div(
          style = "padding: 6px; text-align: center;",
          actionButton("lemtagButton", "Maak tabel")
        )
      )
    else
      return(
        splitLayout
        (
          style = "border: 1px inherit;",
          cellWidths = c("50%", "50%"),
          cellArgs = list(style = "padding: 6px; text-align: center;"),

          actionButton("lemtagButton1", "Laad bestaande tabel"),
          actionButton("lemtagButton2",    "Maak nieuwe tabel")
        )
      )
  })

  prepare <- function(fileName)
  {
    # read data

    conn <- file(description=fileName, open="r")
    gnText <- data.frame(text=readLines(conn), stringsAsFactors=FALSE)
    close(conn)

    # replace multiple empty lines by one

    for (i in 1:nrow(gnText))
    {
      gnText$text[i] <- str_replace_all(gnText$text[i], "(?<=^)[ ]+(?=$)", "")
    }

    gnText$text[1] <- str_replace_all(gnText$text[1], "(?<=\\p{L})-(?=$)", "--")
    gnCorp <- data.frame(data.frame(text=gnText$text[1]))

    for (i in 2:nrow(gnText))
    {
      if (gnText$text[i]=="")
        if (gnText$text[i-1]!="")
          gnCorp <- rbind(gnCorp, data.frame(text=gnText$text[i]))
        else {}
      else
      {
        gnText$text[i] <- str_replace_all(gnText$text[i], "(?<=\\p{L})-(?=$)", "--")
        gnCorp <- rbind(gnCorp, data.frame(text=gnText$text[i]))
      }
    }

    gnText <- gnCorp

    # find paragraphs

    gnCorp <- data.frame()

    i <- 1

    repeat
    {
      if ((substr(gnText$text[i], 1, 1)=="$") |
          (substr(gnText$text[i], 1, 1)=="#") |
          (gnText$text[i]==""))
        gnCorp <- rbind(gnCorp, data.frame(text=gnText$text[i]))
      else

      {
        s <- ""

        repeat
        {
          s <- paste(s, trimws(gnText$text[i], "both"))

          if ((i==nrow(gnText)) ||
              ((substr(gnText$text[i+1], 1, 1)=="$") |
               (substr(gnText$text[i+1], 1, 1)=="#") |
               (gnText$text[i+1]              =="")))

            break
          else
            i <- i + 1
        }

        gnCorp <- rbind(gnCorp, data.frame(text=s))
      }

      i <- i + 1

      if (i>nrow(gnText))
        break
    }

    gnText <- NULL
    gnCorp$text <- as.character(gnCorp$text)

    for (i in 1:nrow(gnCorp))
    {
      gnCorp$text[i] <- str_replace_all(gnCorp$text[i], "(?<=[:alpha:])--\\h+(?=[:alpha:])", "")
    }

    return(gnCorp)
  }

  checkPunct <- function(s)
  {
    s <- str_replace_all(s, "[,;:!¡?¿\\/\\(\\)\\[\\)\\]\\{\\}«»…%]"          , " \\0 ")

    s <- str_replace_all(s, "(?<=[:alpha:])['](?=[:alpha:])", "’" )

    s <- str_replace_all(s, "(?<![:space:])[.'’””‘“„´\\-\\–\\—]$"            , " \\0" )
    s <- str_replace_all(s, "(?<![:space:])[.'’””‘“„´\\-\\–\\—](?![:alpha:])", " \\0" )
    s <- str_replace_all(s, "(?<![:alpha:])[.'’””‘“„´\\-\\–\\—](?![:space:])",  "\\0 ")

    s <- str_replace_all(s, "(?<=[:digit:]) \\. (?=[:digit:])", "." )

    s <- str_replace_all(s, '(?<![:space:])["]$'                             , " \\0" )
    s <- str_replace_all(s, '(?<![:space:])["](?![:alpha:])'                 , " \\0" )
    s <- str_replace_all(s, '(?<![:alpha:])["](?![:space:])'                 ,  "\\0 ")

    s <- str_replace_all(s, "(?<=(^|[:space:]))d ' ", "d’ " )
    s <- str_replace_all(s, "(?<=(^|[:space:]))d ´ ", "d’ " )
    s <- str_replace_all(s, "(?<=(^|[:space:]))d ‘ ", "d’ " )
    s <- str_replace_all(s, "(?<=(^|[:space:]))d ’ ", "d’ " )
    s <- str_replace_all(s, "(?<=(^|[:space:]))D ’ ", "D’ " )
    s <- str_replace_all(s, "(?<=(^|[:space:]))j ' ", "z’ " )
    s <- str_replace_all(s, "(?<=(^|[:space:]))j ´ ", "z’ " )
    s <- str_replace_all(s, "(?<=(^|[:space:]))j ‘ ", "z’ " )
    s <- str_replace_all(s, "(?<=(^|[:space:]))j ’ ", "z’ " )
    s <- str_replace_all(s, "(?<=(^|[:space:]))z ' ", "z’ " )
    s <- str_replace_all(s, "(?<=(^|[:space:]))z ´ ", "z’ " )
    s <- str_replace_all(s, "(?<=(^|[:space:]))z ‘ ", "z’ " )
    s <- str_replace_all(s, "(?<=(^|[:space:]))z ’ ", "z’ " )

    s <- str_replace_all(s, "(?<=(^|[:space:]))d'(?=[:alpha:])", "d’ " )
    s <- str_replace_all(s, "(?<=(^|[:space:]))d´(?=[:alpha:])", "d’ " )
    s <- str_replace_all(s, "(?<=(^|[:space:]))d‘(?=[:alpha:])", "d’ " )
    s <- str_replace_all(s, "(?<=(^|[:space:]))d’(?=[:alpha:])", "d’ " )
    s <- str_replace_all(s, "(?<=(^|[:space:]))D’(?=[:alpha:])", "D’ " )
    s <- str_replace_all(s, "(?<=(^|[:space:]))j'(?=[:alpha:])", "j’ " )
    s <- str_replace_all(s, "(?<=(^|[:space:]))j´(?=[:alpha:])", "j’ " )
    s <- str_replace_all(s, "(?<=(^|[:space:]))j‘(?=[:alpha:])", "j’ " )
    s <- str_replace_all(s, "(?<=(^|[:space:]))j’(?=[:alpha:])", "j’ " )
    s <- str_replace_all(s, "(?<=(^|[:space:]))z'(?=[:alpha:])", "z’ " )
    s <- str_replace_all(s, "(?<=(^|[:space:]))z´(?=[:alpha:])", "z’ " )
    s <- str_replace_all(s, "(?<=(^|[:space:]))z‘(?=[:alpha:])", "z’ " )
    s <- str_replace_all(s, "(?<=(^|[:space:]))z’(?=[:alpha:])", "z’ " )

    s <- str_replace_all(s, "(?<=[:alpha:])'e(?=[:space:])", " ’e" )
    s <- str_replace_all(s, "(?<=[:alpha:])´e(?=[:space:])", " ’e" )
    s <- str_replace_all(s, "(?<=[:alpha:])‘e(?=[:space:])", " ’e" )
    s <- str_replace_all(s, "(?<=[:alpha:])’e(?=[:space:])", " ’e" )
    s <- str_replace_all(s, "(?<=[:alpha:])'k(?=[:space:])", " ’k" )
    s <- str_replace_all(s, "(?<=[:alpha:])´k(?=[:space:])", " ’k" )
    s <- str_replace_all(s, "(?<=[:alpha:])‘k(?=[:space:])", " ’k" )
    s <- str_replace_all(s, "(?<=[:alpha:])’k(?=[:space:])", " ’k" )

    s <- str_replace_all(s, "(?<=(da))'s(?=[:space:])", " ’s" )
    s <- str_replace_all(s, "(?<=(da))´s(?=[:space:])", " ’s" )
    s <- str_replace_all(s, "(?<=(da))‘s(?=[:space:])", " ’s" )
    s <- str_replace_all(s, "(?<=(da))’s(?=[:space:])", " ’s" )
    s <- str_replace_all(s, "(?<=(Da))'s(?=[:space:])", " ’s" )
    s <- str_replace_all(s, "(?<=(Da))´s(?=[:space:])", " ’s" )
    s <- str_replace_all(s, "(?<=(Da))‘s(?=[:space:])", " ’s" )
    s <- str_replace_all(s, "(?<=(Da))’s(?=[:space:])", " ’s" )

    s <- str_replace_all(s, "(?<=\\.)([:space:]\\.)", ".")

    s <- str_replace_all(s, "(?<=((^|[:punct:]|[:space:])[:upper:]))([:space:]\\.)(?!([\\.]|([:space:]*$)))", ".")

    return(s)
  }

  lemtag <- function(gnCorp)
  {
    show_modal_spinner(text = HTML("<br>Woordsoorten worden toegevoegd, dit kan even duren.<br>Sluit de pagina niet af."))

    UDgn <- udpipe_load_model(file = "www/nds_gronings-ud-GitHub-demo.udpipe")
    GNtagged <- data.frame()

    l <- 0

    for (i in 1:nrow(gnCorp))
    {
      if ((substr(gnCorp$text[i], 1, 1)=="$") |
          (substr(gnCorp$text[i], 1, 1)=="#") |
          (       gnCorp$text[i]       ==""))
      {
        df <- data.frame(token_GN = gnCorp$text[i],
                         lemma_NL = "",
                         upos     = "",
                         line     = 0 ,
                         note     = "")

        GNtagged <- rbind(GNtagged, df)
      }
      else
      {
        t <- checkPunct(as.character(gnCorp$text[i]))
        t <- gsub("’t ", "’T ", t)
        t <- gsub("'t ", "'T ", t)
        s <- unlist(tokenize_sentences(t))
        s <- gsub("’T ", "’t ", s)
        s <- gsub("'T ", "'t ", s)

        for (j in (1:length(s)))
        {
          l <- l + 1

          df <- as.data.frame(udpipe_annotate(object=UDgn, x = s[j], parser = "none"))
          df <- data.frame(token_GN = df$token,
                           lemma_NL = df$lemma,
                           upos     = df$upos,
                           line     = rep(l , nrow(df)),
                           note     = rep("", nrow(df)))

          GNtagged <- rbind(GNtagged, df)
        }
      }
    }

    GNtagged$doc_id        <- NULL
    GNtagged$paragraph_id  <- NULL
    GNtagged$sentence_id   <- NULL
    GNtagged$sentence      <- NULL
    GNtagged$token_id      <- NULL
    GNtagged$xpos          <- NULL
    GNtagged$feats         <- NULL
    GNtagged$head_token_id <- NULL
    GNtagged$dep_rel       <- NULL
    GNtagged$deps          <- NULL
    GNtagged$misc          <- NULL

    remove_modal_spinner()

    return(GNtagged)
  }

  lemtagread <- function()
  {
    fileName <- gsub("(.txt$|.TXT$)", ".xlsx", global$OCR)
    result <- read.xlsx(fileName)
    result[is.na(result)] <- ""

    return(result)
  }

  observeEvent(input$lemtagButton,
  {
    global$resultUD <- lemtag(prepare(global$OCR))
  }, ignoreInit = TRUE)

  observeEvent(input$lemtagButton1,
  {
    global$resultUD  <- lemtagread()
  }, ignoreInit = TRUE)

  observeEvent(input$lemtagButton2,
  {
    global$resultUD <- lemtag(prepare(global$OCR))
  }, ignoreInit = TRUE)

  observe(
    js$getSize()
  )

  output$resultUD <- renderRHandsontable(
  {
    req(global$resultUD)

    hot_col(hot    = rhandsontable(global$resultUD, width = input$width - 150, height = input$height - 270, colWidths = c(120, 370, 270, 117, 60), rowHeaders=T, search = T),
            col    = "line",
            format = "0a")
  })

  observe(
  {
    req(global$resultUD)
    req( input$resultUD)

    fileName <- gsub("(.txt$|.TXT$)", ".xlsx", global$OCR)
    write.xlsx(hot_to_r(input$resultUD), fileName, sheetName = "table", headerStyle = createStyle(textDecoration = "BOLD"), rowNames=FALSE, colNames=TRUE, na.string = "", firstRow = TRUE)
  })

  ##############################################################################

  output$showButton <- renderUI(
  {
    req(global$resultUD)
    req( input$resultUD)

    fileName <- gsub("(.txt$|.TXT$)", ".xml", global$OCR)

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
    req(global$resultUD)
    req( input$resultUD)
    req(resultXML())

    uiOutput('thanks')
  })

  write.xml <- function(resultUD, fileName)
  {
    show_modal_spinner(text = HTML("<br>Resultaten worden opgeslagen, dit kan even duren.<br>Sluit de pagina niet af."))

    resultUD$token_GN <- as.character(resultUD$token_GN)
    resultUD$lemma_NL <- as.character(resultUD$lemma_NL)
    resultUD$upos     <- as.character(resultUD$upos)
    resultUD$line     <- as.character(resultUD$line)
    resultUD$note     <- as.character(resultUD$note)

    cat('<?xml version="1.0" encoding="UTF-8"?>\n', file=fileName, append=F)
    cat('\n', file=fileName, append=T)
    cat('<document>\n', file=fileName, append=T)

    firstSrc <- TRUE
    firstArt <- TRUE

    if ((is.na(resultUD$token_GN[1])) || (substr(resultUD$token_GN[1], 1, 1) == ""))
      i <- 1
    else
      i <- 0

    while (i < nrow(resultUD))
    {
      i <- i + 1

      if (firstSrc & firstArt & (!is.na(resultUD$token_GN[i]) &&
         ((substr(resultUD$token_GN[i], 1, 1) != "") & (substr(resultUD$token_GN[i], 1, 1) != "$") & (substr(resultUD$token_GN[i], 1, 1) != "#"))))
      {
        cat('  <source>\n'   , file=fileName, append=T)
        cat('    <article>\n', file=fileName, append=T)
        cat('      <text>\n' , file=fileName, append=T)
        cat('        <p>\n'  , file=fileName, append=T)

        firstSrc <- FALSE
        firstArt <- FALSE

        if ((!is.na(resultUD$token_GN[i+1])) && (substr(resultUD$token_GN[i+1], 1, 1) != ""))
          i <- i - 1
      }
      else

      if ((!is.na(resultUD$token_GN[i])) && (substr(resultUD$token_GN[i], 1, 1) == "$"))
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
        cat("      <meta name='redacteurOFauteur'>"      , substr(resultUD$token_GN[i+ 2], 3, nchar(resultUD$token_GN[i+ 2])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("      <meta name='titel'>"                  , substr(resultUD$token_GN[i   ], 3, nchar(resultUD$token_GN[i   ])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("      <meta name='bron'>"                   , substr(resultUD$token_GN[i+ 1], 3, nchar(resultUD$token_GN[i+ 1])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("      <meta name='deelOFvolume'>"           , substr(resultUD$token_GN[i+ 4], 3, nchar(resultUD$token_GN[i+ 4])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("      <meta name='jaar'>"                   , substr(resultUD$token_GN[i+ 5], 3, nchar(resultUD$token_GN[i+ 5])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("      <meta name='nummerOFaflevering'>"     , substr(resultUD$token_GN[i+ 6], 3, nchar(resultUD$token_GN[i+ 6])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("      <meta name='plaatsVanUitgave'>"       , substr(resultUD$token_GN[i+ 9], 3, nchar(resultUD$token_GN[i+ 9])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("      <meta name='uitgeverij'>"             , substr(resultUD$token_GN[i+10], 3, nchar(resultUD$token_GN[i+10])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("      <meta name='editieOFdruk'>"           , substr(resultUD$token_GN[i+12], 3, nchar(resultUD$token_GN[i+12])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("      <meta name='adresWebsite'>"           , substr(resultUD$token_GN[i+14], 3, nchar(resultUD$token_GN[i+14])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("      <meta name='datumRaadplegingWebsite'>", substr(resultUD$token_GN[i+15], 3, nchar(resultUD$token_GN[i+15])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("      <meta name='opmerkingen'>"            , substr(resultUD$token_GN[i+16], 3, nchar(resultUD$token_GN[i+16])), "</meta>\n", file=fileName, sep = "", append=T)
        cat('    </metadata>\n\n', file=fileName, append=T)

        i <- i + 16

        if ((is.na(resultUD$token_GN[i+1])) || (substr(resultUD$token_GN[i+1], 1, 1) == ""))
        {
          i <- i + 1

          if (!is.na(resultUD$token_GN[i+1]) && (substr(resultUD$token_GN[i+1], 1, 1) != "#"))
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
          if (!is.na(resultUD$token_GN[i+1]) && (substr(resultUD$token_GN[i+1], 1, 1) != "#"))
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

      if ((!is.na(resultUD$token_GN[i])) && (substr(resultUD$token_GN[i], 1, 1) == "#"))
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
        cat("        <meta name='auteur'>"               , substr(resultUD$token_GN[i   ], 3, nchar(resultUD$token_GN[i   ])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("        <meta name='titel'>"                , substr(resultUD$token_GN[i+ 1], 3, nchar(resultUD$token_GN[i+ 1])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("        <meta name='genre'>"                , substr(resultUD$token_GN[i+ 2], 3, nchar(resultUD$token_GN[i+ 2])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("        <meta name='taal1'>"                , substr(resultUD$token_GN[i+ 3], 3, nchar(resultUD$token_GN[i+ 3])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("        <meta name='taal2'>"                , substr(resultUD$token_GN[i+ 4], 3, nchar(resultUD$token_GN[i+ 5])), "</meta>\n", file=fileName, sep = "", append=T)
        cat("        <meta name='opmerkingen'>"          , substr(resultUD$token_GN[i+ 5], 3, nchar(resultUD$token_GN[i+ 5])), "</meta>\n", file=fileName, sep = "", append=T)
        cat('      </metadata>\n\n', file=fileName, append=T)
        cat('      <text>\n'       , file=fileName, append=T)
        cat('        <p>\n'        , file=fileName, append=T)

        i <- i + 14

        if ((is.na(resultUD$token_GN[i+1])) || (substr(resultUD$token_GN[i+1], 1, 1) == ""))
          i <- i + 1
      }
      else

      if ((!is.na(resultUD$token_GN[i])) && (substr(resultUD$token_GN[i], 1, 1) != ""))
      {
        cat('          <w lemma="', resultUD$lemma_NL[i], '" pos="', resultUD$upos[i], '">', resultUD$token_GN[i], '</w>\n', file=fileName, sep = "", append=T)
      }
      else

      if ((is.na(resultUD$token_GN[i])) || (substr(resultUD$token_GN[i], 1, 1) == ""))
      {
        if ((!is.na(resultUD$token_GN[i-1]) && (substr(resultUD$token_GN[i-1], 1, 1) != "$") & (substr(resultUD$token_GN[i-1], 1, 1) != "#")) &
            (!is.na(resultUD$token_GN[i+1]) && (substr(resultUD$token_GN[i+1], 1, 1) != "$") & (substr(resultUD$token_GN[i+1], 1, 1) != "#")))
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
    req(global$resultUD)
    req( input$resultUD)

    fileName <- gsub("(.txt$|.TXT$)", ".xml" , global$OCR)
    write.xml(as.data.frame(hot_to_r(input$resultUD)), fileName)
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
      shinyjs::hide(selector = "#navBar li a[data-value=metadata]")
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
