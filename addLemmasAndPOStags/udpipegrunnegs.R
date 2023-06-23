#!/usr/bin/Rscript --vanilla

################################################################################
#                                                                              #
# Script for tokenizing, lemmatizing and POS-tagging of text that is written   #
# in the Groningen regional language.                                          #
#                                                                              #
# Before running the script install the libraries libcurl, libxml-2.0 and      #
# poppler-cpp. When using Ubuntu 20.04 or later, enter the following commands  #
# in a terminal:                                                               #
#                                                                              #
# $ sudo apt install libssl-dev                                                #
# $ sudo apt install libcurl4-openssl-dev                                      #
# $ sudo apt install libxml2-dev                                               #
# $ sudo apt install libpoppler-cpp-dev                                        #
#                                                                              #
# When running the script for the first time, missing R packages are installed #
# which can take a while!                                                      #
#                                                                              #
# The file 'nds_gronings-ud-GitHub_demo.udpipe' should reside in the same      #
# directory as where this script is stored.                                    #
#                                                                              #
# The file 'Grunnegs.txt' is added as an example text. It can be processed by  #
# entering the following command in a terminal:                                #
#                                                                              #
# $ ./udpipegrunnegs.R -i Grunnegs.txt -f -o Grunnegs.xlsx -x                  #
#                                                                              #
# where the result is stored as an Excel spreadsheet in Grunnegs.xlsx          #
#                                                                              #
# or read from pipe:                                                           #
#                                                                              #
# $ cat Grunnegs.txt|./udpipegrunnegs.R -i - -e -t > Grunnegs.tsv              #
#                                                                              #
# or read from user input (close with Ctrl-d):                                 #
#                                                                              #
# $ ./udpipegrunnegs.R -i - -e -t > Grunnegs.tsv                               #
#                                                                              #
# where the result is stored as a tab-separated file in Grunnegs.tsv.          #
#                                                                              #
# For information about usage and options enter:                               #
#                                                                              #
# $ ./udpipegrunnegs.R -h                                                      #
#                                                                              #
# Copyright: Fryske Akademy, Leeuwarden, The Netherlands, 24 March 2022.       #
# Contact  : wheeringa@fryske-akademy.nl                                       #
#                                                                              #
################################################################################

# install and load packages

packages = c("optparse", "readr", "readtext", "xml2", "rvest", "openxlsx", "stringr", "udpipe")

for (p in packages)
{
  if (suppressWarnings((!library(p, character.only=T, logical.return = T, quietly = T))))
  {
    cat("\nInstalling package", p, "...\n", file = stderr())
    suppressWarnings(install.packages(p, quiet = T))
  }

  if (suppressWarnings((!library(p, character.only=T, logical.return = T, quietly = T))))
  {
    cat("\nPackage", p, "not installed!\n\n", file = stderr())
    quit(status=1)
  }
}

usage ="usage: %prog -i INPUT -e|-f|-w -o OUTPUT -t|-x|-c"

option_list = list(
  make_option(c("-i", "--input" ), action="store"     , help="text or file name or url of website"),

  make_option(c("-e", "--text"  ), action="store_true", help="input  is some text between ' and '"),
  make_option(c("-f", "--file"  ), action="store_true", help="input  is text file"),
  make_option(c("-w", "--web"   ), action="store_true", help="input  is URL of website"),

  make_option(c("-o", "--output"), action="store"     , help="name of output file"),

  make_option(c("-t", "--tsv"   ), action="store_true", help="output is tab-separated file" ),
  make_option(c("-x", "--xlsx"  ), action="store_true", help="output is Microsoft Excel file"),
  make_option(c("-c", "--connlu"), action="store_true", help="output is CoNLL-U file")
);

option_parser <- OptionParser(usage=usage, option_list=option_list)
opt <- parse_args(option_parser)

if (length(opt) == 1)
{
  print_help(option_parser)
  quit(status=1)
}

if (is.null(opt$input))
{
  con <- file("stdin")
  string <- scan(con, what=character(), quote="")
  close(con)
}                    else
if (unlist(opt$input) == "-")
{
  con <- file("stdin")
  string <- scan(con, what=character(), quote="")
  close(con)
}                    else
{
  string <- unlist(opt$input)
}

if ((!is.null(opt$text) && (opt$text==T)) & ( is.null(opt$file)) & ( is.null(opt$web)))
  input <- "text"    else

if (( is.null(opt$text)) & (!is.null(opt$file) && (opt$file==T)) & ( is.null(opt$web)))
  input <- "file"    else

if (( is.null(opt$text)) & ( is.null(opt$file)) & (!is.null(opt$web) && (opt$web==T)))
  input <- "web"     else
{
  print_help(option_parser)
  quit(status=1)
}

if (is.null(opt$output))
{
  result <- stdout()
}                    else
if (unlist(opt$output) == "-")
{
  result <- stdout()
}                    else
{
  result <- unlist(opt$output)
}

if ((!is.null(opt$tsv) && (opt$tsv==T)) & (is.null(opt$xlsx)) & (is.null(opt$connlu)))
  output <- "tsv"    else

if ((is.null(opt$tsv)) & (!is.null(opt$xlsx) && (opt$xlsx==T)) & (is.null(opt$connlu)))
  output <- "xlsx"   else

if ((is.null(opt$tsv)) & (is.null(opt$xlsx)) & (!is.null(opt$connlu) && (opt$connlu==T)))
  output <- "connlu" else
{
  print_help(option_parser)
  quit(status=1)
}

# read data

if (input=="text")
{
  s <- string
}

if (input=="file")
{
  if (file.exists(string))
  {
    s <- readtext(file = string, encoding = "UTF-8")

    s <- subset(s, !grepl("^\\$", text))
    s <- subset(s, !grepl("^\\#", text))

    s <- lapply(s, function(x) str_replace(x, "(?<=[:alpha:])(\\-[ ]*$)", "@@@"))

    s <- paste0(s$text, sep = "", collapse = " ")

    s <- gsub("\\f" , "", s)
    s <- gsub("\\a" , "", s)
    s <- gsub("\\n" , "", s)

    s <- gsub("@@@ ", "", s)
  }
  else
  {
    cat("File ", string, "not found.\n", file = stderr())
    quit(status=1)
  }
}

if (input=="web")
{
  site <- NULL

  tryCatch(
    site <- read_html(string),
    error   = function(something) {},
    warning = function(something) {}
  )

  if (length(site)>0)
  {
    text <- html_text(html_nodes(site, 'p'))
    text <- gsub("\n", "", text)
    text <- gsub("([)[0-9]+(]))", "", text)

    text <- data.frame(text)
    text <- subset(text, str_count(text, "\\w+") > 1)
    text <- subset(text, grepl("[A-Z|a-z]", text))

    if (nrow(text) > 0)
      s <- paste(text$text, sep = "", collapse = "\n\n")
    else
      s <- ""
  }
  else
  {
    cat("\nNo website found at given URL!\n\n", file = stderr())
    quit(status=1)
  }
}

s <- paste(s, collapse = " ")

if (trimws(s) == "")
  s <- "@#@#"

# load model, tokenize, lemmatize, tag

checkPunct <- function(s)
{
  s <- str_replace_all(s, "[,;:!¡?¿\\/\\(\\)\\[\\)\\]\\{\\}«»…%]"          , " \\0 ")

  s <- str_replace_all(s, "(?<=[:alpha:])['](?=[:alpha:])", "’" )
  s <- str_replace_all(s, '"', '”')

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

resultUD <- as.data.frame(udpipe(x = checkPunct(s), object = "nds_gronings-ud-GitHub-demo.udpipe", parser = "none"))
resultUD$doc_id        <- gsub("\\.txt", "", string)
resultUD$start         <- NULL
resultUD$end           <- NULL
resultUD$term_id       <- NULL

resultUD$xpos          <- NULL
resultUD$feats         <- NULL
resultUD$head_token_id <- NULL
resultUD$dep_rel       <- NULL
resultUD$deps          <- NULL

# find start and end positions in text

as_table <- function(resultUD)
{
  s0 <- s

  resultUD <- data.frame(resultUD, start=NA, end=NA)

  for (i in 1:nrow(resultUD))
  {
    token <- resultUD$token[i]
    pos1 <- unlist(gregexpr(pattern = token, text = s0, fixed=T))[1]

    if (pos1==-1)
    {
      token <- str_replace_all(token, "’", "'")
      token <- str_replace_all(token, '”', '"')

      pos1 <- unlist(gregexpr(pattern = token, text = s0, fixed=T))[1]
    }

    pos2 <- pos1 + nchar(token) - 1

    resultUD$start[i] <- pos1
    resultUD$end  [i] <- pos2

    substr(s0, pos1, pos2) <- paste(rep("_", nchar(token)), collapse = "")
  }

  resultUD <- subset(resultUD, token!="@#@#")

  return(resultUD)
}

# write result

if  (output=="tsv")
  write.table(as_table(resultUD), result, sep = "\t", na = "", dec = ".", quote = TRUE, qmethod = "double", row.names = FALSE, col.names = TRUE, fileEncoding = "UTF-8")

if ((output=="xlsx") & (result!=stdout()))
{
  write.xlsx(as_table(resultUD), result, sheetName = "table", headerStyle = createStyle(textDecoration = "BOLD"), rowNames=FALSE, colNames=TRUE, na.string = "", firstRow = TRUE)
} else

if ((output=="xlsx") & (result==stdout()))
  cat("\nMicrosoft Excel file cannot be printed to stdout!\n", file = stderr())

if  (output=="connlu")
  writeLines(as_conllu(resultUD), result)

cat("\nDONE\n\n", file = stderr())
