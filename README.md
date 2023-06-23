# Spotlight-pipeline

In this repository software is made available that has been used in a pipeline in which printed texts were digitized and assembled into a searchable corpus. Originally the pipeline was developed for the digitalization of sources that were written in the Groningen regional language (Gronings), but (with small adaptions) the pipeline can be used for any language.

The Groningen regional language is spoken in the Dutch province of Groningen, and around the Groningen border in the provinces of Drenthe and Fryslân. Gronings forms the northernmost part of Dutch Low Saxon that is recognized as a regional language under part II of the European Charter for Regional or Minority Languages.

This document describes the workflow and the tools that were used for digitizing about 800 sources (books, journals) that were written in Gronings and stored in the library of the University of Groningen. The pipeline for digitizing the 800 sources includes the following steps:

1. Scanning
2. Optical character recognition (OCR)
3. Adding metadata
4. Checking the texts
5. Adding lemma’s and part-of-speech-tags
6. Saving as XML files

Each of the  steps is described below.

## 1. Scanning
The books and other sources were scanned by volunteers using Ricoh copiers that were available at the library of the University of Groningen. The scans were saved as PDF or TIFF format and sent by e-mail to a folder on Google Drive.

## 2.  Optical character recognition (OCR)
Optical character recognition was done by volunteers using ABBYY FineReader PDF, an optical character recognition application developed by ABBYY. The program was installed on laptops that were borrowed by volunteers. The scans (see the previous step) were sent and distributed among the laptops and subsequently processed by the volunteers using the OCR program. This resulted in PDFs with an added text layer which were send back to Google Drive.

## 3. Adding metadata
In this step metadata is added. The metadata was partly available through the library of the University of Groningen, partly found in the sources themselves, and partly needed to be looked up, for example details about the authors.

When considering a data structure, we considered that each source consists of one or more articles. This is the case for magazines, newspapers and books. A book is then a source with only one article. Metadata must be added for each source as well as for each article within a source. Metadata about the source as a whole is placed at the beginning of the text file that contains the OCR text, and metadata about an article within a source is placed at the beginning of the article.

For a source the following metadata can be included: editor, title, source type (book, journal, newpaper, website), series, year, number, place of publication, publisher, edition (or printing), url of website, date of consulting website, comments.

For a article the following metadata can be included: author, title, genre (prose, poetry), language1 (normally Gronings), language2 (if another language is used as well), comments.

We developed a web app that can be used for entering the meta data. The app is found in this GitHub repository in the folder `addMetaData_CheckText`. The app is implemented in R Shiny. Knowledge about R and the R package Shiny is required in order to install this app.

The app takes the PDFs with text layers as input. When a volunteer processes a PDF, the text layer is extracted from the PDF, and the PDF and the text are shown side by side in the app. In order to enter the meta data of a source, the volunteer selects a source and clicks on **Voer metadata in**. Then in the next screen the volunteer clicks on **Bron** and enters  the data in the input fields. When clicking OK, the data is pasted at the beginning of the text. All metadata lines of the source start with $. Then for each article the volunteer clicks the **Artikel** button and enters the data. After clicking OK, the data is copied to  the clipboard. Then the volunteer clicks in the text right before where the article begins and pastes the data. All meta data lines of the article start with #.

For an extensive manual see `handleiding.pdf` in the folder `addMetaData_CheckText`.

The login credentials of  the app are: 
user name: woordwaark
password: w00rdw@@rk

Further comments:

1. db.xlsx contains meta data that will be automatically filled in in the input fields when the volunteer clicks the **Bron** button.
2. The PDFs with textlayer are put in the subfolder `docs`.
3. Credentials of users that are authorized to use  the program are found in the file `data.csv` which is found in the subfolder `auth`.

## 4. Checking the texts

The OCR text is also checked in the same app in which the metadata is added. The check includes the following:

1. Correcting glaring OCR errors such as wrong and/or strange characters.
2. We are only interested in sentences that are written in Gronings.  So (large amounts of) text that is not written in Gronings, and data like a title page, a table of contents or a colophon are removed.

## 5. Adding lemma’s and part-of-speech-tags

We developed a lemmatizer which lemmatizes tokens in Gronings to lemmas in Dutch. Assigning Dutch lemmas to tokens in texts that are written in Gronings is important for two reasons:

1. This allows the user to search the corpus in both Gronings (via the tokens) and Dutch (lemmas);
2. Regional, morphological and spelling variants of a word are 'linked' in this way. For example, if a user searches via the Dutch word _huis_ (English house), sentences with all Groningen variants are found: _hoes_, _huus_, _hoeske_, _huusie_, etc. If the user searches for the Groningen word _hoes_, it is possible not only to find sentences that include the word _hoes_, but also sentences that include _huus_, _hoeske_ and _huusie_.

To be able to lemmatize, a computer model must be trained on the basis of a training corpus. Our training corpus consisted of six texts in Groningen, a file of 109765 tokens, 93739 words and 6513 sentences. When allocating the lemmas, a Dutch cognate was chosen where possible. If there was no cognate in Dutch for the Groningen word, a non-cognate was chosen.




## 6. Saving as XML files
