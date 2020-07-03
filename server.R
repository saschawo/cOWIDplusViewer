# To-Dos:

library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)
library(scales)
library(DT)
library(stringr)
library(stringi)

unigram.df <- fread("data/grams.csv")

commad <- comma_format(big.mark = " ", decimal.mark = ",")

shinyServer(function(input, output, session) {
    rV <- reactiveValues()
    observeEvent(eventExpr = input$button, ignoreNULL = F, {
        
        # Dealing with faulty dates
        beg.date <- input$dateRange[1]
        end.date <- input$dateRange[2]
        
        if (is.na(beg.date)) beg.date <- "2020-01-01"
        if (is.na(end.date)) end.date <- "2020-07-02"
        
        if (beg.date > end.date) {
            beg.date <- "2020-01-01"
            end.date <- "2020-07-02" }
        
        # Creating date range vector
        date.range <- as.Date(as.Date(beg.date):as.Date(end.date), origin = "1970-01-01")
        
        # Cleaning search patterns
        original.pattern <- input$searchPatterns
    
        clean.pattern <- tolower(gsub("^", "", original.pattern, fixed = T))
        clean.pattern <- gsub("$", "", clean.pattern, fixed = T)
        clean.pattern <- gsub("+", "", clean.pattern, fixed = T)
        clean.pattern <- gsub(".", "", clean.pattern, fixed = T)
        clean.pattern <- gsub("*", "", clean.pattern, fixed = T)
        clean.pattern <- gsub("[", "", clean.pattern, fixed = T)
        clean.pattern <- gsub("]", "", clean.pattern, fixed = T)
        clean.pattern <- gsub("|", "", clean.pattern, fixed = T)
        clean.pattern <- gsub("(", "", clean.pattern, fixed = T)
        clean.pattern <- gsub(")", "", clean.pattern, fixed = T)
        clean.pattern <- gsub(";", "", clean.pattern, fixed = T)
        
        # Replace multiple commas with one and clean leading or ending commas
        clean.pattern <- gsub(",+", ",", clean.pattern)
        clean.pattern <- gsub("^,", "", clean.pattern)
        clean.pattern <- gsub(",$", "", clean.pattern)
        
        if (clean.pattern == "") { 
            clean.pattern <- "coronavirus"
            showNotification("Search pattern not valid. Set to 'coronavirus' automatically.",
                             duration = 10, closeButton = T, type = "error")
            updateTextInput(session, "searchPatterns", value = "coronavirus")
        }
        
        # Parse search patterns
        sep.patterns <- str_trim(strsplit(clean.pattern, ",", fixed = T)[[1]])
        
        # check for duplicate search patterns
        sep.patterns <- unique(sep.patterns)
        
        # check for 4 to 6 search patterns
        if (length(sep.patterns) %in% 4:6) {
            showNotification("Please note: The more search patterns you are using, the longer the calculation will take.",
                             duration = 10, closeButton = T, type = "warning")
        }
        
        # Too many search patterns?
        if (length(sep.patterns) > 6) {
            showNotification("Too many search patterns. The first 6 patterns (in alphabetical order) will be used. This could take a while.",
                             duration = 10, closeButton = T, type = "error")
            sep.patterns <- sep.patterns[1:6] }
        
        # Processing search patterns in lapply loop, result:
        # list with n_patterns elements, each has two elements:
        # 1. hit.pat (dataframe for plotting)
        # 2. hit.pat.inst (dataframe for DTs)
        result.list <- lapply(sep.patterns, FUN = function (pat.i) {
            t0 <- Sys.time()
            # is pattern a bigram?
            is.bigram <- grepl(" ", pat.i, fixed = T)
            # check for very short search patterns
            if (nchar(pat.i) < 3 & input$inOrAll == "in") {
                showNotification("Very short (< 3 letters) patterns can lead to a considerable decrease in performance.", duration = 10, closeButton = T, type = "warning")
            }
            
            # Pasting ^$ if exclusive search is selected
            if (!is.bigram) { # if unigram
                pat.i.label <- ifelse(input$inOrAll == "in",
                                      paste0("*", pat.i, "*"), # label with * if in-search
                                      pat.i) # delete regex if all-search
                pat.i <- ifelse(input$inOrAll == "in",
                                paste0("^\\S*", pat.i, "\\S*$"),
                                paste0("^", pat.i, "$")) 
            } else { # if bigram
                cat("Bigram search.\n")
                pat.i.label <- pat.i
                pat.i <- paste0("^", pat.i, "$")
            }
            
            # Getting hits
            cat("Getting hits.\n")
            hit.pat.all <- unigram.df[stri_detect_regex(unigram.df$entry, pat.i),]
            #hit.pat.all <- unigram.df[grepl(pat.i, unigram.df$entry),]
            
            # Aggregating over word forms
            cat("Aggregating.\n")
            if (nrow(hit.pat.all) > 0) { # Are there any hits?
                hit.pat.all <- as.data.table(hit.pat.all)
                hit.pat <- hit.pat.all[, list(freq = sum(freq), rel.freq = sum(rel.freq)), by = date]
                #hit.pat <- aggregate(cbind(freq, rel.freq) ~ date, data = hit.pat.all, FUN = sum)
            } else { # If not, return an empty data.frame
                hit.pat <- data.frame(date = as.character(date.range),
                                      freq = 0,
                                      rel.freq = 0)
                hit.pat.all <- data.frame(date = as.character(date.range),
                                          entry = pat.i,
                                          freq = 0,
                                          rel.freq = 0)
            }
            
            # Fixing empty dates
            cat("Fixing empty dates.\n")
            empty.dates <- as.character(date.range[!(date.range %in% as.Date(hit.pat$date))])
            if (length(empty.dates) > 0) { # Only do if there are any empty dates
                hit.pat <- rbind(hit.pat,
                                  data.frame(date = empty.dates,
                                             freq = 0,
                                             rel.freq = 0)) }
            hit.pat <- hit.pat[order(hit.pat$date),] # Order by date (because, maybe, there are empty dates at the end)
            
            # Calculating rolling mean
            cat("Calculating rolling mean.\n")
            # First, deal with faulty inputs
            rmean.window <- input$rollWindow
            if (is.na(as.numeric(input$rollWindow))) {
                rmean.window <- 1
                updateNumericInput(session, inputId = "rollWindow", value = 1)
            }
            else if (input$rollWindow < 1) { 
                rmean.window <- 1
                updateNumericInput(session, inputId = "rollWindow", value = 1) }
            else if (input$rollWindow > 14) {
                rmean.window <- 14
                updateNumericInput(session, inputId = "rollWindow", value = 14) }
            rV$rmean.window <- rmean.window
            
            hit.pat$rmean <- frollmean(hit.pat$rel.freq, rmean.window, align = "center")
            
            # Including search pattern in data.frame
            hit.pat$search.pattern <- pat.i
            
            # Overall type frequencies for DTs at the bottom (aggregating by date)
            cat("Calculating overall type frequency.\n")
            hit.pat.inst <- aggregate(freq ~ entry, data = hit.pat.all, FUN = sum)
            hit.pat.inst <- hit.pat.inst[order(hit.pat.inst$freq, decreasing = T),]
            
            # Assigning pattern label
            hit.pat$pattern.label <- pat.i.label
            
            cat(round(difftime(Sys.time(), t0, units = "secs"), 3), "seconds.\n")
            list(hit.pat = hit.pat,
                 hit.pat.inst = hit.pat.inst)
        }) # pattern loop finished!
        
        # Adding information to reactive values
        rV$dateRange <- c(beg.date, end.date)
        
        # Creating overall plotting dataframe
        plot.df <- lapply(result.list, FUN = function (x) x[["hit.pat"]])
        rV$plot.df <- as.data.frame(rbindlist(plot.df))
        
        # Creating plot
        rV$out.plot <- ggplot(data = rV$plot.df,
                              aes(x = as.Date(date),
                                  y = rmean,
                                  col = pattern.label)) +
            geom_line(size = 1) +
            scale_x_date(limits = as.Date(rV$dateRange), date_breaks = "2 weeks",
                         date_labels = "%m-%d") +
            scale_y_continuous(labels = comma) +
            labs(x = "", y = "Hits in 10 000 tokens", col = "Search pattern",
                 subtitle = paste("Rolling mean with window size of", rV$rmean.window, "day(s)"),
                 caption = "cOWIDplus Viewer, IDS Mannheim") +
            theme_minimal() + theme(plot.subtitle = element_text(size = 8),
                                    plot.caption = element_text(size = 8, color = "grey70"),
                                    axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
        
        # Creating DataTable for output
        if (input$inOrAll == "in") {
            DTout <- lapply(result.list, FUN = function (x) x[["hit.pat.inst"]])
            DTout <- as.data.frame(rbindlist(DTout))
            names(DTout) <- c("Wordform", "Absolute_Frequency")
            rV$DTout <- DTout[order(DTout$Absolute_Frequency, decreasing = T),]
        }
    })
    
    observeEvent(eventExpr = input$`button-search`, ignoreNULL = F, {
        original.pattern.big <- input$bigramSearch
        
        clean.pattern.big <- tolower(gsub("^", "", original.pattern.big, fixed = T))
        clean.pattern.big <- gsub("$", "", clean.pattern.big, fixed = T)
        clean.pattern.big <- gsub("+", "", clean.pattern.big, fixed = T)
        clean.pattern.big <- gsub(".", "", clean.pattern.big, fixed = T)
        clean.pattern.big <- gsub("*", "", clean.pattern.big, fixed = T)
        clean.pattern.big <- gsub("[", "", clean.pattern.big, fixed = T)
        clean.pattern.big <- gsub("]", "", clean.pattern.big, fixed = T)
        clean.pattern.big <- gsub("|", "", clean.pattern.big, fixed = T)
        clean.pattern.big <- gsub("(", "", clean.pattern.big, fixed = T)
        clean.pattern.big <- gsub(")", "", clean.pattern.big, fixed = T)
        clean.pattern.big <- gsub(";", "", clean.pattern.big, fixed = T)
        
        # Replace multiple commas with one and clean leading or ending commas
        clean.pattern.big <- gsub(",+", ",", clean.pattern.big)
        clean.pattern.big <- gsub("^,", "", clean.pattern.big)
        clean.pattern.big <- gsub(",$", "", clean.pattern.big)
        
        if (clean.pattern.big == "") { 
            clean.pattern.big <- "corona"
            showNotification("Search pattern not valid. Set to 'corona' automatically.",
                             duration = 10, closeButton = T, type = "error")
            updateTextInput(session, "bigramSearch", value = "corona")
        }
        
        cat(clean.pattern.big, "\n")
        
        if (input$front_end_both == "vorne") clean.pattern.big <- paste0("^", clean.pattern.big, " ")
        if (input$front_end_both == "hinten") clean.pattern.big <- paste0(" ", clean.pattern.big, "$")
        
        cat(clean.pattern.big, "\n")
        
        bigram.hits <- unigram.df[stri_detect_regex(unigram.df$entry, clean.pattern.big) &
                                      unigram.df$bigram,]
        bigram.hits.agg <- bigram.hits[, list(Absolute_Frequency = sum(freq)), by = entry]
        rV$DTout_bigs <- bigram.hits.agg[order(Absolute_Frequency, decreasing = T),]
    })
    
    output$resultPlot <- renderPlot(res = 120, {
        rV$out.plot })
    # Creating output DTs
    output$hit.pat.insts <- DT::renderDataTable(rV$DTout, rownames = F)
    output$bigram.results <- DT::renderDataTable(rV$DTout_bigs, rownames = F,
                                                 options = list(pageLength = 25))
    # Creating download object
    output$download <- downloadHandler(
        filename = function () {
            paste0("cOWIDplus-Daten-", Sys.Date(), ".csv")
        },
        content = function (con) {
            outputFile <- rV$plot.df
            write.table(outputFile, file = con, sep = "\t", row.names = F)
        }
    )
    
    output$downloadPNG <- downloadHandler(
        filename = function () {
            paste0("cOWIDplus-Plot-", Sys.Date(), ".png")
        },
        content = function (con) {
            ggsave(filename = con, plot = rV$out.plot, width = 10, height = 5, dpi = 250, device = "png",
                   scale = 3/4)
        }
    )
    
    output$downloadPDF <- downloadHandler(
        filename = function () {
            paste0("cOWIDplus-Plot-", Sys.Date(), ".pdf")
        },
        content = function (con) {
            ggsave(filename = con, plot = rV$out.plot, width = 10, height = 5, device = "pdf",
                   scale = 3/4)
        }
    )
    
})
