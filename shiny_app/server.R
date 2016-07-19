library(shiny)
library(UsingR)
library(tm)
library(stringr)

predict_word <- function(word, two_words, database_2gram, database_3gram, NUM_OUT = 3) {

    library(stringr)

    if(two_words != '') {
        cand_3gram = predict_from_two(two_words, database_3gram);
    }
    
    rexp_exact = sprintf('^%s ', word);
    rexp_likely = sprintf('^%s', word);

    candidates = database_2gram[which(grepl(rexp_exact, database_2gram$ngrams)),];
    res_len = nrow(candidates);

    if (res_len == 0) {

        # could not find exact match - lets search for likely candidates
        candidates = database_2gram[which(grepl(rexp_likely, database_2gram$ngrams)),];
        res_len = nrow(candidates);

        return(form_output(candidates, cand_3gram, NUM_OUT, FALSE));

    } else {
        return(form_output(candidates, cand_3gram, NUM_OUT, TRUE));
    }

}

predict_from_two <- function(words, database_3gram) {

    library(stringr);

    rexp_exact = sprintf('^%s ', words);
    rexp_likely = sprintf('^%s', words);

    candidates = database_3gram[which(grepl(rexp_exact, database_3gram$ngrams)),];
    res_len = nrow(candidates);

    if (res_len == 0) {

        # could not find exact match - lets search for likely candidates
        candidates = database_3gram[which(grepl(rexp_likely, database_3gram$ngrams)),];
        res_len = nrow(candidates);
        
        if(res_len > 0) {
            return( form_output_3gram(candidates) );
        } else {
            return('');
        }

    } else {
        return( form_output_3gram(candidates) );
    }

}

form_output <- function( cands, cand_3gram, NUM_OUT, exact) {

    res_len = nrow(cands);

    if (res_len == 0) {
        return( list("", vector()) );
    } else {
    
        # if cand3_gram is present - add it to the result with the biggest weight possible!!!
        if (cand_3gram != '') {
            max_wght = max(cands$freq);
            max_wght = max_wght + 100;
            tmp = cands[1,];
            tmp$ngrams = cand_3gram;
            tmp$freq = max_wght;
            
            cands = rbind(cands, tmp);
        }

        num_out = min(NUM_OUT, res_len);

        cands$freq <- as.numeric(cands$freq);
        cands = cbind(cands, strlen = 1/nchar(cands$ngrams));

        cands$ngrams = str_trim(cands$ngrams);

        cands = cands[ order(cands$freq, cands$strlen, decreasing = c(TRUE)) ,];
        cands = cands[1:num_out,];

        tmp = cands$ngrams[1:num_out];

        if (exact) {
            if (num_out == 1) {
                return( list(tmp[1], "" ) );
            } else {
                return( list(tmp[1], tmp[2:num_out]) );
            }

        } else {
            if (num_out > 0) {
                return( list("", tmp[1:num_out]) );
            } else {
                return( list("", "") );
            }

        }

    }
}

form_output_3gram <- function( cands ) {
    cands$freq <- as.numeric(cands$freq);
    cands = cbind(cands, strlen = 1/nchar(cands$ngrams));

    cands$ngrams = str_trim(cands$ngrams);

    cands = cands[ order(cands$freq, cands$strlen, decreasing = c(TRUE)) ,];
    
    return(cands$ngrams[1]);
}

preprocessInputText <- function (input) {

    # convert to lower-case
    input = tolower(input);

    if (input == '') {
        return(list('', ''));
    }

    # removing stop words
    input = removeWords(input, stopwords('english'));

    # removing all numbers and punctuation characters
    punct <- '[]\\?!\"\'#$%&(){}+*/:;,._`|~\\[<=>@\\^-]1234567890”“’«»…';
    input <- gsub(punct, "", input);

    # removing spaces
    input = gsub('[ ]{2,}', " ", input);
    input = str_trim(input);

    wrds = strsplit(input, ' ');
    num_wrds = length(wrds[[1]]);
    last_word = wrds[[1]][num_wrds];
    
    if (num_wrds >= 2) {
      last_two = sprintf('%s %s', wrds[[1]][num_wrds - 1], wrds[[1]][num_wrds]);
    } else {
      last_two = '';
    }

    return( list(last_word, last_two) );
}

shinyServer(
    function(input, output) {
    
        library(ngram);
        
        # constants used further
        STEP = 0.01;

        DATABASE_2GRAM = read.table('./data/merged_2gram.csv', sep = ';', stringsAsFactors = FALSE);
        DATABASE_3GRAM = read.table('./data/merged_3gram.csv', sep = ';', stringsAsFactors = FALSE);

        inpData <- reactive({
            input$Submit
            input_text 	<- input$input_text;
            return(input_text);
        })

        procInputText <- reactive({
          inp = inpData();
          proc_res = preprocessInputText(inp);
          return( list(inp, proc_res[[1]], proc_res[[2]]) );
        })

        output$main_prediction <- renderPrint({
        #   util_rates = calcUtilRates();
        #   util_rates[1]
            inp_txts = procInputText();

            if(inp_txts[[1]] == '' || inp_txts[[2]] == '') {
                return(cat(""))
            } else {
                out_str = inp_txts[[1]];

                res = predict_word(inp_txts[[2]], inp_txts[[3]], DATABASE_2GRAM, DATABASE_3GRAM, 4);

                if (res[[1]] == "") {
                    return("");
                } else {
                    out_wrd = res[[1]];
                    
                    if( wordcount(str_trim(out_wrd)) == 2) {
                        out_wrd = gsub(inp_txts[[2]], '', out_wrd);
                    } else {
                        out_wrd = gsub(inp_txts[[3]], '', out_wrd);
                    }
                    
                    final_res = sprintf("%s %s", out_str, out_wrd)
                    return(cat(final_res));
                    #return(res[[1]]);
                    #return(inp_txts[[2]]);
                    #return( wordcount(str_trim(out_wrd)) )
                }

            }
        })

        output$likely_candidates <- renderPrint({
            inp_txts = procInputText();
            if(inp_txts[[1]] == '' || inp_txts[[2]] == '') {
                return(cat(""))
            } else {
                res = predict_word(inp_txts[[2]], inp_txts[[3]], DATABASE_2GRAM, DATABASE_3GRAM, 4);
                

                if (res[[2]] == "") {
                    return("");
                } else {
                    out_str = inp_txts[[1]];
                    out_wrd = res[[2]];
                    out_wrd = gsub(inp_txts[[2]], '', out_wrd);
                    res = sprintf("%s %s\n", out_str, out_wrd);
                }

                return( cat(res) );
            }

        })

    	output$DEBUG <- renderPrint({
        #   util_rates = calcUtilRates();
        #   util_rates[2]
            inp_txts = procInputText();
            # res = sprintf('%s + %s\n', inp_txt[[1]], MODEL$le_value);
            if(inp_txts[[1]] == '' || inp_txts[[2]] == '') {
                return(cat("Invalid input"))
            } else {
                res = sprintf("DEBUG : INPUT WAS \'%s\' and \'%s\'", inp_txts[[2]], inp_txts[[3]])
                return( cat(res) );
            }

        })

	}
)
