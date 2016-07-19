test_the_model <- function (model_dir) {

    phrases = c(
    'the guy in front of me just bought a pount of bacon, a bouquet, and a case of',
    'you\'re the reason why I smile everyday. Can you follow me please? it would mean the',
    'Hey sunshine, cna you follow me and make me the',
    'Very early observations on the Bills game : offense still struggling but the',
    'Go on a romantic date at the',
    'well i\'m pretty sure my granny has some old bagpipes in ther garage. I\'ll dust them off and be on my',
    'Ohhhhh #PointBreak on tomorrow. Love that file and haven\'t seen it in quite some',
    'After the ice buchet challenge Louls will push his long wet hair out of his eyes with his little',
    'Be grateful for the good times and keep the faith during the',
    'If this isn\'t the cutest thing you\'ve ever seen, then you must be',
    'When you breathe, I want to be the air for you. I\'ll be there for you, I\'d live and I\'d',
    'Guy at my table\'s wife got up to go to the bathroom and I asked about dessert and he started telling me about his',
    'I\'d give anything to see arctic monkeys this',
    'Talking to your mom has the same effect as a hug and helps reduce you',
    'When you were in Holland you were like 1 inch away from me but you hadn\'t time to take a',
    'I\'d just like all of these questions answered, a presentation of evidence, and a jury to settle the',
    'I can\'t deal with unsymetrical things. I can\'t even hold an uneven number of bags of groceries in each',
    'Every inch of you is perfect from the bottom to the',
    'I\'m thankful my childhood was filled with imagination and bruises from',
    'It\'s interesting how all the same people play in Adam Sandler\'s');

    answers = c('beer', 'world', 'happiest', 'defence', 'beach', 'way', 'time', 'fingers', 'bad', 'insane',
    'die', 'marital', 'weekend', 'stress', 'picture', 'matter', 'hand', 'top', 'outside', 'movies');

    if (length(phrases) != length(answers)) {
        print('ERROR! WRONG SIZES IN TESTING DATASET!');
        return(-1);
    }

    tst_len = length(phrases);

    TST_SET = data.frame(input = phrases, answer = answers);

    gram2_fname = sprintf("%s/merged_2gram.csv", model_dir);
    gram3_fname = sprintf("%s/merged_3gram.csv", model_dir);

    model_2gram = read.table(gram2_fname, sep = ';', stringsAsFactors = FALSE);
    model_3gram = read.table(gram3_fname, sep = ';', stringsAsFactors = FALSE);

    st_time = Sys.time();
    correct = 0;
    for (k in 1:tst_len) {

        correct_ans = TST_SET$answer[k];
        input = TST_SET$input[k];

        model_ans = predict_word(input, model_2gram, model_3gram);

        # DEBUG
        print( sprintf('    ITERATION : %d', k) );
        print( sprintf('INPUT : %s', input) )
        print( sprintf('ANSWER : %s', correct_ans) )
        print( model_ans );

        if ( sum(grepl(correct_ans, model_ans)) > 0) {
            correct = correct + 1;
        }

    }
    end_time = Sys.time();

    # calculating results of testing
    err_rate = (tst_len - correct) / tst_len;
    total_time = end_time - st_time;

    print( sprintf("DONE TESTING.  Error rate : %f %%    Calculation time : %s sec ", err_rate*100, total_time/tst_len) );

    return (list(err_rate, total_time));
}

predict_word <- function (in_phrase, model_2gram, model_3gram) {

    inputs = form_inputs(in_phrase);

    if(inputs[[1]] == '' && inputs[[2]] == '') {
        print('ERROR IN predict_word() : INVALID INPUT !!!');
        return('');
    }

    return( predict_helper(inputs[[1]], inputs[[2]], model_2gram, model_3gram) );
}

form_inputs <- function (input) {

    library(tm);
    library(stringr);

    # convert to lower-case
    input = tolower(input);

    if (input == '') {
        return(list('', ''));
    }

    # our regexp for punctuation chars
    punct <- '[]\\?!\"\'#$%&(){}+*/:;,._`|~\\[<=>@\\^-]1234567890”“’«»…';

    # removing all numbers and punctuation characters
    input <- gsub(punct, "", input);

    # removing stop words
    stp_wrds = stopwords('english');
    stp_wrds = gsub(punct, "", stp_wrds);
    input = removeWords(input, stp_wrds);

    # removing extra spaces
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

predict_helper <- function(word, two_words, database_2gram, database_3gram, NUM_OUT = 3) {

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
        # return( list("", vector()) );
        return('');
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

        return(tmp);

        # if (exact) {
        #     if (num_out == 1) {
        #         return( list(tmp[1], "" ) );
        #     } else {
        #         return( list(tmp[1], tmp[2:num_out]) );
        #     }
        #
        # } else {
        #     if (num_out > 0) {
        #         return( list("", tmp[1:num_out]) );
        #     } else {
        #         return( list("", "") );
        #     }
        #
        # }

    }
}

form_output_3gram <- function( cands ) {
    cands$freq <- as.numeric(cands$freq);
    cands = cbind(cands, strlen = 1/nchar(cands$ngrams));

    cands$ngrams = str_trim(cands$ngrams);

    cands = cands[ order(cands$freq, cands$strlen, decreasing = c(TRUE)) ,];

    return(cands$ngrams[1]);
}
