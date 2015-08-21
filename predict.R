# function to clean input text
clean_string <- function(input_text) {
    cleaned = tolower(input_text)
    cleaned = gsub("[[:punct:]]", "", cleaned)
    cleaned = gsub("[[:digit:]]", "", cleaned)
    cleaned = gsub(" +", " ", cleaned)

    return(cleaned)
}


# function to get candidate keys
get_candidates <- function(input_text) {
    input_text = clean_string(input_text)
    input_text = strsplit(input_text, " ")[[1]]
    input_len = length(input_text)
    # logic to return the correct number of candidates based on input length
    if(input_len < 3) {
            if(input_len == 1) {
                    cands = input_text
                    return(cands)
            }
            else {
                    c1 = paste(input_text[(input_len - 1)], input_text[input_len])
                    c2 = input_text[input_len]
                    cands = c(c1, c2)
                    return(cands)
            }
    }
    else {
            c1 = paste(input_text[(input_len - 2)], input_text[(input_len -1)], input_text[input_len])
            c2 = paste(input_text[(input_len - 1)], input_text[input_len])
            c3 = input_text[input_len]
            cands = c(c1, c2, c3)
            return(cands)
    }
}


# function to add lambdas to a set of candidate keys for fuzzy matching against dict
add_lambdas <- function(raw_candidates) {
        with_lambdas = raw_candidates
        anon = "[a-z]+"
        for(cand in raw_candidates) {
                len = length(strsplit(cand, " ")[[1]])
                if(len > 1) {
                        split_cand = strsplit(cand, " ")[[1]]
                        # sub "anon" in for each word
                        for(i in 1:len) {
                                split_copy = split_cand
                                split_copy[i] = anon
                                lambda = paste(split_copy, collapse = " ")
                                with_lambdas = c(with_lambdas, lambda)
                        }
                }
        }

        return(with_lambdas)
}


# super simple predict function, shifting most of the logic to the get_top_matches function
predict_v3 <- function(input_text, dict) {
    prediction = get_top_matches(input_text, dict)[1, ]
    if(length(prediction) > 0) {
        return(prediction$trailing)
    }
    else {
        # with lambdas and new logic for returning top preds on empty match, this handles most cases
        prediction = get_top_matches(input_text, dict, use_lambdas = TRUE)[1, ]
        if(length(prediction) > 0) {
            return(prediction$trailing)
        }
        else {
            return("something went horribly wrong")
        }
        # --------------
    }
}



# function to return all (fuzzy) matches against a set of keys derived from input text
# NEED TO ADD LOGIC TO ENFORCE UNIQUENESS OF PREDICTIONS WHEN STEPPING DOWN IN LENGTH
get_top_matches <- function(input_text, dict, use_lambdas = FALSE) {
    matches = data.frame()
    cands = get_candidates(input_text)
    # add lambdas if requested
    if(use_lambdas == TRUE) {
        cands = add_lambdas(cands)
    }
    # create a dataframe of candidates and their lengths for back-off matching
    lengths = numeric(0)
    for(cand in cands) {
        lengths = c(lengths, length(strsplit(cand, " ")[[1]]))
    }
    cand_df = data.frame(cbind(lengths, cands))
    best_len = 3

    # WHILE IS A PROBLEM FOR INPUT THAT WILL NEVER MATCH
    while(length(unique(matches$trailing)) < 4 & best_len > 0) {
        cands_to_try = as.character(cand_df[cand_df$lengths == best_len,]$cands)
        for(candidate in cands_to_try) {
            candidate = paste0("^", candidate, "$")
            match_subset = subset(dict, grepl(candidate, dict$leading))
            matches = rbind(matches, match_subset)
        }
        best_len = best_len - 1
    }
    # add if / else statement here for both len(unique(pred)) == len(pred), and len(matches) == 0
    if(nrow(matches) == 0) {
        top_1grams = head(dict[order(-dict$frequency), ], 4)
        top_1grams$trailing = top_1grams$leading
        return(top_1grams)
    } else if(nrow(matches) < 4) {
        top_1grams = head(dict[order(-dict$frequency), ], 4)
        top_1grams$trailing = top_1grams$leading
        matches = rbind(matches, top_1grams)
        return(matches[order(-matches$n, -matches$frequency), ])
    } else {
        return(matches[order(-matches$n, -matches$frequency), ])
    }
}


# better function to plot predictions using get_top_matches
# NEEDS TO HANDLE CASE WHERE RETURNED DF HAS FEWER THAN 4 ENTRIES, NEEDS TO USE LAMBDAS IF NEEDED
plot_preds_v2 <- function(input_text, dict) {
    top_matches = get_top_matches(input_text, dict)[2:4, ]
    top_matches = top_matches[order(top_matches$frequency), ]
    best_answers = data.frame("prediction" = as.factor(top_matches$trailing), "score" = top_matches$frequency, stringsAsFactors = FALSE)

    return(barplot(as.matrix(best_answers$score), beside=TRUE, horiz = TRUE, legend.text = best_answers$prediction))
}



# TODO:
# force top candidates to be unique words (for plot)
# ^^^ this is currently a problem because it's getting predictions for only candidates generated from the same 3 word fragment -- need to figure out how to compile regexes to get alternative predictions with "anonymous" words inserted
# fix scoring mechanism
# fix original dataframe - remove "$NUMBER" sentinel; try using generators to build ngrams from a single string of text
# make plot better
# add some links / better filler text
# add a little more styling to the page

# code to get trailing word where leading word = candidate:
# candidate = 'something'
# dict["trailing"][dict["leading"] == candidate]
# dict["frequency"][dict["leading"] == candidate]
# test = data.frame("val" = dict$trailing[dict$leading == candidate], "score" = dict$frequency[dict$leading == candidate] * dict$n[dict$leading == candidate], stringsAsFactors = FALSE)
# best = test$val[test$score == max(test$score)]
# this works returning multiple values

# BEST PREDICTED STRING: "hi there how are you going to be a good day for a while to get a chance to win the game"
