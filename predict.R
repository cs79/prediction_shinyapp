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


# use get_candidates to predict (see commented stuff below related to imported dict DF from Python)
# main thing to try to fix with this is add in lambdas and make the lookup work with regexes instead of exact match lookups
predict_v2 <- function(input_text, dict) {
    cands = get_candidates(input_text)
    # cands = add_lambdas(cands)
    best_answers = data.frame("prediction" = character(0), "score" = numeric(0), stringsAsFactors = FALSE)
    for(candidate in cands) {
        # need to search by regex here rather than exact match
        matches = data.frame("val" = dict$trailing[dict$leading == candidate], "score" = dict$frequency[dict$leading == candidate] ^ dict$n[dict$leading == candidate], stringsAsFactors = FALSE)
        top_pred = matches$val[matches$score == max(matches$score)]
        top_pred_score = matches$score[matches$score == max(matches$score)]
        best_match = data.frame("prediction" = top_pred, "score" = top_pred_score, stringsAsFactors = FALSE)
        best_answers = rbind(best_answers, best_match)
    }
    # omit the NAs since those aren't real predictions, just a one-word match
    best_answers = best_answers[complete.cases(best_answers), ]
    prediction = best_answers$prediction[best_answers$score == max(best_answers$score)]
    if(length(prediction) > 0) {
        return(best_answers$prediction[best_answers$score == max(best_answers$score)])
    }
    else {
        return("this is a test prediction for when no matches are found")
    }
}


# super simple predict function, shifting most of the logic to the get_top_matches function
predict_v3 <- function(input_text, dict) {
    prediction = get_top_matches(input_text, dict)[1, ]
    if(length(prediction) > 0) {
        return(prediction$trailing)
    }
    else {
        return("this is a test prediction for when no matches are found")
    }
}



# function to return all (fuzzy) matches against a set of keys derived from input text
# NEED TO ADD LOGIC TO ENFORCE UNIQUENESS OF PREDICTIONS WHEN STEPPING DOWN IN LENGTH
get_top_matches <- function(input_text, dict) {
    matches = data.frame()
    cands = get_candidates(input_text)
    # removing lambdas for now
    #cands = add_lambdas(cands)
    lengths = numeric(0)
    for(cand in cands) {
        lengths = c(lengths, length(strsplit(cand, " ")[[1]]))
    }
    cand_df = data.frame(cbind(lengths, cands))
    best_len = 3


    while(length(unique(matches$trailing)) < 4) {
        cands_to_try = as.character(cand_df[cand_df$lengths == best_len,]$cands)
        for(candidate in cands_to_try) {
            candidate = paste0("^", candidate, "$")
            match_subset = subset(dict, grepl(candidate, dict$leading))
            matches = rbind(matches, match_subset)
        }
        best_len = best_len - 1
    }
    return(matches[order(-matches$n, -matches$frequency), ])
}


# original "get all matches" function:
get_all_matches <- function(input_text, dict) {
    matches = data.frame("prediction" = character(0), "score" = numeric(0), stringsAsFactors = FALSE)
    cands = get_candidates(input_text)
    # removing lambdas to test performance improvement
    #cands = add_lambdas(cands)
    for(candidate in cands) {
        candidate = paste0("^", candidate, "$")
        match_subset = subset(dict, grepl(candidate, dict$leading))
        matches = rbind(matches, match_subset)
    }
    return(matches)
}


# function to plot predictions aside from the top one
# THIS NEEDS MORE WORK -- try to utilize fuzzy matching, and collapse/combine scores for the same prediction if it occurs more than once
plot_preds <- function(input_text, dict) {
        cands = get_candidates(input_text)
        best_answers = data.frame("prediction" = character(0), "score" = numeric(0), stringsAsFactors = FALSE)
        for(candidate in cands) {
                matches = data.frame("val" = dict$trailing[dict$leading == candidate], "score" = dict$frequency[dict$leading == candidate] ^ dict$n[dict$leading == candidate], stringsAsFactors = FALSE)
                top_pred = matches$val[matches$score == max(matches$score)]
                top_pred_score = matches$score[matches$score == max(matches$score)]
                best_match = data.frame("prediction" = top_pred, "score" = top_pred_score, stringsAsFactors = FALSE)
                best_answers = rbind(best_answers, best_match)
        }
        # omit the NAs since those aren't real predictions, just a one-word match
        best_answers = best_answers[complete.cases(best_answers), ]

        best_answers = best_answers[order(best_answers$score, decreasing = TRUE), ]
        return(barplot(as.matrix(log(best_answers$score)), beside=TRUE, horiz = TRUE, legend.text = best_answers$prediction))
}


# better function to plot predictions using get_top_matches
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
