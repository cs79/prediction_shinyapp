predict <- function(input_text) {

    # for now, do nothing with the actual input and return a random set of choices:

    fakelist = c("this", "is", "sample", "predicted", "output", "that's", "fake", "for", "testing", "purposes")

    shuffled = sample(fakelist, length(input_text))

    if (input_text == "") {
        return("")
    }

    else {
        return(shuffled)
    }
}

# clean input text
clean_string <- function(input_text) {
    cleaned = tolower(input_text)
    cleaned = gsub("[[:punct:]]", "", cleaned)
    cleaned = gsub("[[:digit:]]", "", cleaned)
    cleaned = gsub(" +", " ", cleaned)

    return(cleaned)
}


# get candidate keys
get_candidates <- function(input_text) {
    input_text = clean_string(input_text)
    input_text = strsplit(input_text, " ")[[1]]
    input_len = length(input_text)
    c1 = paste(input_text[(input_len - 2)], input_text[(input_len -1)], input_text[input_len])
    c2 = paste(input_text[(input_len - 1)], input_text[input_len])
    c3 = input_text[input_len]
    cands = c(c1, c2, c3)

    return(cands)
}

# use get_candidates to predict (see commented stuff below related to imported dict DF from Python)
predict_v2 <- function(input_text, dict) {
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
    prediction = best_answers$prediction[best_answers$score == max(best_answers$score)]
    if(length(prediction) > 0) {
        return(best_answers$prediction[best_answers$score == max(best_answers$score)])
    }
    else {
        return("this is a test prediction for when no matches are found")
    }
}

# code to get trailing word where leading word = candidate:
# candidate = 'something'
# dict["trailing"][dict["leading"] == candidate]
# dict["frequency"][dict["leading"] == candidate]
# test = data.frame("val" = dict$trailing[dict$leading == candidate], "score" = dict$frequency[dict$leading == candidate] * dict$n[dict$leading == candidate], stringsAsFactors = FALSE)
# best = test$val[test$score == max(test$score)]
# this works returning multiple values

# BEST PREDICTED STRING: "hi there how are you going to be a good day for a while to get a chance to win the game"
