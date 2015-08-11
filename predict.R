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


# get candidate keys
get_candidates <- function(input_text) {
    input_text = strsplit(input_text, " ")[[1]]
    input_len = length(input_text)
    c1 = paste(input_text[(input_len - 2)], input_text[(input_len -1)], input_text[input_len])
    c2 = paste(input_text[(input_len - 1)], input_text[input_len])
    c3 = input_text[input_len]
    cands = c(c1, c2, c3)

    return(cands)
}

# use get_candidates to predict (see commented stuff below related to imported dict DF from Python)
predict_v2 <- function(input_text) {
    cands = get_candidates(input_text)

    for(i in cands) {
        cat(i)
    }
}

# code to get trailing word where leading word = candidate:
# candidate = 'something'
# test["trailing"][test["leading"] == candidate]
# this works returning multiple values

