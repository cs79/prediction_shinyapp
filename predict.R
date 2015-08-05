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
