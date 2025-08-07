##' Mutes Twitter accounts using for than 'ncrit' hashtags among 'N' tweets in
##' search of along with of 'term'.
##'
##' This is a modified version of the code in a wonderful tweet by Colin
##' Gillespie (csgillespie) on 2020-August-26. It requires the \code{rtweet}
##' and \code{data.table} packages.
##' @title Mute Twitter Users with Excessive Hashtag Use
##' @param term A character variable to search for, defaults to \sQuote{#rstats}
##' @param N An number of tweets to fetch, defaults to 1000
##' @param ncrit A number of hashtags after which use is deemed excessive, defaults to 10
##' @return \code{NULL}, invisibly, but the function is invoked for the side effect
##' of calling \code{post_mute}.
##' @examples
##' \dontrun{
##' ## mute users with more than 10 hashtags among
##' ## the 1000 most recent #rstats tweets
##' muteTweeters("#rstats", 1000, 10)
##' }
##' @author Dirk Eddelbuettel
muteTweeters <- function(term="#rstats", N=1000, ncrit=10) {
    if (!requireNamespace("rtweet", quietly=TRUE)) stop("Package 'rtweet' needed.", call.=FALSE)
    if (!requireNamespace("data.table", quietly=TRUE)) stop("Package 'data.table' needed.", call.=FALSE)
    countChars <- function(txt, s) sum( strsplit(txt, "")[[1]] == s )
    DT <- data.table::data.table(rtweet::search_tweets(q=term, n=N, include_rts=FALSE, verbose=FALSE))
    DT[           , list(text,user_id)][                           # select
                  , nm := countChars(text, "#"),       by=text][   # compute 'n matching chars'
        nm > ncrit, list(id = unique(user_id))][                   # filter for at least 5, unique-ify
                  , list(res = rtweet::post_mute(id)), by=id]      # apply each id to post_mute()
    invisible(NULL)
}

utils::globalVariables(c("text", "user_id", "nm", "id"))
