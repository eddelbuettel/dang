## Bootstrapped by two email by Kurt Hornik to an r-package-devel thread I started in Aug 2024

##' @title Get ORCID IDs known at CRAN
##'
##' @description This function returns a three-column data frame with first name, family name and
##' orcid for all maintainers with (optional) ORCID fields at CRAN.
##'
##' @return A data frame with three columns given (ie first name), family (name) and oid.
##' @author Kurt Hornik (plus small tweaks by Dirk Eddelbuettel)
##' @seealso A new function \code{tools::CRAN_authors_db()} in r-devel as of August 2024.
cranORCIDs <- function() {

    ## Kurt Hornik, 2024-08-20
    ## plus farmed out functions, first one on in r-devel's tools right now
    .ORCID_iD_canonicalize <- function (x) {
        .ORCID_iD_variants_regexp <- yoink("tools", ".ORCID_iD_variants_regexp")
        sub(.ORCID_iD_variants_regexp, "\\3", x)
    }
    ## used in lapply below
    .get_person_object <- function(a) {
        if(!is.na(a)) {
            .read_authors_at_R_field <- yoink("utils", ".read_authors_at_R_field")
            a <- tryCatch(.read_authors_at_R_field(a), error = identity)
            if (inherits(a, "person"))
                return(a)
        }
        NULL
    }
    ## used in lapply below
    .get_detail_with_orcid <- function(e) {
        if(is.null(o <- e$comment["ORCID"]) || is.na(o))
            return(NULL)
        cbind(given = paste(e$given, collapse = " "),
              family = paste(e$family, collapse = " "),
              oid = unname(.ORCID_iD_canonicalize(o)))
    }
    ## A classic, and you-know-who-wrote-it
    yoink <- function(package, symbol) do.call(":::", list(package, symbol))

    x <- tools::CRAN_package_db()
    a <- lapply(x[["Authors@R"]], .get_person_object)
    a <- do.call(c, a)
    a <- lapply(a, .get_detail_with_orcid)
    a <- unique(as.data.frame(do.call(rbind, a)))

    ## add a sort
    a <- sort_by(a, ~ a$family + a$given)
    ## and prune empty 'family'
    a <- a[a$family != "", ]

    invisible(a)
}
