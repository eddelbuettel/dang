\name{NEWS}
\title{News for Package \pkg{dang}}
\newcommand{\ghpr}{\href{https://github.com/eddelbuettel/dang/pull/#1}{##1}}
\newcommand{\ghit}{\href{https://github.com/eddelbuettel/dang/issues/#1}{##1}}

\section{Changes in version 0.0.16 (2023-12-02)}{
  \itemize{
    \item Added new function \code{str.language()} based on post by Bill Dunlap
    \item Added new argument \code{sleep} in \code{intradayMarketMonitor}
    \item Switched to \code{dev.hold()} and \code{dev.flush()} in
    \code{intradayMarketMonitor} with thanks to Paul Murrell
    \item Updated continued integration setup, twice, and package badges
    \item Added new function \code{shadowedPackages}
    \item Added new function \code{limitDataTableCores}
    \item Updated two \code{error()} calls to updated \pkg{tidyCpp} signature
    to not tickle \code{-Wformat} warnings under R-devel
    \item Updated two URL to please link checks in R-devel
    \item Switch two tests for variable of variable to \code{is.*} and
    \code{inherits()}, respectively
  }
}

\section{Changes in version 0.0.15 (2021-10-26)}{
  \itemize{
    \item Corrected scope of 'extern "C"' declaration to the actually C-callable
    function (noticed by \code{clang++}, change requested by CRAN)
  }
}

\section{Changes in version 0.0.14 (2021-10-17)}{
  \itemize{
    \item Updated continuous integration to run on Linux only.
    \item Edited \code{checkNonAscii.cpp} for readability.
    \item More robust title display in \code{intradayMarketMonitor.R}.
    \item New C++-based function to read and convert encoding via the
    R-supplied \code{iconv} library, noted a potential variability.
    \item New function \code{wday} returning day of the week as
    \code{integer}.
    \item The signature to \code{as.data.table} was standardized.
    \item A new function \code{rollMinMax} was added illustrating use of
    the \code{NumVec} class from \pkg{tidyCpp}.
    \item The \code{configure} script can record the last commit date and
    sha1 to automate timestamping builds, but not activated in this release.
    \item \code{checkCRANStatus()} now works correctly for single-package
    lookups (Jordan Mark Barbone in \ghpr{4}).
  }
}

\section{Changes in version 0.0.13 (2021-02-17)}{
  \itemize{
    \item New function \code{intradayMarketMonitor} based on an earlier
    gist-posted snippet by Josh Ulrich.
    \item The CI setup was generalized as a test for
    \href{https://eddelbuettel.github.io/r-ci/}{r-ci} and is used
    essentially unchanged with three different providers.
  }
}

\section{Changes in version 0.0.12 (2020-10-14)}{
  \itemize{
    \item New functions \code{muteTweets} and \code{checkPackageAsciiCode}.
    \item Updated CI setup.
  }
}

\section{Changes in version 0.0.11 (2019-10-24)}{
  \itemize{
    \item New functions \code{getGitRoot}, \code{inGit} and \code{isConnected}.
    \item Improved function \code{as.data.table.xts}.
  }
}

\section{Changes in version 0.0.10 (2019-02-10)}{
  \itemize{
    \item Initial CRAN release.  See \code{ChangeLog} for earlier changes.
  }
}
