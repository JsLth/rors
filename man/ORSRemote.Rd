% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remote.R
\name{ORSRemote}
\alias{ORSRemote}
\title{ORS remote instance}
\description{
Creates a new ORS instance object. This R6 class is typically
constructed by \code{\link{ors_instance}}.

This object represents a remote instance, i.e. an existing server like
the public API.
}
\section{Super class}{
\code{\link[rors:ORSInstance]{rors::ORSInstance}} -> \code{ORSRemote}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{url}}{URL to the ORS server}

\item{\code{token}}{Information about API tokens}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ORSRemote-new}{\code{ORSRemote$new()}}
\item \href{#method-ORSRemote-clone}{\code{ORSRemote$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="rors" data-topic="ORSInstance" data-id="get_status"><a href='../../rors/html/ORSInstance.html#method-ORSInstance-get_status'><code>rors::ORSInstance$get_status()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rors" data-topic="ORSInstance" data-id="get_url"><a href='../../rors/html/ORSInstance.html#method-ORSInstance-get_url'><code>rors::ORSInstance$get_url()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rors" data-topic="ORSInstance" data-id="is_mounted"><a href='../../rors/html/ORSInstance.html#method-ORSInstance-is_mounted'><code>rors::ORSInstance$is_mounted()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rors" data-topic="ORSInstance" data-id="is_ready"><a href='../../rors/html/ORSInstance.html#method-ORSInstance-is_ready'><code>rors::ORSInstance$is_ready()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ORSRemote-new"></a>}}
\if{latex}{\out{\hypertarget{method-ORSRemote-new}{}}}
\subsection{Method \code{new()}}{
Initialize the remote ORS instance.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ORSRemote$new(server, token = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{server}}{\code{[character]}

URL of a server that accepts OpenRouteService requests. This can be a URL
to a local or a remote server. The official public API can be accessed using
the shortcuts \code{"public"} or \code{"pub"}. Keep in mind that the public
API is rate-restricted and requests are automatically throttled to 40
requests per minute. Routing functions \emph{will} be slow for larger
datasets.}

\item{\code{token}}{\code{[logical]}

Whether \code{server} requires authorization over a token. ORS tokens
are stored in the \code{ORS_TOKEN} environment variable. Defaults to
\code{FALSE}. If \code{server} is the public API, \code{token} is set
to \code{TRUE}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ORSRemote-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ORSRemote-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ORSRemote$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
