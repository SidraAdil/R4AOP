# Shorten the paths of the recorded API responses.
#
# httptest2 stores each response in a folder structure that mirrors the request
# URL. The API host name is long, which pushed the fixture paths past the 100
# byte limit that a package tarball guarantees, and R CMD check reported it.
#
# Replacing the host and version prefix with "aop" keeps the paths short. The
# same substitution is applied to the request, so that playback still finds the
# file that recording wrote.

api_prefix <- "aopwiki.rdf.bigcat-bioinformatics.org/api/v1"

httptest2::set_redactor(function(response) {
  httptest2::gsub_response(response, api_prefix, "aop", fixed = TRUE)
})


