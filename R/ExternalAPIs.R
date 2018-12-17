opPOSThelsinki <- function(url, body, encode) {
  POST(
    url,
    add_headers(api_key = Sys.getenv("HELSINKI_API_KEY")),
    body = body,
    encode = encode
  )
}
