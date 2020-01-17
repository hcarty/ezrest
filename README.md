# Easy REST requests using cohttp

See `ezrest.mli` for detailed usage information.

## Example usage
Let's take a look at what it's like to use Ezrest from a REPL.
```ocaml require-package=ezrest env=demo
# #require "ezrest"
```

First, a simple `HEAD` request.
```ocaml env=demo
# let site = Uri.of_string "http://jsonplaceholder.typicode.com/"
val site : Uri.t = <abstr>
# Ezrest.head site
- : Cohttp.Response.t Ezrest.result =
Ok
 {Cohttp.Response.encoding = Cohttp__.Transfer.Unknown; headers = <abstr>;
  version = `HTTP_1_1; status = `OK; flush = false}
```

Now we can `GET` some content, failing if the site tries to redirect us.
```ocaml env=demo
# Ezrest.get ~follow:0 site
- : string Ezrest.result =
Ok
 "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\" />\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1, shrink-to-fit=no\" />\n<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/modern-normalize/0.5.0/modern-normalize.min.css\" />\n<link rel=\"stylesheet\""... (* string length 7323; truncated *)
```

We can `PUT` and `POST` too!
```ocaml env=demo
# let put_site = Uri.with_path site "/posts/1"
val put_site : Uri.t = <abstr>
# Ezrest.put put_site
- : string Ezrest.result = Ok "{\n  \"id\": 1\n}"
# let post_site = Uri.with_path site "/posts"
val post_site : Uri.t = <abstr>
# Ezrest.post post_site
- : string Ezrest.result = Ok "{\n  \"id\": 101\n}"
```

And `DELETE` if that's what's necessary.
```ocaml env=demo
# Ezrest.delete put_site
- : string Ezrest.result = Ok "{}"
```
