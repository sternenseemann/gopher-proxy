# gopher-proxy

gopher-proxy is a tiny [wai](https://hackage.haskell.org/package/wai) application running on top of [warp](https://hackage.haskell.org/package/warp). It acts as a gopher-over-http proxy for a specific server, e. g. to http-ify a gopher space.

## Usage

Example usage:

    gopher-proxy --host foo.org --http-port 8080

In this particular example, gopher-proxy would proxy the `foo.org` gopher server and bind its http service on `127.0.0.1:8080` (to be proxied to by another web server like `nginx`).

These are all optional flags which allow to change default behavior:

* `--port`: The port of the gopher server, defaults to `70`
* `--css-url`: Use some specific css file instead of the default one.
* `--css-url`: The http path of the css file, defaults to `/gopher-proxy.css` (should be changed, if your gopher server has a file with the same name
* `--base-url`: The path of the directory which will appear as root directory of gopher-proxy to the user, defaults to `/`. Should be changed if you configured your proxying web server to expose gopher-proxy as, say `/gopher-space/`.
* `--listen-public`: If this flag is set, gopher-proxy will accept connections on its public IP address(es).
* `--default-mime-type`: Mime type to use if spacecookie can't guess it, defaults to "application/octet-stream"
* `--timeout`: connection timeout in milliseconds, defaults to 10 seconds.
* `--server-name`: The server name of the server to proxy, defaults to the host name. This value is used to detect wether a menu item is pointing to another gopher server than the proxied one. This is particularly useful, if you use e. g. `127.0.0.1` instead of the public host name.

## Things to keep in mind

* Your gopher server must send UTF-8-encoded gopher responses
* gopher-proxy might misinterpret certain content, because of the context-sensitive nature of gopher
* gopher-proxy does not support Gopher+

## Roadmap

- [ ] Add Logging
