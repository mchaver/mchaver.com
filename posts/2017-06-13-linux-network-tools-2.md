---
title: Linux Network Tools - cURL
---

## cURL (see URL)

`curl` is a command line tool that uses libcurl to make URL requests. libcurl 
is a portable library that works across many platforms. It supports cookies, 
FTP, HTTP, HTTPS, proxy tunneling, etc.

## Quick Reference

```
$ curl -i -X GET http://localhost:7000/users

$ curl -i -H "Content-Type: application/json" -d '{"username":"user","password":"password"}' -X POST http://localhost:7000/api/login

$ curl -i -d '{"username":"user","password":"password"}' -X POST http://localhost:7000/users

$ curl -i -d '{"username":"user","password":"password"}' -X PUT http://localhost:7000/users/1

$ curl -X DELETE http://localhost/users/1
```

## Flags

- `-#`, `--progress-bar`: display a simple progress bar.

- `-b`, `--cookie <file-or-pairs>`: `curl -b "X=1; Y=Yes" http://localhost:7000/users`.

- `-d`, `--data <data>`: request body.

- `-F`, `--form <name=content>`: form data `curl -F "user=james;type=text/foo" http://localhost:7000/users`.

- `-g`, `--globoff`: can include the `{}[]` characters in the URL.

- `-H`, `--header <header>`: headers included with the request.

- `-i`, `--include`: include HTTP headers in the output.

- `-I`, `--head`: fetch headers only

- `-L`, `--location`: if a header and 3XX response code is received, repeat the 
query to the new location.

- `-v`, `--verbose`: provide more information.

- `-X`, `--request`: DELETE, GET, HEAD, OPTIONS, POST, PUT. If not included will default to GET.

## Examples

### GET

We can make a simple GET request by typing the address and port after `curl`.
`curl` assumes the request is GET if no explicit request type is given.

```bash
$ curl localhost:7000
```

We can test it out by listening to port 7000 with `nc`, then make a connection 
with `curl` and type something in the command line window where `nc` is running.

```bash
$ nc -l 7000
GET / HTTP/1.1
Host: localhost:7000
User-Agent: curl/7.47.0
Accept: */*

Hello cURL!
```

```bash
$ curl localhost:7000
Hello cURL!
```

### POST

Login request with JSON.

```bash
$ curl -H "Content-Type:application/json"  -d '{"email":"user@home.com",password:"password"}"' -X POST http://localhost:3000/api/login
```

Make authenticated request.

```bash
$ curl -H "X-Auth-Token: 123abc" -H "X-User-Id: 112233" -H "Content-Type:application/json"  -d '{"email":"newuser@home.com",password:"password"}"' -X POST http://localhost:3000/api/users
```

Login request for a GraphQL API.

```bash
$ curl -H "Content-Type:application/json"  -d '{"query":"mutation{login(email:"user@home.com",password:"password"){token,user}}"' -X POST http://localhost:3000/graphql
```

## References

- [curl.1 the man page](https://curl.haxx.se/docs/manpage.html)
