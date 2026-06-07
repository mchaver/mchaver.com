---
title: Moving from Haskell Servant to Rust Axum
kind: note
state: complete
tags: haskell, rust
---

I have a couple of servers running with Haskell [servant](https://docs.servant.dev/en/latest/tutorial/index.html). If you don't know servant, it's a package for defining APIs as Haskell types. It's a nice way to define server and client APIs. If you like Haskell, definitely check it out.

Anyway, the servers are running in docker containers on a VPS. Even though they are relatively small and handle simple things like basic auth, user accounts, a bit of business logic, etc. the updates can be painfully slow to rebuild the dockers even with caching. A fresh build takes up to 40 minutes. A cached build can be ten to thirty minutes depending on how deep the change is.

I came across this [comment on hacker news](https://news.ycombinator.com/item?id=47958503) and started thinking about testing other ecosystems. I have used a bit of Rust in the past so I decided to migrate one server from Haskell servant to Rust axum. The main concern was keeping code high quality so I used Rust's linter clippy and ported over all the unit tests I had in Haskell to Rust. I also used docker to build the project. Currently a fresh build is about 15 minutes as opposed to Haskell's 40 minutes. I did have to give up some type safety, but for small servers with relatively simple business logic, it is not a huge loss.

I will launch the service this week. I'll report back in a month or so on my experiences in production.
