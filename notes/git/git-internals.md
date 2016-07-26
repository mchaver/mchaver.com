Plumbing - low-level
Porcelain - high-level

Git was initially developed as a toolkit for Version Control System (VCS)
rather than a user-friendly VCS. Ii has a lot of verbs for doing low-level work.

`git run` creates `.git` which is where almost everything that stores and
manipulates is located.

####Git Objects

Git is a content-addressable filesystem, simple key-value data store. You can
insert content into it, and it will return a key which can be used to retrieve
the content again at any time.

`hash-object` takes data, stores it in `.git` and returns the key. 

[Git Internals](https://git-scm.com/book/en/v2/Git-Internals-Plumbing-and-Porcelain)
