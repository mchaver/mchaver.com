---
title: Using curl and GraphQL
tags: command line, GraphQL
---

POST a mutation to login. We can put that GraphQL query in the body. We will
receive a token and a user object.

```bash
curl 
  -H "Content-Type:application/json" 
  -d '{
      "query":
        "mutation {
          login(email:\"admin@admin.com\",password:\"password\") {
            token,
            user {
               id
              ,createdAt
              ,updatedAt
            }
          }
        }"
      ,"variables":null
    }' 
  -X POST 'http://localhost:4000/graphql'
```

GET an unauthenticated GraphQL query. This requires the `-g` flag in order to 
allow curly brackets `{}` in the URL.

```bash
curl 
 -g -X GET 'http://localhost:4000/graphql?query={post(date:\"2017-01-01\"){date,contents}}'
```

GET an authenticated GraphQL query. The required headers may vary depending on 
the application.

```bash
curl 
  -H "Content-Type:application/graphql" 
  -H "X-Auth-Token:  aaaaaaaaaaaaaaaaaaaaaaaa" 
  -H "X-Auth-UserId: 888888888888888888888888" 
  -g -X GET 'http://localhost:4000/graphql?query={user(id:\"Julio\"){name,age}}'
```
