While you are running the test-server, you can either run test-client or the following curl commands:
```
curl -H 'Content-type: application/json' localhost:3000/user/add --data '{"name": "Alice", "age": 42}'
curl -H 'Content-type: application/json' localhost:3000/user/get/Alice
```
