# servant-starter-app

A fully functional app template for starting a new
[servant](https://hackage.haskell.org/package/servant) app with [servant-auth-cookie](https://hackage.haskell.org/package/servant-auth-cookie), [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple) and [postgresql-simple-migrations](https://github.com/ameingast/postgresql-simple-migration). 

Check out the [servant-auth](https://github.com/sboehler/servant-starter-app/tree/servant-auth) branch for a version that uses [servant-auth](https://github.com/haskell-servant/servant-auth).

This is the result of my own Haskell learning experience - reviews, helpful
suggestions & pull requests are welcome!

## Instructions

Prerequisites: Install [stack](https://docs.haskellstack.org/en/stable/README/)
and have a PostgreSQL database named 'servant-starter-app' running on port
5432, without authentication (see src/Database.hs if you require  additional
configuration).

Starting the server:

```bash
stack setup
stack run 
```

Testing the API:

```bash
# create a new user
curl -X POST -v -H "Content-Type: application/json" -d '{"credentialsEmail":"user@example.com", "credentialsPassword":"a password"}' localhost:4000/user

# log in
curl -X POST -b cookies -c cookies -v -H "Content-Type: application/json" -d '{"credentialsEmail":"user@example.com", "credentialsPassword":"a password"}' localhost:4000/session

# access the protected user endpoint, which returns the user as a JSON object
curl -b cookies -c cookies -v -H "Content-Type: application/json" localhost:4000/user

# log out
curl -X DELETE -b cookies -c cookies -v -H "Content-Type: application/json" localhost:4000/session

# verify the user endpoint is not accessible anymore
curl -b cookies -c cookies -v -H "Content-Type: application/json" localhost:4000/user
```
