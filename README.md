# geminic

An Erlang library for building Gemini protocol clients.

## Build

    $ rebar3 compile

## Retrieve a Gemini page without auto-redirect

    1> geminic:request( "gemini://warpengineer.space" ).

## Retrieve a Gemini page with auto-redirect

    1> geminic:setopt(autoredirect, true).
    2> geminic:request("gemini://warpengineer.space").

## Retrieve a Gemini page with a custom timeout of 60 seconds (default is 30 seconds)

    1> geminic:request( "gemini://warpengineer.space", 60000 ).

## Use a client certificate

    1> geminic:setopt(clientcert, geminic:make_cert("mycert.crt","mykey.key")).
    2> geminic:request("gemini://warpengineer.space/restricted/index.gemini").

## Author

- [@WarpEngineer](https://github.com/WarpEngineer)

## License

Copyright (c) 2021 A. G. Madi.
Licensed under [MIT](https://github.com/WarpEngineer/geminic/blob/master/LICENSE).

