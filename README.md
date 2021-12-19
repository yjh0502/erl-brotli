brotli
=====

A library providing Erlang API for Brotli compression library.

Build
-----

    $ rebar3 compile

Example Usage (from Elixir)
-----
Start `iex` and then:

```
iex(1)> c ("src/brotli.erl")
[:brotli]

iex(2)> c ("src/brotli_nif.erl")
[:brotli_nif]

iex(3)> c ("src/brotli_encoder.erl")
[:brotli_encoder]

iex(4)> data = File.read!("README.md")
"brotli\n=====\n\nAn OTP library\n\nBuild\n-----\n\n    $ rebar3 compile\n..."

iex(5)> :brotli.encode(data)
{:ok,
 <<27, 26, 3, 0, 140, 146, 28, 142, 124, 217, 200, 164, 20, 156, 211, 199, 168,
   156, 219, 19, 176, 219, 248, 140, 58, 157, 210, 144, 133, 150, 2, 76, 94,
   201, 231, 55, 179, 243, 125, 215, 180, 141, 235, 59, 213, 185, 57, 61, ...>>}
```

### License

Library and most tests are licensed on [BSD-3-Clause](LICENSE) License.
Some tests, in files matching glob `prop_*.erl` are GPL-3.0 licensed.
