brotli
=====

An OTP library

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

iex(3)> data = File.read!("README.md")
"brotli\n=====\n\nAn OTP library\n\nBuild\n-----\n\n    $ rebar3 compile\n..."

iex(4)> :brotli.encode(data)
<<27, 63, 0, 0, 4, 254, 115, 91, 127, 37, 253, 64, 37, 145, 9, 24, 138, 6, 67,
  17, 37, 220, 118, 138, 187, 41, 76, 228, 132, 35, 13, 24, 219, 14, 32, 47, 4,
  75, 143, 249, 185, 81, 45, 180, 71, 235, 190, 41, 53, 44, ...>>
```
