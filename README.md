Observer Web
============
Observer Web is erlang observer web frontend, base code borrowed from observer gui.

## Feature
Currently supported:
* System
* Load Charts
* Memory Allocators
* Processes (preview)

## TODO

- [ ] Applications
- [x] Processes
- [x] Process info
- [x] Port info
- [x] Table viewer
- [ ] Trace Overview

## Usage
Build backend
```bash
rebar3 compile
```

Build fornt end
```bash
cd priv
npm install
npm run build
```
To start the release in the foreground.
```bash
./start.sh
```
Open http://127.0.0.1:8449 in your browser


## License

    The MIT License (MIT)

Checkout http://package.elm-lang.org/packages/terezka/elm-plot/5.1.0/
