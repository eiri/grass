Grass
======

## Synopsis

Erlang graph database compatable with [Gremlin](https://github.com/tinkerpop/gremlin/wiki). Backend on Basho's [LevelDB](https://github.com/basho/leveldb), REST interface with [Webmachine](https://github.com/basho/webmachine/wiki)

## Motivation

I am interested in studing of graph theory, so writing a toy graph database seems to be an obvious way to go about it.

## Installation and run

_Note: erl and rebar should be presented in $PATH_

```bash
$ git clone https://github.com/eiri/grass.git
$ cd grass
$ make build
$ make run
```

```erlang
...
* Starting grass: [ok]
22:49:56.615 [debug] Supervisor grass started gs_leveldb_server:start_link([]) at pid <0.103.0>
22:49:56.615 [info] Application grass started on node grass@StarFortress
22:49:56.865 [debug] Lager installed handler lager_backend_throttle into lager_event
(grass@StarFortress)1>
```

## API Reference

TBD

## Tests

TBD

## License

MIT. See [License](https://github.com/eiri/grass/blob/master/License "MIT License")