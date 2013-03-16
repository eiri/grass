Grass
======

## Synopsis

Erlang graph database compatable with [Gremlin](https://github.com/tinkerpop/gremlin/wiki). Backend on Basho's [LevelDB](https://github.com/basho/leveldb), REST interface with [Webmachine](https://github.com/basho/webmachine/wiki)

## Motivation

I am interested in studing of graph theory, so writing a toy graph database seems to be an obvious way to go about it.

## Notes and ToDo

Currently (ver 0.1) Grass supports only one undirected unannotated graph and provides read-only web interface for graph's representation in [DOT langage](http://www.graphviz.org/content/dot-language)

On the roadmap support for graphs' annotation, support for directed graphs, complete REST interface and complaiance with Tinkerpop stack.

## Installation

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

## Quick example

There are built-in function `grass:example(Key)` created for demonstration proposes. It creates a graph out of one of the three poems, where each word made to vertex and edged to immidiate neighbours. Available values for `Key`: `limeric`, `tiger` or `jabberwocky`.

### How to run

_Note: To draw the graph [GraphViz](http://www.graphviz.org/Documentation.php) need to be installed._<br />
_On MacOSX: `$ brew install brew install graphviz --with-app --with-freetype`_

```erlang
...
22:50:41.157 [info] * Starting grass: [ok]
(grass@StarFortress)1> grass:example(limeric).
done
(grass@StarFortress)2>
```

In next terminal ( _grass' node needs to be up_ )

```bash
$ curl http://localhost:9922?graph=limeric | dot -Tpng -o limeric.png
$ open limeric.png
```

You should get something like:<br />
![Limeric Graph](https://github.com/eiri/grass/blob/priv/limeric.png")

## General usage

Create and populate a graph

```erlang
(grass@StarFortress)> grass:graphs().
[]
(grass@StarFortress)> grass:create(<<"G">>).
21:48:05.115 [debug] Supervisor gs_graph_sup started gs_graph_server:start_link([{<<"G">>,"/Users/eiri/...
ok
(grass@StarFortress)> grass:verticies(<<"G">>).
[]
(grass@StarFortress)> grass:edges(<<"G">>).
[]
(grass@StarFortress)> grass:add_vertex(<<"G">>, <<"a">>).
ok
(grass@StarFortress)> grass:add_vertex(<<"G">>, <<"b">>).
ok
(grass@StarFortress)> grass:add_vertex(<<"G">>, <<"c">>).
ok
(grass@StarFortress)> grass:add_vertex(<<"G">>, <<"d">>).
ok
(grass@StarFortress)> grass:add_edge(<<"G">>, <<"a">>, <<"b">>).
ok
(grass@StarFortress)> grass:add_edge(<<"G">>, <<"a">>, <<"c">>).
ok
(grass@StarFortress)> grass:add_edge(<<"G">>, <<"b">>, <<"c">>).
ok
(grass@StarFortress)> grass:add_edge(<<"G">>, <<"c">>, <<"d">>).
ok
(grass@StarFortress)> grass:vertex_exists(<<"G">>, <<"a">>).
true
(grass@StarFortress)> grass:vertex_exists(<<"G">>, <<"z">>).
false
(grass@StarFortress)> grass:edge_exists(<<"G">>, <<"a">>, <<"c">>).
true
(grass@StarFortress)> grass:edge_exists(<<"G">>, <<"a">>, <<"b">>).
true
(grass@StarFortress)> grass:edge_exists(<<"G">>, <<"a">>, <<"d">>).
false
(grass@StarFortress)> grass:verticies(<<"G">>).
[<<"a">>,<<"b">>,<<"c">>,<<"d">>]
(grass@StarFortress)> grass:edges(<<"G">>).
[[<<"a">>, <<"b">>],
 [<<"a">>, <<"c">>],
 [<<"b">>, <<"c">>],
 [<<"c">>, <<"d">>]]
(grass@StarFortress)> grass:add_vertex(<<"G">>, <<"e">>).
ok
(grass@StarFortress)> grass:add_vertex(<<"G">>, <<"f">>).
ok
(grass@StarFortress)> grass:add_edge(<<"G">>, <<"b">>, <<"e">>), grass:add_edge(<<"G">>, <<"e">>, <<"f">>), grass:add_edge(<<"G">>, <<"c">>, <<"f">>).
ok
(grass@StarFortress)> grass_utils:find_path(<<"G">>, <<"a">>, <<"f">>).
[<<"a">>,<<"b">>,<<"c">>,<<"f">>]
(grass@StarFortress)> grass_utils:find_short_path(<<"G">>, <<"a">>, <<"f">>).
[<<"a">>,<<"c">>,<<"f">>]
```

Web interface

```bash
$ curl http://localhost:9922?graph=G
graph G {
  "a" -- "b";
  "a" -- "c";
  "b" -- "c";
  "b" -- "e";
  "c" -- "d";
  "c" -- "f";
  "e" -- "f";
}
```

Clean up

```erlang
(grass@StarFortress)> grass:drop(<<"G">>).
ok
(grass@StarFortress)> grass:verticies(<<"G">>).
[]
(grass@StarFortress)> grass:graphs().
[<<"G">>]
(grass@StarFortress)> grass:destroy(<<"G">>).
ok
(grass@StarFortress)> grass:graphs().
[]
```

## Complete API

[API wiki](https://github.com/eiri/grass/wiki/API)

## Documentation

Check out [Wiki](https://github.com/eiri/grass/wiki) or build a local documentation:

```bash
$ make docs
$ cd doc
$ open index.html
```

## Tests

API covered with suite of unit tests. CI and code coverage to follow.

```bash
...
$ make test
======================== EUnit ========================
module 'grass'
  grass:278: test_add_vertex (Add vertex)...ok
  ...
  [done in 0.146 s]
=======================================================
  All 55 tests passed.
```

## License

MIT. See [License](https://github.com/eiri/grass/blob/master/License "MIT License")