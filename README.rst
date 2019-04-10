=======
 Lagra
=======

RDF for Erlang.

Description
===========

Lagra is an `RDF
<https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/>`_ library
for `Erlang <http://erlang.org/>`_. It supports the serialization and
deserialization of various RDF formats, with the option of incremental
parsing, so that large graph files can be processed in a streaming
fashion. Lagra also provides an API for querying and manipulating
(small) RDF graphs directly in memory.

It supports the following RDF serialization formats:

 * `N-Triples <https://www.w3.org/TR/n-triples/>`_ (read/write)
 * `Turtle <https://www.w3.org/TR/turtle/>`_ (read/write)

Why "lagra"?
------------

It's for managing LAbelled GRAphs.
   
Documentation
=============

`API documentation is available
<http://carfax.org.uk/software/lagra/apidocs/>`_. Comprehensive usage
documentation and tutorial documentation has not yet been written
(it's early days).

Quick example
=============

::

   1> application:start(lagra).
   ok
   2> G = lagra:create_store(trivial).
   <0.88.0>
   3> {ok, File} = file:open("doap.ttl", [read, {encoding, utf8}]).
   {ok,<0.90.0>}
   4> ok = lagra:parse(G, File, turtle, #{base => "http://carfax.org.uk/software/lagra/"}).
   ok
   5> lagra:find_all_t(G, {'_', '_', lagra_model:new_iri(<<"http://usefulinc.com/ns/doap#GitRepository">>)).
   [{{iri,<<"http://git.darksatanic.net/repo/lagra.git/">>},
     {iri,<<"http://www.w3.org/1999/02/22-rdf-syntax-ns#type">>},
     {iri,<<"http://usefulinc.com/ns/doap#GitRepository">>}},
    {{iri,<<"https://github.com/darkling/lagra.git">>},
     {iri,<<"http://www.w3.org/1999/02/22-rdf-syntax-ns#type">>},
     {iri,<<"http://usefulinc.com/ns/doap#GitRepository">>}}]
   6> lagra:destroy_store(G).
   ok

Future plans
============

In no particular order:

 * Write Turtle more elegantly
 * Read/write JSON-LD
 * Read/write RDF/XML
 * Use a remote SPARQL/SPARUL endpoint as a store
 * Improve the in-memory store, so that it is usable for more than
   trivial-sized graphs
 * Add more functions for walking the triple graph
 * Namespace management (parse namespaces from input documents; serialize
   with supplied namespace map)

I have an immediate need for the first two items on the above list,
and will shortly be needing the remote store functionality, so they
have the highest priority.

Contact
=======

:Author: Hugo Mills <hugo@carfax.org.uk>
