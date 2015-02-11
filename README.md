SCMSRV
======

This is a silly implementation of a generic TCP server in Scheme.  It's intended
for use with MIT/GNU Scheme.  The idea is that the generic server waits on a
server socket and invokes a handler for every incoming connection.  MIT/GNU
Scheme doesn't seem to have a threading library, so the server is single
threaded.

Since a single-threaded TCP server is pretty simple, I'm also planning to
include an HTTP handler, which will be extensible.  It will be responsible for
parsing requests and providing an interface to access requests.  It will invoke
a request handler, which will return a response that will be written to the
server.
