erlang-dns
==========

Erlang/OTP DNS server

EDNS is an authorative non recursive DNS server I am writing for my site
http://www.domain-name-registration.co.za.

The idea is to have a simple DNS server that can be configured with 
arbitrary Zones via simple Erlang modules. It's up to you whether the
Zone is defined in some file or a DB for example. As long as it returns
a zone as a list of resource records EDNS can serve it up.

Current Release
===============
v0.1-alpha

Example
=======
Have a look at the end to end test scenario for an example of how to setup
a zone provider module.

To run the end to end test executed `make e2e`.

System Architecture
===================
Please see the wiki https://github.com/hcvst/erlang-dns/wiki/Architecture

TODO
====
* case insensitive lookup
* sanity-check the zone provided by a callback module
* use inet_dns ADT helper functions in the resolver rather than pattern matching.

Notes
=====
Section 4.3.2 Algorithm in http://tools.ietf.org/html/rfc1034
