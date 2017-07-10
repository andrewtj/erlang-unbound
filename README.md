# erlang-unbound

An Erlang interface to NLnet Lab's libunbound. libunbound provides a DNS
resolver including cache and DNSSEC validation.

## Overview

libunbound's `ub_ctx` is made available to Erlang via a port driver that is
opened and controlled via the `unbound_drv` module. On top of `unbound_drv`
`unbound_server` provides query coalescing and cancellation in the event of a
client crash. An instance of `unbound_server` maintained by the application's
server is also made available via the `unbound` module.

## Configuration

The default configuration for `unbound_server` is specified by the
`server_defaults` property in the applications environment. It is not valid
to include process registration (`{register, _}`) or trust anchor maintain
(`{trust_anchor, {maintain, _}}`) options in the default environment.

### Properties

* `{forwarders, Forwarder}` specifies one or more hosts to forward queries to.
  `Forwarder` may be a single iolist, binary string or tuple representing an
   IPv4 or IPv6 address, or a list of the aforementioned
* `hosts` reads a list of hosts from the systems hosts file
* `{hosts, PathSpec}` reads a list of hosts from `PathSpec`
* `{register, ServerName}` where `ServerName` is a server name specification
  passed to `gen_server:start_link/4` (not valid as a default)
* `{trust_anchor, TrustAnchorSpec}` configures a DNSSEC trust anchor.
  `TrustAnchorSpec` must be one of:
  * `auto` - uses the key root.key contained in the applications priv dir. If
    the server is started by the application supervisor it will maintain the
    trust anchor
  * `{auto, PathSpec}` as above with a key located at `PathSpec`
  * `read` uses the key root.key contained in the applications priv dir
  * `{read, PathSpec}` as above with a key located at `PathSpec`
  * `{maintain, PathSpec}` uses and maintains a key located at `PathSpec`
    (not valid as a default)
  * a binary string or string containing a trust anchor
* `resolvconf` reads a list of hosts to forward queries to from the systems
  resolv.conf
* `{resolvconf, PathSpec}` reads a list of hosts to forward queries to from
  `PathSpec`
* `{Key, Value}` specifies an unbound.conf option where `Key` and `Value` are
  iolists or binary strings, and `Key` includes a trailing ':'

A `PathSpec` is a file system path as an iolist or binary. If it is contained
a tuple tagged `priv`, the path will be appended to the applications priv dir
with `filename:join/2`.

## API

### Records

* `#ub_question{}`
	* `name`
	* `type`
	* `class`
* `#ub_result{}`
	* `question`
	* `data`
	* `canonname`
	* `rcode`
	* `answer_packet`
	* `havedata`
	* `nxdomain`
	* `secure`
	* `bogus`
	* `why_bogus`
	* `ttl`
* `#ub_callback{}`
	* `process`
	* `ref`
	* `error`
	* `result`
* `#ub_drv_callback{}`
	* `port`
	* `id`
	* `error`
	* `result`

### unbound

`start()` and `stop()` are helpers for starting and stopping the application
from a shell.

`resolve(#ub_question{} = Q) -> {ok, Ref}`

`resolve(Name, Type) -> {ok, Ref}`

`resolve(Name, Type, Class) -> {ok, Ref}`

`cancel(Ref) -> ok`

### unbound_server

`defaults()`

`start_link() -> {ok, Pid}`

`start_link(Opts) -> {ok, Pid}`

`resolve(ServerRef, #ub_question{} = Q) -> {ok, Ref}`

`resolve(ServerRef, Name, Type) -> {ok, Ref}`

`resolve(ServerRef, Name, Type, Class) -> {ok, Ref}`

`cancel(ServerRef, Ref) -> ok`

### unbound_drv

`open() -> {ok, Port}`

`close(Port) -> ok`

`resolve(Port, #ub_question{} = Q) -> {ok, Id}`

`cancel(Port, Id) -> ok`

`add_ta(Port, TrustAnchor) -> ok`

`add_ta_autr(Port, Path) -> ok`

`add_ta_file(Port, Path) -> ok`

`hosts(Port) -> ok`

`hosts(Port, Path) -> ok`

`resolvconf(Port) -> ok`

`resolvconf(Port, Path) -> ok`

`set_fwd(Port, Addr) -> ok`

`get_option(Port, Key) -> {ok, Value}`

`set_option(Port, Key, Value) -> ok`

`version(Port) -> {ok, Version}`
