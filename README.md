# erlang-unbound

An Erlang interface to NLnet Lab's libunbound. libunbound provides a DNS
resolver including cache and DNSSEC validation.

## Overview

libunbound's `ub_ctx` is made available to Erlang via a port driver that is
opened and controlled via the `unbound_drv` module. On top of `unbound_drv`,
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

A `DriverError` is one of `{error, nomem}` if insufficient memory was
available to complete the callback, or, if libunbound encountered an error,
`{error, {ub, N, S}}` where `N` is an integer identifying the error and `S` is
a binary string describing the error.

### Records

Records can be included with `-include_lib("unbound/unbound.hrl").`.

* `#ub_question{}`
    * `name` - a binary containing a DNS Name in ASCII presentation format
    * `type` - an integer representing a DNS Type
    * `class` - an integer representing a DNS Class
* `#ub_result{}`
    * `question` - a `#ub_question{}`
    * `data` - a list of DNS Resource Record datas binaries
    * `canonname` - the canonical DNS Name of the result in ASCII, or an
      empty binary
    * `rcode` - response code as an integer
    * `answer_packet` - the message that answers the query as a binary
    * `havedata` - `true` if there is data, `false` otherwise
    * `nxdomain` - `true` if the name does not exist, `false` otherwise
    * `secure` - `true` if the response is secure, `false` otherwise
    * `bogus` - `true` if a security failure occurred, `false` otherwise
    * `why_bogus` - binary string explaining why the response is bogus
    * `ttl` - how long the response is valid for in seconds
* `#ub_callback{}`
    * `process` - pid of the `unbound_server` providing this response
    * `ref` - opaque identifier for the resolve request
    * `error` - `false` if no error occurred or `DriverError`
    * `result` - a `#ub_result{}` if the request could be answered or `false`
      otherwise
* `#ub_drv_callback{}`
    * `port` - port of the port driver providing this response
    * `id` - integer identifier for the resolve request
    * `error` - `false` if no error occurred or `DriverError`
    * `result` - a `#ub_result{}` if the request could be answered or `false`

### DNS Parameters

Macros representing DNS Class and Type integers can be included with
`-include_lib("unbound/params.hrl").`. For example Class IN is `?UB_CL_IN`
and Type SOA is `?UB_TY_SOA`. See `include/params.hrl`.

### unbound

`start()` and `stop()` are helpers for starting and stopping the application
from a shell.

`resolve(#ub_question{} = Q)` is equivalent to calling
`unbound_server:resolve/2` with a `ServerRef` of the applications
`unbound_server` instance.

`resolve(Name, Type)` is equivalent to calling
`resolve(Name, Type, ?UB_CL_IN).`

`resolve(Name, Type, Class)` is equivalent to calling
`unbound_server:resolve/4` with a `ServerRef` of the applications
`unbound_server` instance.

`cancel(Ref)` is equivalent calling `unbound_server:cancel/2`
with a `ServerRef` of the applications `unbound_server` instance.

### unbound_server

`defaults()` returns the default server options set in the application
environment. This will crash if invalid options are set in the environment.

`start_link()` is equivalent to calling `start_link(defaults())`. This will
crash if invalid options are set in the environment.

`start_link(Opts)` creates a new process configured with the properties in the
list `Opts`.

`resolve(ServerRef, Name, Type)` is equivalent to calling
`resolve(ServerRef, Name, Type, ?UB_CL_IN)`.

`resolve(ServerRef, Name, Type, Class)` takes `Name`, an iolist or binary
containing a DNS name in ASCII format, `Type` an integer representing a DNS
Type and `Class` representing a DNS Class. It constructs a `#ub_question{}`
`Q` and then calls `resolve(ServerRef, Q)`.

`resolve(ServerRef, #ub_question{} = Q)` in the success case returns
`{ok, Ref}` where `Ref` is an opaque identifier for the request. When an
answer becomes available it will be delivered to the calling process as a
`ub_callback{}`.

`cancel(ServerRef, Ref)` takes a `Ref` as returned by `resolve` and cancels
the associated resolve request.

### unbound_drv


`load()` and `unload()` load and unload the port driver.

`open()` returns a new instance of the port driver (calling load if necessary)
and returning `{ok, Port}` or `{error, Reason}`. `close(Port)` closes the
port.

`resolve(Port, #ub_question{} = Q)` starts an asynchronous query. `Q` must
contain a valid `#ub_question{}`. If `Q` is valid, the port will respond with
`{ok, Id}` where `Id` is an integer identifying the request, or,
a `DriverError`. When an answer becomes available, it will be delivered to the
drivers parent process as a `#ub_drv_callback{}`.

`cancel(Port, Id)` cancels the request identified by `Id` and returns `ok` or
`DriverError`.

`add_ta(Port, TrustAnchor)` loads a trust anchor from the binary string
`TrustAnchor` and returns `ok` or `DriverError`.

`add_ta_autr(Port, Path)` loads a trust anchor from file `Path`, a binary
string, and returns `ok` or `DriverError`. If the file is writable, libunbound
will update it as needed.

`add_ta_file(Port, Path)` loads a trust anchor from file `Path`, a binary
string, and returns `ok` or `DriverError`.

`hosts(Port)` loads the systems host file and returns `ok` or `DriverError`.

`hosts(Port, Path)` loads hosts from file `Path`, a binary string, and returns
`ok` or `DriverError`.

`resolvconf(Port)` loads forwarders from the systems resolv.conf file and
returns `ok` or `DriverError`.

`resolvconf(Port, Path)` loads forwarders from the file `Path`, a binary
string, and returns `ok` or `DriverError`.

`set_fwd(Port, Addr)` adds `Addr`, a binary string containing an IPv4 or IPv6
address as a forwarder.

`get_option(Port, Key)` retrieves the configuration value for the binary
string `Key`. Returns `{ok, Value}` where `Value` is a binary string or
`DriverError`.

`set_option(Port, Key, Value)` sets the configuration value `Key` to value
`Value`. `Key` and `Value` must be binary strings and `Key` must end with a
':'. Returns `ok` or `DriverError`.

`version(Port)` returns `{ok, Version}` where `Version` is a binary string
containing the version of libunbound in use, or `{error, nomem}`.
