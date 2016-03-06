# Fix Permissions

Set the permissions of files and directories to a specific mode (one for files and another for directories). This is especially useful as a cron job to fix permissions on a shared file system, such as one hosted by [netatalk](http://netatalk.sourceforge.net), which has a strange habit of using weird permissions.

## Building

### Prerequisites

* [Erlang/OTP](http://www.erlang.org) R17|R18
* [rebar](https://github.com/rebar/rebar)

### Testing

To build and test the application:

```
$ make test
```

### Releasing

To produce a self-contained [escript](http://www.erlang.org/doc/man/escript.html), use the `release` make target, like so.

```
$ make release
```

The result (named `fixperms`) can be copied to a suitable location (e.g. `/usr/local/bin`) and invoked directly.

## Usage

Invoke like so `fixperms --help` to get the command line help. Two arguments are required, the source dataset and the destination dataset.

```
$ /usr/local/bin/fixperms photos shared
```

Typically this script is run via a cron job.
