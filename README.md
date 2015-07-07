# Fix Permissions

Set the permissions of files and directories to a specific mode (one for files and another for directories). This is especially useful as a cron job to fix permissions on a shared file system, such as one hosted by netatalk, which has a strange habit of using weird permissions.

## Building

Requires [Erlang/OTP](http://www.erlang.org) R17 for building and running, and [rebar](https://github.com/rebar/rebar) for building.

```
$ make
```

The result will be a self-contained [escript](http://www.erlang.org/doc/man/escript.html) named `fixperms` that can be copied to a suitable location (e.g. `/usr/local/bin`) and invoked directly.

## Usage

Invoke like so `fixperms --help` to get the command line help.
