# Fix Permissions

Set the permissions of files and directories to a specific mode (one for files and another for directories). This is especially useful as a cron job to fix permissions on a shared file system, such as one hosted by netatalk, which has a strange habit of using weird permissions.

## Building

```
$ rebar get-deps
$ rebar -r prepare-deps
$ make
```

## Usage

Invoke like so `fixperms --help` to get the command line help.
