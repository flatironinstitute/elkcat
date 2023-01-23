# elkcat

elkcat is a highly configurable elasticsearch query tool designed for querying log messages.
It is similar to elktail, but does not (yet) have follow (tail -f) functionality.
It has a complex configuration that allows customization of which arguments can be used to query documents.

See the [config](elkcat.yaml) file for documentation on the configuration.

The example configuration provides these options:

```
Usage: elkcat OPTION|QUERY ...
                --not               Same as "!" (below)
  -o            --or                Same as "|" (below)
  -n COUNT      --count=COUNT       Select at most COUNT messages.
  -r            --reverse           Reverse sort order (newest to oldest).
  -a TIMESTAMP  --after=TIMESTAMP   Select messages gte TIMESTAMP.
  -b TIMESTAMP  --before=TIMESTAMP  Select messages lt TIMESTAMP.
  -h TERM       --host=TERM         Select hostname matching TERM.
  -I IP         --ip=IP             Select messages by IP address,
                                    which may be a subnet specification.
  -S VALUE      --severity=VALUE    Select messages with severity VALUE,
                                    which may be a comma-separated list.
  -F VALUE      --facility=VALUE    Select messages with facility VALUE,
                                    which may be a comma-separated list.
  -P TERM       --program=TERM      Select program matching TERM.
  QUERY         Select messages matching QUERY.
  '('           Begin a grouped set of arguments.
  ')'           End a grouped set of arguments.
  '!'           Exclude messages matching the immediately next argument
                (NOT: high precedence).
  '|'           Select messages matching either left or right arguments
                (OR: low precedence).

Arguments can be grouped and joined by operators (above), similar to find(1).
By default all terms must match (AND).

TIMESTAMP arguments may be absolute or relative in various forms, including
"2 days ago" or "last Thursday".  Timestamps are interpreted relative to the
current time for the first argument, or any preceding time for subsequent
arguments.  For example, "-a 17:00 -b '5 minutes'" selects until 5:05 pm.

TERM arguments can be an exact value, a comma-separated list of values, a
wildcard match (using "*?"), or a regular expression (as "/REGEX/[i]").

QUERY arguments can be a full-text search phrase, a simple elastic query
(starting with "+-"), or a regular expression (as "/REGEX/[i]").  When
possible avoid regular expressions as they can be slow.
```

## Installation

1. Install [Haskell stack](https://docs.haskellstack.org/en/stable/).
1. Clone or download this repo.
1. `cd gnulib-parse-datetime && autoreconf && ./configure && make && cd ..` (TODO: incorporate this into cabal build)
1. `stack install`

Running `elkcat` the first time will install the example config file to `~/.config/elkcat.yaml`.  You should then edit this file to taste.
