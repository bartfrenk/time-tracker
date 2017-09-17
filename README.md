# Time Tracker

Tracks time spent on JIRA issues. Example workflow:

```bash
# work on issue PROJECT-3145
track start PROJECT-3145
# started work on issue 2718 in the configurable default project 30 minutes ago
track start 2718 -t -30m
track stop
track review
track book
```

The `start` and `stop` commands update the local log, the `review` command
displays the local log by day, and the `book` command attempts to push the local log to
JIRA and clears all local items that were sucessfully pushed.

## Setup

Requires [Haskell Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install).

To set up, clone the repository and run

```bash
cd time-tracker
stack setup
stack build
stack install
```

This builds the executable and copies it to a central location (on my system:
~/.local/bin). Then copy `res/config.yaml` to `~/.track.yaml`, and fill in the
remaining details. Finally, create the directory to hold the local state:

```bash
mkdir ~/.track
```

## Run

To get help on the available commands, run

```bash
track --help
```
