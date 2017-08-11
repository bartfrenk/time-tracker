# Time Tracker

Tracks time spent on JIRA issues. Example workflow:

```bash
track start PROJECT-3145
# work on issue PROJECT-3145
track start 2718
# work on issue 2718 in the configurable default project
track stop
track review
track book
```

## Setup

Requires [Haskell Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install).

To set up, clone the repository and run

```bash
cd time-tracker
stack build
stack install
```

This builds the executable and copies it to a central location (on my system: ~/.local/bin).

To get help on the available commands, run

```bash
track --help
```
