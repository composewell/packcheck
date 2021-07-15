## PR Checklist

The commit-id of packcheck in the CI configs must be updated after any
change in `packcheck.sh` otherwise CIs would be passing because they
will be happily using an old version of packcheck. This should be the last
commit in the PR.

## Release Process

* See [the streamly maintanenace
  guide](https://github.com/composewell/streamly/blob/master/MAINTAINING.md)
  for general process.
* Update the commit-id of the packcheck script in config files
* Test the whole build matrix removing all the commented builds.
* Test removing caching
* Update the version number in config files
* Update version number in the script
* Update Changelog accordingly
