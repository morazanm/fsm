# Contributing Guidelines
We are always open to accepting pull requests and suggestions. If you are find a bug and wish to fix it please create a issue and link your pull request to the issue. If you are looking for ways to contribute please check out the [issues](https://github.com/morazanm/fsm/issues) section. Issues marked `good first issue` are a great way to get started. 


## Pull Requests
When creating a pull request clearly specify what you are trying to accomplish and tag the current active maintainers. Please also write your tests using `rackunit`. They can be included either in the `fsm-test` directory or in bottom of the file using the `(module+ test)` pattern.


## Building fsm 
To build `fsm` run `raco make .` at the root of the repository. An alternate option is to open `main.rkt` in DrRacket and hit the run button. There is also a [Makefile](../../Makefile) to help streamline the build processes.


## Running the test suit
To run the test suit you can run `raco test fsm-test fsm-gviz fsm-core fsm-gui`. If you do not want to run the tests locally we do have a CI tool setup to pull requests that will run the tests for you. All you need to do is create a PR with your branch and they will auto run.

If you want to quickly try out your changes we have a [testing-file.rkt](../../testing-file.rkt) with pre-built machines that you can use to test your code. Just make sure you do not include any changes from this file in your Pull request. 


## Scribble files
When a change is made to the scribble files you can either press the run button in DrRacket or run the following from the `fsm-doc` directory:
```bash
scribble fsm.scrbl
```

We have a CI tool that will auto deploy scribble file changes to the fsm website when a pull request is merged into the master branch so you do not need to include the scribble generated files in the pull request. **NOTE: Currently we do not have a CI tool step to check for errors when building the scribble file, so make sure that it compiles before merging into master.**
