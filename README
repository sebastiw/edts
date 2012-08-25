                                     __    __
                                    |  \  |  \
                      ______    ____| $$ _| $$_     _______
                     /      \  /      $$|   $$ \   /       \
                    |  $$$$$$\|  $$$$$$$ \$$$$$$  |  $$$$$$$
                    | $$    $$| $$  | $$  | $$ __  \$$    \
                    | $$$$$$$$| $$__| $$  | $$|  \ _\$$$$$$\
                     \$$     \ \$$    $$   \$$  $$|       $$
                      \$$$$$$$  \$$$$$$$    \$$$$  \$$$$$$$

                    -- The Erlang Development Tool Suite --

## Copyright ##
2012 Thomas Järvstrand

EDTS is licensed under the Lesser Gnu General Public License, please see
COPYING.LESSER for details.

## Introduction ##

The Erlang Development Tool Suite (EDTS) is meant to be a package of useful
development tools for working with the Erlang programming language in Emacs. It
bundles a number of useful external packages, together specialized Erlang
plugins for them, and its own features to create an efficient development
environment that is easy to set up.

Currently EDTS provides:
- Rudimentary project support
- Code navigation (limited)
- Auto-completion, using auto-complete-mode
- Auto-highlighting, using auto-highlight-mode

Planned features in the short term are:
- More extensive navigation
- In-buffer flymake-like compilation
- In-buffer xref checks
- In-buffer running of unit tests
- yasnippets
- A nice interface to the erlang debugger
- Convenient access to Erlang documentation

## Getting started ##

- First of all, make sure your code is compiled using the debug_info compile
  option.
- Get EDTS:

  $git clone git@github.com:tjarvstrand/edts.git
  $cd edts
  $make

- Make sure EDTS is loaded and started in your .emacs:

  (add-to-list 'load-path "<path-to-edts-repo>")
  (require 'edts-start)

  EDTS is meant to be a able to replace distel but only provides a subset of the
  most commonly used of distels features. If you would like to run both distel
  and edts, just make sure that your edts setup is executed after your distel
  setup.

- Configure you projects. EDTS projects are stored in the variable
  `edts-projects'. `edts-projects' is a list of projects, where each project is
  an association list. The properties that can be set for each project are:

  name          - The name of the project. Required.
  root          - The top level directory of where you have your project
                  contents. Required.
  node-sname    - The erlang sname that the project's erlang node should
                  have. Defaults to same name as the project.
  lib-dirs      - A list of paths (relative to the project's root) where the
                  project's code is located. All subdirectories of lib-dirs are
                  assumed to be otp-applications. Defaults to '("lib").
  start-command - A custom command that EDTS should execute to start the
                  project's Erlang node. If this is set, the command must set
                  the node's sname to be the same as the value specified in the
                  project's node-sname.

  Example:
  (setq edts-projects
      '(
        ( ;; My awesome project.
         (name       . "awesome_stuff")
         (root       . "~/src/awesome_stuff")
         (node-sname . "stuff")
         (lib-dirs   . ("lib" "test"))
         (start-command . "./my-start-script.sh")
        )
       ))

That should be all it takes. If it's not, I've failed. Please send a bugreport.

## How it works ##

Once set up, EDTS will automatically fire up it's own Erlang node when you start
your Emacs. Once you open the first file that is located inside one of your
projects, then EDTS will automatically fire up the corresponding project node
and initiate communication between the EDTS-node and the project-node. If a node
with the same name as the project's node is already registered with the Erlang
port mapper daemon (epmd), then EDTS will initiate communication with that node
instead. The EDTS node exposes a REST-interface (using webmachine) through which
emacs can then communicate with the project node.

