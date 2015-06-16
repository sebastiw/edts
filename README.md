                                     __    __
                                    |  \  |  \
                      ______    ____| $$ _| $$_     _______
                     /      \  /      $$|   $$ \   /       \
                    |  $$$$$$\|  $$$$$$$ \$$$$$$  |  $$$$$$$
                    | $$    $$| $$  | $$  | $$ __  \$$    \
                    | $$$$$$$$| $$__| $$  | $$|  \ _\$$$$$$\
                     \$$     \ \$$    $$   \$$  $$|       $$
                      \$$$$$$$  \$$$$$$$    \$$$$  \$$$$$$$

                    -- The Erlang Development Tool Suite --

## License ##
Copyright (C) 2012 by Thomas Järvstrand, Håkan Nilsson
              2013 by Thomas Järvstrand

EDTS is licensed under the Lesser Gnu General Public License. See COPYING.LESSER
for details.

## Introduction ##

The Erlang Development Tool Suite (EDTS) is a package of useful development
tools for working with the Erlang programming language in Emacs. It bundles a
number of useful external packages, together with specialized Erlang plugins for
them, and its own features to create a complete and efficient development
environment that is easy to set up.

Currently EDTS provides:
- A snazzy erlang shell wrapper with syntax highlighting and auto-completion.
- In-buffer flymake-like compilation
- In-buffer xref checks
- Dialyzer integration
- Rudimentary project support
- Code navigation.
- Auto-completion, using auto-complete-mode
- Auto-highlighting, using auto-highlight-mode
- Convenient access to Erlang documentation
- In-buffer running of unit tests
- A usable interface to the erlang debugger

For more information, hit `M-x describe-minor-mode RET edts-mode RET`.

## Getting started ##

#### Requirements:
  - Emacs 23.3 or later (24.2 or higher recommended)

#### First of all, ensure your environment is setup correctly:
  - If you're using Emacs 23, set up package.el according to the instructions
on [Emacs Wiki](http://www.emacswiki.org/emacs/ELPA).
  - You will need make and Erlang installed or the package installation will
fail.
  - You will also need both elpa and melpa package repositories added to your
sources. Add these lines to your .emacs:
    - ```(add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/"))```
    - ```(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))```
    - ```(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))```

  - Make sure your code is compiled with the debug_info option set.

#### Get EDTS:
```M-x package-install RET edts RET```

#### Make sure EDTS gets loaded in your .emacs:
An easy way is to load edts-start:

```
(add-hook 'after-init-hook 'my-after-init-hook)
(defun my-after-init-hook ()
  (require 'edts-start))
```

#### Configure your projects.
  EDTS projects are configured by creating a file called
  `.edts` in your project's root. The configuration file is a number of lines,
  where each line is in the format:
` :<property> <value> `

  Values that are lists must be prefixed with a single-quote, eg. `'("lib")`. See
  example below.

  Valid properties are:

  - `name <string>`

    The name of the project. Defaults to the last component of the project
    root-directory (eg a root set to `~/src/p` would yield `p` as the project name
    if not explicitly set.

  - `node-name <string>`

  The name that the project's erlang node should have. It can be either a short
  or long Erlang node name and defaults to the name of the project.

  - `node-sname <string>`

  Deprecated. This is now an alias for `node-name`.

  - `erlang-cookie <string>`

  The erlang cookie to use for the connection to the project's erlang node. If
  EDTS can connect to the node with its default cookie, the project node's
  cookie for the EDTS server will also be updated.

  - `lib-dirs <list of strings>`

  A list of paths (relative to the project's root) where the project's code is
  located. All subdirectories of lib-dirs are assumed to be otp-applications.
  If you're using rebar, this variable should contain your deps_dir and all
  lib_dirs from your rebar.config.
  Defaults to `'("lib" "deps")`.

  - `start-command <string>`

  A custom command that EDTS should execute to start the project's Erlang node.
  If this is set, the command must set the node's sname to be the same as the
  value specified in the project's node-sname. The command must also not set the
  erlang cookie to anything other than the default `~/.erlang.cookie`.
  Defaults to `erl -sname <node-sname>`.

  - `otp-path <string>`

  The path to any custom OTP-version to use for the project. You only have to
  set this if the project uses a different OTP-release than the one that comes
  first in your exec-path. The OTP-release's bin-directory will be added to
  the head of the exec-path and the PATH environment variable when starting the
  project node.

  - `dialyzer-plt <string>`

  The absolute path to any custom PLT-file on which to base the creation of the
  project's own PLT-file. You only have to set this if the plt in dialyzer's
  default location (`$DIALYZER_PLT` or `$HOME/.dialyzer_plt`, in that order) is
  not appropriate for the project. The plt-file pointed to will not be
  overwritten, but instead used as a base when building the new plt-file for the
  project, which will be located in your `edts-data-directory`.

  - `app-include-dirs <list of strings>`

  A list of directories to search for include files inside each application. Eg.
  if set to `'("include")`, files in any application's include directory can be
  included with `-include("file.hrl")` instead of `-include("../file.hrl")`. This is
  useful if you have a build configuration that sets up your paths for you
  during your normal build process. If set, `'("include")` is usually the only
  reasonable value for this property.

  - `project-include-dirs <list of strings>`

  A list of directories to search for include files inside at the project-level.
  Eg. if set to `'("test/include")`, files in any module can include files from
  `<project-root>/test/include` with just a `-include("file.hrl")`. This is useful
  if you have a build configuration that sets up your paths for you during your
  normal build process.

  - `xref-error-whitelist <list of strings>`

  A list of regular expressions that will be applied as a whitelist to xref
  error descriptions. Useful if you are using external libraries (such as
  Quickcheck) for which you don't have access to binaries compiled with
  debug_info.

  - `xref-file-whitelist <list of strings>`

  Same as `xref-error-whitelist`, but the regular expressions  will be applied
  to the path of file the file where the errors occur rather than the
  description of the error.

##### Example configuration:
  ```elisp
  :name "awesome_stuff"
  :node-sname "awesome"
  :lib-dirs '("lib" "test")
  :app-include-dirs '("include")
  :project-include-dirs '("test/shared/include")
  ```
  Local modifications to project configurations - useful when working on more
  than one checkout of the same project - can be done in two ways:
  - Edit the project configuration file directly. If you do this in Emacs, the
    project will be automatically re-initialized as soon as you save the .edts-
    file.
  - Add overrides by calling `edts-project-override` in your .emacs.
    `edts-project-override` takes a project-root and a plist of configuration
    values to override.

    Example:
    ```elisp
    (edts-project-override "~/my-project" '(:name "my-project-dev"
                                            :node-sname "my-project-dev")
                                            :lib-dirs '("lib" "test" "hacks"))
    ```

#### Get the Erlang documentation (optional).
  - This is now a guided procedure. Just hit `M-x edts-man-setup RET` and follow
    the instructions.

    NB. Requires an internet connection and the process will make a small change
    to you .emacs-file.


That should be all it takes. If it's not, please report any issues on github.

## Backward compatibility note ##

If you have previously configured EDTS 'the old way' in `edts-projects`, you
can still keep this configuration and everything should work as before.
However, EDTS will conveniently convert your old configuration and create a
`.edts` file in your project root. You can turn off this behaviour by setting
`edts-project-inhibit-conversion` to a non-nil value.

## How it works ##

Once set up, EDTS will automatically fire up it's own Erlang node when you start
your Emacs. Once you open the first file that is located inside one of your
projects, then EDTS will automatically fire up the corresponding project node
and initiate communication between the EDTS-node and the project-node. If a node
with the same name as the project's node is already registered with the Erlang
port mapper daemon (epmd), then EDTS will initiate communication with that node
instead. The EDTS node exposes a REST-interface (using webmachine) through which
emacs can then communicate with the project node.

## EDTS and Distel ##

EDTS is meant to be a able to replace Distel but only provides part of the most
commonly used of Distel's features, specifically the equivalents of
`erl-find-module`, `erl-find-source-under-point`, `erl-who-calls` and
`erl-refactor-subfunction`. As far as I know, those are the only Distel features
that 98% of people use, but if there is anything from Distel that you are
missing in EDTS, please let me know.

If you are using EDTS, please remove Distel from your configuration, since
running both can create some confusion.

## Known Issues ##

Some users are experiencing serious performance issues with the auto-completion
during the first use after startup. This is usually solved by typing `C-g` a
couple (two or three, it seems to vary) of times when Emacs "hangs" the first
time. It is most likely caused by a bug in the emacs c-code that affects
the auto-complete package. If you experience these issues, it's recommended to
switch to emacs 24.2 where the problem is [fixed](https://github.com/auto-complete/auto-complete/issues/153),
but if the problems persist, any help in debugging the issue would be appreciated
since I have never myself been able to reproduce it.

When killing some buffers, Emacs 23 decides to move point to `(point-max)` in a
seemingly completely unrelated buffer. This will sometimes happen as an effect
of EDTS' `after-save-hook`. The issue does not exist in Emacs 24.

If you're using proxy server, you have to make sure that the proxy is not used
for communicating with EDTS:
```(add-to-list 'url-proxy-services '("no_proxy" . "0:4587"))```
