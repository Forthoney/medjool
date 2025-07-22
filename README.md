## Medjool
Jinmori is a command line argument parser supporting a variety of parsing paradigms.

### Usage
Create a command by instantiating structure through the `CommandFn` functor.
This functor requires you to set which parser to use, as well as the flags and arguments the command should take.

### Parser Presets
Currently, the library ships with one preset parser and a functor to generate preset parsers.
The preset parser `GNUParser` works with GNU-style flag/option syntax, supporting short and long flags with following rules:
* If a flag only has a long version (e.g., --interactive), the argument _must_ be separated by a `=`.
* If a flag has a short version, the argument must be separated by a space.
* All strings after `--` are considered arguments.
Medjool does not support extrapolation for unambiguous but incomplete flags.

The functor `Parser_PrefixFn` supports a simpler convention where flags are indicated by a prefix of choice.
This can be used to express MLton style flags which are preceded by a single `-`.
