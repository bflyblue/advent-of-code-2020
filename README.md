# Advent of Code 2020

My [Haskell](https://www.haskell.org/) solutions for [Advent of Code 2020](https://adventofcode.com/2020).
I'll be aiming for fairly straightforward solutions so I hope they should be easier to read and understand
this way. I'm always happy to hear suggestions or improvements, so feel free to open an
[Issue](https://github.com/bflyblue/advent-of-code-2020/issues) or
[Pull Request](https://github.com/bflyblue/advent-of-code-2020/pulls) if you have anything to add!

## Nix

My preferred style of development is to use a nix-shell to specify tooling per project rather than install
then system-wide. I haven't tested this besides on my devbox but if you'd like to try use it, you can
follow the instructions [here](https://nixos.org/download.html) to install Nix and then the following
should work from the root of the github working directory:

```ShellSession
$ nix-shell
(fetches/builds the environment the first time and caches it in /nix/store)
$ cd day1
$ cabal run < input
```

The nix-shell includes the [haskell-language-server](https://github.com/haskell/haskell-language-server)
which integrates fairly nicely with `nvim`, and VSCode using the
[vscode-haskell](https://github.com/haskell/vscode-haskell) and
[nix-env-selector](https://github.com/arrterian/nix-env-selector) extensions.
