let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { inherit config; };
  lib = pkgs.lib;
  compilerVersion = "ghc8102";
  compilerSet = pkgs.haskell.packages."${compilerVersion}";
  gitIgnore = pkgs.nix-gitignore.gitignoreSourcePure;
  config = {
    packageOverrides = super: let self = super.pkgs; in rec {
      haskell = super.haskell // {
        packageOverrides = self: super: {
          aoc2020-day1 = super.callCabal2nix "aoc2020-day1" (gitIgnore [./.gitignore] ./day1) {};
          aoc2020-day2 = super.callCabal2nix "aoc2020-day2" (gitIgnore [./.gitignore] ./day2) {};
          aoc2020-day3 = super.callCabal2nix "aoc2020-day3" (gitIgnore [./.gitignore] ./day3) {};
        };
      };
    };
  };
  filterAoc = lib.filterAttrs (n: v: lib.hasPrefix "aoc2020-" n);

in {
  inherit pkgs compilerSet;
  aoc = filterAoc compilerSet;
  shell = compilerSet.shellFor {
    packages = p: lib.attrValues (filterAoc p);
    buildInputs = with pkgs; [
      compilerSet.cabal-install
      compilerSet.haskell-language-server
    ];
  };
}
