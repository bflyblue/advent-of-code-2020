let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { inherit config; };
  compilerVersion = "ghc884";
  compilerSet = pkgs.haskell.packages."${compilerVersion}";
  gitIgnore = pkgs.nix-gitignore.gitignoreSourcePure;
  config = {
    packageOverrides = super: let self = super.pkgs; in rec {
      haskell = super.haskell // {
        packageOverrides = self: super: {
          day1 = super.callCabal2nix "day1" (gitIgnore [./.gitignore] ./day1) {};
        };
      };
    };
  };

in {
  inherit pkgs;
  shell = compilerSet.shellFor {
    packages = p: with p; [
      day1
    ];
    buildInputs = with pkgs; [
      compilerSet.cabal-install
      compilerSet.haskell-language-server
    ];
  };
}
