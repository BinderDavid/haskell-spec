let nixpkgs = import (import ./nix/sources.nix).nixpkgs
  { config.allowUnfree = true; };
in with nixpkgs;
rec {
  shell = mkShell {
    nativeBuildInputs = [ elan steam-run ];
    shellHook =
     ''
      alias lean="steam-run lean"
      alias lake="steam-run lake"
     '';
  };
}
