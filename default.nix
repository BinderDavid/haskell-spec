let nixpkgs = import (import ./nix/sources.nix).nixpkgs
  { config.allowUnfree = true; };
in with nixpkgs;
rec {
  lean4mode = fetchFromGitHub {
    repo = "lean4-mode";
    owner = "leanprover-community";
    rev = "1388f9d1429e38a39ab913c6daae55f6ce799479";
    hash = "sha256-6XFcyqSTx1CwNWqQvIc25cuQMwh3YXnbgr5cDiOCxBk=";
  };
  shell = mkShell {
    nativeBuildInputs = [ elan steam-run python3 ];
    shellHook =
     ''
      ## It should be enough to add
      ## (require 'lean4-mode)
      ## to the .emacs file.
      export EMACSLOADPATH="${lean4mode}:$EMACSLOADPATH"
      alias lean="steam-run lean"
      alias lake="steam-run lake"
     '';
  };
}
