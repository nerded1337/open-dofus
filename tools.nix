with import (builtins.fetchGit {
  name = "nixos-20.09";
  url = https://github.com/nixos/nixpkgs-channels/;
  ref = "refs/heads/nixos-20.09";
}) {};

callPackage ./tools/stack.nix {}
