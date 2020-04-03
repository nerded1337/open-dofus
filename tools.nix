with import (builtins.fetchGit {
  name = "nixpkgs-19.09-darwin";
  url = https://github.com/nixos/nixpkgs-channels/;
  ref = "refs/heads/nixpkgs-19.09-darwin";
  rev = "0ee9ceff38f1abd0fcb672841a40e75f6cbc2620";
}) {};

callPackage ./tools/stack.nix {}
