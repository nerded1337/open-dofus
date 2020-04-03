{ pkgs ? import <nixpkgs> {} }: with pkgs;
  stack.overrideAttrs (old: {
    patches = [ ./fix-pkg-parser.patch ];
  })
