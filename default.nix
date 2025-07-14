{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.callPackage ./gopher-proxy.nix { }
