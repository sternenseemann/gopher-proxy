{ pkgs ? import <nixpkgs> {} }:

let
  hpkgs = pkgs.haskellPackages.override {
    overrides = self: super: {
      attoparsec = self.attoparsec_0_14_1;
    };
  };
in

hpkgs.callPackage ./gopher-proxy.nix { }
