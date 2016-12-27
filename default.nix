let pkgs = import <nixpkgs> {};
in pkgs.haskellPackages.callPackage ./gopher-proxy.nix { }
