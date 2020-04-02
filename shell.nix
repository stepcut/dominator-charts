{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, ghcjs-base, chili, mtl,
        stdenv, text, time, hsx2hs, random
      }:
      mkDerivation {
        pname = "dominator-charts";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base containers ghcjs-base chili mtl text time hsx2hs random ];
        buildTools = [ pkgs.haskellPackages.cabal-install pkgs.haskellPackages.cabal2nix ];
        homepage = "http://www.github.com/Happstack/isomaniac-charts";
        description = "A chart library for the HTML5 canvas using dominator";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
