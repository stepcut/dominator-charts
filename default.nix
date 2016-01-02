{ mkDerivation, base, containers, ghcjs-base, isomaniac, mtl
, stdenv, text, time
}:
mkDerivation {
  pname = "isomaniac-charts";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers ghcjs-base isomaniac mtl text time
  ];
  homepage = "http://www.github.com/Happstack/isomaniac-charts";
  description = "A chart library for the HTML5 canvas using isomaniac";
  license = stdenv.lib.licenses.bsd3;
}
