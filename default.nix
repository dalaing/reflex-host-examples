{ mkDerivation, base, dependent-sum, mtl, reflex, stdenv
, transformers
}:
mkDerivation {
  pname = "reflex-host-examples";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base dependent-sum mtl reflex transformers
  ];
  license = stdenv.lib.licenses.bsd3;
}
