{ mkDerivation, base, bytestring, cassava, io-streams, QuickCheck
, stdenv, tasty, tasty-quickcheck, vector
}:
mkDerivation {
  pname = "cassava-streams";
  version = "0.3.0.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cassava io-streams vector
  ];
  testHaskellDepends = [
    base bytestring cassava io-streams QuickCheck tasty
    tasty-quickcheck vector
  ];
  homepage = "https://github.com/pjones/cassava-streams";
  description = "io-streams interface for the cassava CSV library";
  license = stdenv.lib.licenses.bsd3;
}
