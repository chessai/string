{ stdenv, fetchFromGitHub, cmake }:

stdenv.mkDerivation rec {
  pname = "simdjson";
  version = "0.7.0";

  src = fetchFromGitHub {
    owner = "simdjson";
    repo = "simdjson";
    rev = "v${version}";
    sha256 = "14gi2zq430nfjy424q6r57imc2gnz30nhx4r0wbajzp9qvna819w";
  };

  nativeBuildInputs = [ cmake ];

  cmakeFlags = [
    "-DSIMDJSON_JUST_LIBRARY=ON"
  ];
}
