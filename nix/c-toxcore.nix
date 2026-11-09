# c-toxcore from its checkout, which has every function hs-toxcore-c binds.
{ lib, stdenv, cmake, pkg-config, libsodium, libopus, libvpx }:
stdenv.mkDerivation {
  pname = "c-toxcore";
  version = "checkout";
  src = lib.fileset.toSource {
    root = ../../c-toxcore;
    fileset = lib.fileset.gitTrackedWith { recurseSubmodules = true; } ../../c-toxcore;
  };
  nativeBuildInputs = [ cmake pkg-config ];
  buildInputs = [ libsodium libopus libvpx ];
  cmakeFlags = [
    # hTox shows only warnings and errors.
    "-DMIN_LOGGER_LEVEL=WARNING"
    "-DUNITTEST=OFF"
    "-DDHT_BOOTSTRAP=OFF"
    "-DBOOTSTRAP_DAEMON=OFF"
    "-DMUST_BUILD_TOXAV=ON"
    "-DCMAKE_POSITION_INDEPENDENT_CODE=ON"
    # Its toxcore.pc puts these after the prefix.
    "-DCMAKE_INSTALL_LIBDIR=lib"
    "-DCMAKE_INSTALL_INCLUDEDIR=include"
  ];
}
