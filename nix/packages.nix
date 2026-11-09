# Haskell packages nixpkgs has broken, too old or missing: hs-toxcore,
# hs-toxsync, hs-tox, hs-toxav and the msgpack packages from the workspace
# checkouts, and qrcode-core without version bounds.
pkgs: self: super:
let
  inherit (pkgs.lib) fileset;
  inherit (pkgs.haskell.lib) doJailbreak markUnbroken overrideCabal;

  # Only the library (tools and tests need things we don't, and some can't
  # be built for Android).
  libraryWith = args: name: dir: overrideCabal
    (self.callCabal2nix name
      (fileset.toSource {
        root = dir;
        fileset = fileset.unions [
          (dir + "/${name}.cabal")
          (dir + "/LICENSE")
          (dir + "/src")
          (fileset.maybeMissing (dir + "/cbits"))
          (fileset.maybeMissing (dir + "/native"))
        ];
      })
      args)
    (old: {
      postPatch = (old.postPatch or "") + ''
        sed -i '/^\(executable\|test-suite\|benchmark\) /,$d' ${name}.cabal
      '';
      executableHaskellDepends = [ ];
      testHaskellDepends = [ ];
      benchmarkHaskellDepends = [ ];
      doCheck = false;
      doHaddock = false;
      enableLibraryProfiling = false;
    });
  library = libraryWith { };
in
{
  msgpack-types = library "msgpack-types" ../../hs-msgpack-types;
  msgpack-binary = library "msgpack-binary" ../../hs-msgpack-binary;
  msgpack-arbitrary = library "msgpack-arbitrary" ../../hs-msgpack-arbitrary;
  toxcore = doJailbreak (library "toxcore" ../../hs-toxcore);
  toxsync = library "toxsync" ../../hs-toxsync;
  tox = library "tox" ../../hs-tox;
  toxav = libraryWith
    {
      libavcodec = pkgs.ffmpeg;
      ffmpeg = pkgs.ffmpeg;
      opus = pkgs.libopus;
    } "toxav" ../../hs-toxav;
  # hs-toxav on Android links Android's MediaCodec library, which its NDK
  # brings.
  mediandk = null;
  qrcode-core = doJailbreak (markUnbroken super.qrcode-core);
}
