# The app's shared library for Android.
{ androidArch ? "aarch64" }:
let
  hatter = import ./hatter.nix;
  inherit (hatter) sources;
  lib = import "${hatter.src}/nix/lib.nix" { inherit sources androidArch; };
  hatterSrc = import "${hatter.src}/nix/hatter-src.nix" { inherit sources; };
  androidPkgs = (import sources.nixpkgs {
    config.allowUnfree = true;
    config.android_sdk.accept_license = true;
  }).pkgsCross."${androidArch}-android-prebuilt";
  # Linked into the app's library (Android has no libsodium).
  libsodium = androidPkgs.libsodium.overrideAttrs
    (old: {
      dontDisableStatic = true;
      configureFlags = (old.configureFlags or [ ]) ++ [ "--with-pic" ];
    });
  # Call codecs, static too. nixpkgs configures libvpx for Linux (that
  # toolchain can't link for Android) and adds -lpthread, which is in
  # Android's libc. Its Android target links its own programs with -static,
  # which we don't build.
  libvpx = androidPkgs.libvpx.overrideAttrs (old: {
    env = (old.env or { }) // { NIX_LDFLAGS = ""; };
    outputs = [ "out" "dev" ];
    postPatch = (old.postPatch or "") + ''
      sed -i 's/check_add_ldflags -static/true/' build/make/configure.sh
    '';
    configureFlags = map
      (f: {
        "--target=arm64-linux-gcc" = "--target=arm64-android-gcc";
        "--enable-shared" = "--disable-shared";
      }.${f} or f)
      old.configureFlags
    ++ [ "--disable-examples" "--disable-tools" "--disable-unit-tests" ];
  });
  libopus = androidPkgs.libopus.overrideAttrs (old: {
    mesonFlags = old.mesonFlags ++ [ "-Ddefault_library=static" "-Dextra-programs=disabled" "-Dtests=disabled" ];
  });
  ffmpeg = (androidPkgs.ffmpeg-headless.override {
    inherit libvpx;
    withHeadlessDeps = false;
    withSmallDeps = false;
    buildAvcodec = true;
    buildAvutil = true;
    withVpx = true;
    withStatic = true;
    withShared = false;
  }).overrideAttrs (old: {
    # nixpkgs says Linux.
    configureFlags = map (f: if f == "--target_os=linux" then "--target_os=android" else f) old.configureFlags
      ++ [ "--enable-pic" ];
  });
  # For SQLCipher. We only use libcrypto, and kernel TLS doesn't build for
  # Android.
  openssl = (androidPkgs.openssl.override { static = true; }).overrideAttrs (old: {
    configureFlags = (old.configureFlags or [ ]) ++ [ "no-ktls" "no-tests" ];
  });
  crossDeps = import "${hatter.src}/nix/cross-deps.nix" {
    inherit sources androidArch hatterSrc;
    # Without profiling, like Hatter (it would need Hatter's profiling
    # libraries otherwise).
    hpkgs = hatter.pkgs.lib.composeExtensions (import ./packages.nix hatter.pkgs)
      (self: super:
      let
        version = builtins.substring 0 8 (builtins.hashString "sha256" (toString [
          (hatter.app hatter.pkgs)
          ../platform
          hatter.src
          self.toxcore.drvPath
          self.toxsync.drvPath
          self.tox.drvPath
          self.toxav.drvPath
        ]));
      in {
        saltine = super.saltine.override { inherit libsodium; };
        direct-sqlite = import ./sqlcipher.nix { pkgs = hatter.pkgs; inherit openssl; } super.direct-sqlite;
        toxav = super.toxav.override {
          libavcodec = ffmpeg;
          inherit ffmpeg;
          opus = libopus;
        };
        htox = with hatter.pkgs.haskell.lib; disableLibraryProfiling (dontCheck (overrideCabal
          (self.callCabal2nix "htox" (hatter.app hatter.pkgs) { })
          (old: {
            postPatch = (old.postPatch or "") + ''
              substituteInPlace src/HTox/Version.hs --replace-fail '"dev"' '"${version}"'
            '';
          })));
      });
    # Only Main.hs is built outside the package.
    consumerCabal2Nix = { mkDerivation, base, lib, htox }:
      mkDerivation {
        pname = "htox-main";
        version = "0.1.0";
        libraryHaskellDepends = [ base htox ];
        license = lib.licenses.gpl3Only;
      };
  };
in
lib.mkAndroidLib {
  inherit hatterSrc crossDeps;
  pname = "htox";
  mainModule = ../app/Main.hs;
  javaPackageName = "chat.tox.hstox";
  # Each bubble and avatar is a few native views, more than the fixed pool.
  dynamicNodePool = true;
  # Tox runs on its own threads.
  threadedRts = true;
  # For the debug screen's heap and GC numbers, and its export's heap census
  # by type (only when asked: the interval is over a day).
  rtsOpts = "-T -hT -i100000 -po/data/data/chat.tox.hstox/files/heap";
  extraLinkObjects = [
    "${libsodium}/lib/libsodium.a"
    "${openssl.out}/lib/libcrypto.a"
    "${ffmpeg.lib or ffmpeg}/lib/libavcodec.a"
    "${ffmpeg.lib or ffmpeg}/lib/libavutil.a"
    "${libvpx}/lib/libvpx.a"
    "${libopus}/lib/libopus.a"
    "-lmediandk"
  ];
  # Hatter's sources are in the build directory, but we use it from the
  # package database.
  extraModuleCopy = ''
    rm -r Hatter.hs Hatter
  '';
}
