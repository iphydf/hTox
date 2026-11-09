# hTox on the desktop: `nix-shell --run 'cabal run htox'`. Cabal builds
# Hatter's Qt platform from its checkout too.
let
  inherit (import ./.) pkgs hpkgs host qt;
in
hpkgs.shellFor {
  packages = ps: [ ps.htox-desktop ps.hatter-qt ];
  nativeBuildInputs = [
    pkgs.cabal-install
    pkgs.pkg-config
    # A display to run it on and look at it.
    pkgs.xorg-server
    pkgs.xdotool
    pkgs.imagemagick
  ];
  QT_PLUGIN_PATH = "${qt.qtbase}/${qt.qtbase.qtPluginPrefix}:${qt.qtmultimedia}/${qt.qtbase.qtPluginPrefix}";
  QML_IMPORT_PATH = "${qt.qtdeclarative}/${qt.qtbase.qtQmlPrefix}:${qt.qtmultimedia}/${qt.qtbase.qtQmlPrefix}";
  FONTCONFIG_FILE = pkgs.makeFontsConf { fontDirectories = [ pkgs.dejavu_fonts pkgs.noto-fonts-color-emoji ]; };
  # Cabal keeps what pkg-config said when it configured hatter-qt, so it's
  # configured again when the host is another build. Nix builds htox under
  # the same unit ID whatever its source, so we link again when it changes.
  #
  # Qt's closure repeats many flags, and gcc hands all of them to the linker
  # in one variable, which may be at most 128 KiB. A flag with its argument
  # counts as one, and the first stays.
  shellHook = ''
    dedupe() {
      printf '%s\n' $1 | awk '
        /^-(isystem|idirafter|rpath)$/ && !flag { flag = $0; next }
        { if (flag) { $0 = flag " " $0; flag = "" }
          if (!seen[$0]++) out = out (out == "" ? "" : " ") $0 }
        END { print out }'
    }
    export NIX_CFLAGS_COMPILE="$(dedupe "$NIX_CFLAGS_COMPILE")" NIX_LDFLAGS="$(dedupe "$NIX_LDFLAGS")"
    unset -f dedupe
    if [ "$(cat dist-newstyle/hatter-qt-host 2>/dev/null)" != ${host} ]; then
      rm -rf dist-newstyle/build/*/*/hatter-qt-*
      mkdir -p dist-newstyle && echo ${host} > dist-newstyle/hatter-qt-host
    fi
    if [ "$(cat dist-newstyle/htox 2>/dev/null)" != ${hpkgs.htox} ]; then
      rm -rf dist-newstyle/build/*/*/htox-desktop-*
      mkdir -p dist-newstyle && echo ${hpkgs.htox} > dist-newstyle/htox
    fi
  '';
}
