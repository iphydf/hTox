# hTox on the desktop: `nix-build desktop`, then `result/bin/htox`. The
# packages it's built from are in passthru, for shell.nix.
let
  hatter = import ../nix/hatter.nix;
  inherit (hatter) pkgs;
  inherit (pkgs.haskell.lib) dontCheck dontHaddock disableLibraryProfiling;
  # Like the libraries packages.nix builds.
  plainly = p: disableLibraryProfiling (dontHaddock (dontCheck p));
  host = pkgs.callPackage "${hatter.qt}/host.nix" { include = "${hatter.src}/include"; };
  # The program and the icons its data-dir has, from Android's resources.
  src = pkgs.lib.fileset.toSource {
    root = ../.;
    fileset = pkgs.lib.fileset.unions [ ./htox-desktop.cabal ./Main.hs ../platform/res/drawable ];
  };
  hpkgs = (import "${hatter.src}/nix/hpkgs.nix" { inherit pkgs; }).extend
    (pkgs.lib.composeExtensions (import ../nix/packages.nix pkgs) (self: super: {
      hatter = dontHaddock (dontCheck super.hatter-project);
      direct-sqlite = import ../nix/sqlcipher.nix { inherit pkgs; inherit (pkgs) openssl; link = true; } super.direct-sqlite;
      # Not wrapped: it's a library, and Qt's paths go around the program. It
      # passes on only the host's own pkg-config modules, as in Hatter's
      # hpkgs.nix.
      hatter-qt = (pkgs.haskell.lib.overrideCabal
        (plainly (self.callCabal2nix "hatter-qt" (pkgs.lib.cleanSourceWith { src = hatter.qt; filter = path: _: baseNameOf path != "dist-newstyle"; }) { hatter-qt-host = host; }))
        { __onlyPropagateKnownPkgConfigModules = true; })
        .overrideAttrs { dontWrapQtApps = true; };
      htox = plainly (self.callCabal2nix "htox" (hatter.app pkgs) { });
      htox-desktop = pkgs.haskell.lib.overrideCabal
        (self.callCabal2nix "htox-desktop" (pkgs.lib.fileset.toSource { root = ./.; fileset = ./htox-desktop.cabal; }) { })
        (_: { inherit src; postUnpack = "sourceRoot+=/desktop"; });
    }));
  qt = pkgs.qt6;
in
(pkgs.haskell.lib.justStaticExecutables hpkgs.htox-desktop).overrideAttrs (old: {
  # Qt's plugins and QML modules, where the program finds them.
  nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ qt.wrapQtAppsHook ];
  buildInputs = (old.buildInputs or [ ]) ++ [ qt.qtbase qt.qtdeclarative qt.qtmultimedia qt.qtsvg ];
  passthru = (old.passthru or { }) // { inherit pkgs hpkgs host qt; };
})
