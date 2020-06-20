{ chan ? "e1843646b04fb564abf6330a9432a76df3269d2f"
, compiler ? "ghc864"
, withHoogle ? false
, doHoogle ? false
, doHaddock ? false
, enableLibraryProfiling ? false
, enableExecutableProfiling ? false
, strictDeps ? false
, isJS ? false
, asShell ? false
}:
let


  # It's a shpadoinkle day
  shpadoinkle = builtins.fetchGit {
    url    = https://gitlab.com/fresheyeball/Shpadoinkle.git;
    rev    = "77a56850ab7897f28b107a729cff893e74dd15bd";
    ref    = "master";
  };


  Shpadoinklekasten-lib-src = builtins.fetchGit {
    url    = ssh://git@github.com/morganthomas/Shpadoinklekasten-lib.git;
    rev    = "a165206186da84fb4fe87a36aaedcc190b96a274";
    ref    = "master";
  };


  # Additional ignore patterns to keep the Nix src clean
  ignorance = [
    "*.md"
    "figlet"
    "*.nix"
    "*.sh"
    "*.yml"
  ];


  # Get some utilities
  inherit (import (shpadoinkle + "/nix/util.nix") { inherit compiler isJS; }) compilerjs gitignore;


  # Build faster by doing less
  chill = p: (pkgs.haskell.lib.overrideCabal p {
    inherit enableLibraryProfiling enableExecutableProfiling;
  }).overrideAttrs (_: {
    inherit doHoogle doHaddock strictDeps;
  });


  # Overlay containing Shpadoinkle packages, and needed alterations for those packages
  # as well as optimizations from Reflex Platform
  shpadoinkle-overlay =
    import (shpadoinkle + "/nix/overlay.nix") { inherit compiler isJS; };


  # Haskell specific overlay (for you to extend)
  haskell-overlay = hself: hsuper: {
    aeson = hsuper.aeson;
    containers = hsuper.containers;
    exceptions = hsuper.exceptions;
    jsaddle = hsuper.jsaddle;
    mongoDB = hsuper.mongoDB;
    network = hsuper.network;
    Shpadoinkle-continuations = hsuper.Shpadoinkle-continuations;
    Shpadoinklekasten-lib = hself.callCabal2nix "Shpadoinklekasten-lib" "${Shpadoinklekasten-lib-src}" {};    transformers = hsuper.transformers;
    unliftio = hsuper.unliftio;
  };


  # Top level overlay (for you to extend)
  Shpadoinklekasten-server-overlay = self: super: {
    haskell = super.haskell //
      { packages = super.haskell.packages //
        { ${compilerjs} = super.haskell.packages.${compilerjs}.override (old: {
            overrides = super.lib.composeExtensions (old.overrides or (_: _: {})) haskell-overlay;
          });
        };
      };
    };


  # Complete package set with overlays applied
  pkgs = import
    (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${chan}.tar.gz";
    }) {
    overlays = [
      shpadoinkle-overlay
      Shpadoinklekasten-server-overlay
    ];
  };


  # We can name him George
  Shpadoinklekasten-server = pkgs.haskell.packages.${compilerjs}.callCabal2nix "Shpadoinklekasten-server" (gitignore ignorance ./.) {};


in with pkgs; with lib; with haskell.packages.${compiler};

  if inNixShell || asShell
  then shellFor {
    inherit withHoogle;
    packages = _: [Shpadoinklekasten-server];
    COMPILER = compilerjs;
    buildInputs = [ stylish-haskell cabal-install ghcid ];
    shellHook = ''
      ${lolcat}/bin/lolcat ${./figlet}
    '';
  } else chill Shpadoinklekasten-server
