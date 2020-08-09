{
  description = "Dev environment for bitwarden-tui";
  outputs = { self, nixpkgs }: {
    # setup the devShell for x86_64-linux.
    devShell.x86_64-linux =
      with nixpkgs.legacyPackages.x86_64-linux;
      let
        inherit (lib) makeLibraryPath;
        hs = haskell.packages.ghc884;
        tools = [
          hs.ghc
          hs.cabal-install
          hs.ghcid
          binutils-unwrapped
        ];
        libraries = [
          bitwarden-cli
          zlib
        ];
        libraryPath = "${makeLibraryPath libraries}";
      in
        mkShell {
          buildInputs = tools ++ libraries;
          shellHook = ''
            export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${libraryPath}"
            export LIBRARY_PATH="${libraryPath}"
          '';
        };
  };
}
