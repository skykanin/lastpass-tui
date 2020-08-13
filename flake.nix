{
  description = "Dev environment for lastpass-tui";
  outputs = { self, nixpkgs }: {
    # setup the devShell for x86_64-linux.
    devShell.x86_64-linux =
      with nixpkgs.legacyPackages.x86_64-linux;
      let
        inherit (lib) makeLibraryPath;
        hs = haskell.packages.ghc884;
        tools = [
          hs.brittany
          hs.ghc
          hs.cabal-install
          hs.ghcid
          binutils-unwrapped
        ];
        libraries = [
          lastpass-cli
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
