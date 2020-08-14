{
  description = "Package build and dev environment for lastpass-tui";
  outputs = { self, nixpkgs }: {
    # setup derivation for x86_64-linux
    defaultPackage.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
      haskell.packages.ghc884.callCabal2nix "lastpass-tui" self {};
    
    # setup devShell for x86_64-linux.
    devShell.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
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
