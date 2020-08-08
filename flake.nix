{
  description = "Flake description";
  edition = 201909;
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
        libraries = [];
        libraryPath = "${makeLibraryPath libraries}";
      in
        pkgs.runCommand "shell" {
          buildInputs = tools ++ libraries;
          shellHook = ''
            export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${libraryPath}"
            export LIBRARY_PATH="${libraryPath}"
          '';
        } "";
  };
}
