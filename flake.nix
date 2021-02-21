{
  description = "Package build and dev environment for lastpass-tui";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix/master";
  outputs = { self, haskellNix, nixpkgs }: {
    # setup derivation for x86_64-linux
    defaultPackage.x86_64-linux =
      let hn = haskellNix.legacyPackages.x86_64-linux.haskell-nix;
          drv = hn.project {
            src = hn.haskellLib.cleanGit {
              name = "lastpass-tui";
              src = ./.;
            };
            compiler-nix-name = "ghc884";
          };
      in drv.lastpass-tui;
   
    # setup devShell for x86_64-linux.
    devShell.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
      let
        inherit (lib) makeLibraryPath;
        hs = haskell.packages.ghc884;
        tools = [
          binutils-unwrapped
          hs.ghc
          hs.cabal-install
          hs.ghcid
          hs.fourmolu
        ];
        libraries = [
          lastpass-cli
          xclip
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
