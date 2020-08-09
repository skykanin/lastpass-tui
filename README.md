# Bitwarden TUI
A terminal user interface for bitwarden written in haskell

## Develop
- Install nix `curl -L https://nixos.org/nix/install | sh`
- Enable experimental features `mkdir /etc/nix && echo "experimental-features = nix-command flakes" > /etc/nix/nix.conf`
- `cd bitwarden-tui`
- `nix develop`

## Compile and run the project
`cabal run exe:bwt`
