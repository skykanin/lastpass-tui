# LastPass TUI
A terminal user interface for LastPass written in haskell

## Develop
### Prerequisites
Either run the `setup.sh` script or follow these steps manually:
- Install nix `curl -L https://nixos.org/nix/install | sh`
- If you use `fish-shell` you have to manually add `~/.nix-profile/bin/` to your path:
  - `set -U fish_user_paths ~/.nix-profile/bin $fish_user_paths`
- Otherwise do as told by the nix installation: 
  - `. ~/.nix-profile/etc/profile.d/nix.sh`
- Enable experimental features 
  - `nix-env -iA nixpkgs.nixFlakes`
  - `mkdir ~/.config/nix && echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf`

### Start contributing!
Run `nix develop` to enter the project environment.

This adds everything listed in the `flake.nix` file to your path.
### Run
On the first run you will be asked to `cabal update`.

After that you can run the project with
`cabal run`

### Build
`cabal build [...opts]`
