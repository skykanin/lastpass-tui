# LastPass TUI
[![GitHub issues by-label](https://img.shields.io/github/workflow/status/skykanin/lastpass-tui/Haskell%20CI?logo=GitHub&style=for-the-badge)](https://github.com/skykanin/lastpass-tui/actions?query=workflow%3A%22Haskell+CI%22)


A terminal user interface for LastPass written in Haskell.

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

This adds everything listed in the `flake.nix` files `devShell` attribute to your path.
### Run
On the first run you will be asked to `cabal update`.

After that you can run the project with
`cabal run`

### Build
To build the project using cabal you can run
`cabal build [...opts]`
However if you want to build the executable through nix you can run
`nix-build -A lastpass-tui.components.exes.lpt`
The resulting executable will be under `./result/bin/lpt`

### Test
To run the tests you first need to provide a test account. The template is given in
`misc/testUserTemplate.json`. The actual json filename is the same, but without the
"template" suffix. Now simply run
`cabal test`
