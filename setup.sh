#!/bin/bash
echo -e "== INSTALLING NIX =="
curl -L https://nixos.org/nix/install | sh
[[ $SHELL == "/bin/fish" ]] && fish -c set -U fish_user_paths ~/.nix-profile/bin $fish_user_paths || . ~/.nix-profile/etc/profile.d/nix.sh
echo -e "\n== ENABLING FLAKES ==\n"
~/.nix-profile/bin/nix-env -iA nixpkgs.nixFlakes
mkdir ~/.config/nix && echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf
echo -e "\n== ENTERING DEV ENV =="
~/.nix-profile/bin/nix develop
