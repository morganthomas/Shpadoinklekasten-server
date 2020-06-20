nix-build --arg isJS false                                          --no-out-link | cachix push shpadoinkle
nix-build --arg isJS true                                           --no-out-link | cachix push shpadoinkle
nix-build --arg isJS false --arg asShell true                       --no-out-link | cachix push shpadoinkle
nix-build --arg isJS true  --arg asShell true                       --no-out-link | cachix push shpadoinkle
nix-build --arg isJS false --arg asShell true --arg withHoogle true --no-out-link | cachix push shpadoinkle
