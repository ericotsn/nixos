{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      emacs-overlay,
    }:
    let
      overlays = [
        emacs-overlay.overlays.default
      ];
    in
    {
      formatter.aarch64-linux = nixpkgs.legacyPackages.aarch64-linux.nixfmt-tree;

      nixosConfigurations.vm-aarch64 = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [
          { nixpkgs.overlays = overlays; }

          ./hosts/vm-aarch64.nix
          ./users/eotsn.nix
        ];
      };
    };
}
