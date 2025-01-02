{
  description = "Seryiza's NixOS flake";

  inputs = {
    # NixOS official package source, using the nixos-23.11 branch here
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";

    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
      # The `follows` keyword in inputs is used for inheritance.
      # Here, `inputs.nixpkgs` of home-manager is kept consistent with
      # the `inputs.nixpkgs` of the current flake,
      # to avoid problems caused by different versions of nixpkgs.
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
    };

    emacs-lsp-booster.url = "github:slotThe/emacs-lsp-booster-flake";
  };

  outputs = { self, nixpkgs, home-manager, nur, emacs-lsp-booster, ... }@inputs: {
    nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = inputs;
      modules = [
        {
          nixpkgs.overlays = [
            nur.overlays.default
            inputs.emacs-overlay.overlay
            emacs-lsp-booster.overlays.default
          ];
        }

	      ({ pkgs, ... }:
	      let
	        nur-no-pkgs = import nur {
            nurpkgs = import nixpkgs { system = "x86_64-linux"; };
          };
	      in {
          imports = [
            #nur-no-pkgs.repos.dukzcry.modules.logitech-k380
          ];
	      })

        # Import the previous configuration.nix we used,
        # so the old configuration file still takes effect
        ./configuration.nix

	      home-manager.nixosModules.home-manager
        {
	        home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
	        home-manager.users.seryiza = import ./home.nix;
        }

        nur.modules.nixos.default
      ];
    };
  };
}
