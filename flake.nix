{
  description = "Seryiza's NixOS flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = { url = "github:nix-community/emacs-overlay"; };

    emacs-lsp-booster.url = "github:slotThe/emacs-lsp-booster-flake";

    xremap = { url = "github:xremap/nix-flake"; };

    zen-browser = { url = "github:0xc000022070/zen-browser-flake"; };

    bzmenu.url = "github:e-tho/bzmenu";
    iwmenu.url = "github:e-tho/iwmenu";
    rep.url = "github:eraserhd/rep";
    llm-agents.url = "github:numtide/llm-agents.nix";
  };

  outputs = { self, nixpkgs, home-manager, nur, emacs-lsp-booster, xremap, ...
    }@inputs: {
      nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = inputs;
        modules = [
          {
            nixpkgs.overlays =
              [ nur.overlays.default emacs-lsp-booster.overlays.default ];
          }

          xremap.nixosModules.default

          ./configuration.nix

          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.seryiza = import ./home.nix;
            home-manager.extraSpecialArgs = inputs;
          }

          nur.modules.nixos.default
        ];
      };
    };
}
