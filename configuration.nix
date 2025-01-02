# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:
let
  my-obsidian = pkgs.symlinkJoin {
    name = "obsidian";
    paths = [ pkgs.obsidian ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/obsidian \
      --add-flags "--disable-features=WaylandFractionalScaleV1"
    '';
  };
in {
  nixpkgs.config.permittedInsecurePackages = [
    #"electron-25.9.0"
    #"electron-24.8.6"
    "electron-27.3.11"
  ];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # TODO: delete it after sound fix
  # https://github.com/NixOS/nixpkgs/issues/330685
  #boot.kernelPackages = pkgs.linuxKernel.packages.linux_6_10;

  nixpkgs.overlays = [(final: prev: {
    jdk = pkgs.zulu21;
    clojure = prev.clojure.override {jdk = pkgs.zulu21;};
    #obsidian-wayland = prev.obsidian.override {electron = final.electron_24;};
  })];

  #hardware.logitech-k380.enable = true;

  imports =
    [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ];

    # Bootloader.
    # boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;
    boot.loader.efi.efiSysMountPoint = "/boot";

    # Flakes and other experimental features
    nix.settings.experimental-features = [ "nix-command" "flakes" ];

    boot.loader.grub.enable = true;
    boot.loader.grub.device = "nodev";
    boot.loader.grub.useOSProber = true;
    boot.loader.grub.efiSupport = true;

    networking.hostName = "nixos"; # Define your hostname.
    # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

    # Configure network proxy if necessary
    # networking.proxy.default = "http://user:password@proxy:port/";
    # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

    # Enable networking
    networking.networkmanager.enable = true;

    # Set your time zone.
    time.timeZone = "Asia/Bishkek";
    #time.timeZone = "America/New_York";

    # Select internationalisation properties.
    i18n.defaultLocale = "en_US.UTF-8";

    i18n.extraLocaleSettings = {
      LC_ADDRESS = "ru_RU.UTF-8";
      LC_IDENTIFICATION = "ru_RU.UTF-8";
      LC_MEASUREMENT = "ru_RU.UTF-8";
      LC_MONETARY = "ru_RU.UTF-8";
      LC_NAME = "ru_RU.UTF-8";
      LC_NUMERIC = "ru_RU.UTF-8";
      LC_PAPER = "ru_RU.UTF-8";
      LC_TELEPHONE = "ru_RU.UTF-8";
      LC_TIME = "ru_RU.UTF-8";
    };

    # Enable the X11 windowing system.
    services.xserver.enable = true;

    services.xserver.displayManager.lightdm.enable = false;
    services.xserver.displayManager.lightdm.greeters.gtk.enable = false;

    services.xserver.displayManager.gdm.enable = true;
    services.xserver.displayManager.gdm.wayland = true;
    services.xserver.displayManager.gdm.debug = false;

    # Enable the GNOME Desktop Environment.
    services.xserver.desktopManager.gnome.enable = true;
    services.xserver.desktopManager.gnome.debug = false;
    qt = {
      enable = true;
      style = "adwaita";
      platformTheme = "gnome";
    };

    environment.pathsToLink = [ "/share/xdg-desktop-portal" "/share/applications" ];

    # Configure keymap in X11
    services.xserver = {
      xkb = {
        variant = "";
        layout = "us";
      };
    };

    # Enable CUPS to print documents.
    services.printing.enable = true;

    # Samba
    services.samba = {
      enable = true;
    };

    services.gvfs.enable = true;

    # Enable sound with pipewire.
    # sound.enable = false;
    hardware.pulseaudio.enable = false;

    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      # If you want to use JACK applications, uncomment this
      #jack.enable = true;

      # use the example session manager (no others are packaged yet so this is enabled by default,
      # no need to redefine it in your config for now)
      #media-session.enable = true;
    };

    # sudo
    security.sudo.extraConfig = ''
      Defaults timestamp_timeout=-1
    '';

    services.clamav = {
      daemon.enable = true;
      updater.enable = true;
    };

    programs.dconf.enable = true;
    programs.autojump.enable = true;

    programs.nix-ld.enable = true;

    programs.appimage = {
      enable = true;
      binfmt = true;
    };

    # Enable touchpad support (enabled default in most desktopManager).
    # services.xserver.libinput.enable = true;

    services.input-remapper.enable = true;

    virtualisation.docker.enable = true;
    virtualisation.docker.rootless = {
      enable = true;
      setSocketVariable = true;
    };

    fonts.packages = [
      pkgs.nerdfonts
    ];

    services.flatpak.enable = true;

    # Define a user account. Don't forget to set a password with ‘passwd’.
    users.users.seryiza = {
      isNormalUser = true;
      description = "Sergey Zaborovsky";
      extraGroups = [ "audio" "input" "networkmanager" "wheel" "docker" ];
      packages = [
        #pkgs.firefox
        pkgs.firefox-bin
        pkgs.google-chrome
        pkgs.kubectl
        pkgs.telegram-desktop
        pkgs.enpass
        pkgs.google-cloud-sdk
        pkgs.postgresql
        pkgs.onlyoffice-bin
        pkgs.masterpdfeditor
        pkgs.gh
        pkgs.monaspace
        pkgs.inkscape
        pkgs.comfortaa
        pkgs.less
        pkgs.delta
        pkgs.planify
        pkgs.dbeaver-bin
        pkgs.p7zip
        pkgs.unrar
        pkgs.anki-bin
        pkgs.gimp
        pkgs.gtk4-layer-shell
        pkgs.walker
        pkgs.wireguard-tools
        pkgs.tangram
        pkgs.python3
        pkgs.logseq
        pkgs.libvterm
        pkgs.cmake
        pkgs.libtool
        pkgs.polylith
        pkgs.pdfstudio2022
        pkgs.cifs-utils
        pkgs.impression
        pkgs.celluloid
        pkgs.terraform
        pkgs.sops
        pkgs.steam-run
        pkgs.uxn
        pkgs.leiningen
        pkgs.act
        pkgs.appimage-run 
        pkgs.lsb-release
        pkgs.dmidecode
        pkgs.monocraft
        pkgs.qbittorrent

        # gnome
        pkgs.gnome-tweaks
        pkgs.dconf-editor
        pkgs.gnome-terminal
        pkgs.clojure-lsp
        pkgs.lua-language-server
        pkgs.nil
        pkgs.ripgrep
        pkgs.jdk
        pkgs.zprint
        pkgs.geckodriver

        pkgs.dconf2nix
        pkgs.gcc
        pkgs.nodejs_22
        pkgs.unzip
        pkgs.clojure
        pkgs.babashka
        pkgs.emacs-lsp-booster

        pkgs.htop
        #pkgs.obsidian-wayland
        #pkgs.obsidian
        pkgs.transmission_4-gtk
        pkgs.evtest
        pkgs.ulauncher
        pkgs.chntpw
        #my-obsidian

        #  thunderbird
      ];
    };

    # List packages installed in system profile. To search, run:
    # $ nix search wget
    environment.systemPackages = with pkgs; [
      #  vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
      #  wget
    ];

    environment.variables = {
      XCURSOR_SIZE = "40";
      LIBVA_DRIVER_NAME = "iHD";
      #GDK_SCALE = "2";
      #GDK_DPI_SCALE = "0.5";
      #QT_AUTO_SCREEN_SCALE_FACTOR = "1";
    };

    environment.sessionVariables.NIXOS_OZONE_WL = "1";

    hardware.graphics = {
      enable = true;
      extraPackages = [
        pkgs.intel-media-driver
        pkgs.intel-vaapi-driver
        pkgs.libvdpau-va-gl
      ];
    };

    hardware.bluetooth = {
      enable = true;
      powerOnBoot = true;
    };

    zramSwap = {
      enable = true;
      memoryPercent = 200;
    };

    boot.kernel.sysctl = {
      "vm.swappiness" = 180;
      "vm.page-cluster" = 0;
    };


    # Some programs need SUID wrappers, can be configured further or are
    # started in user sessions.
    # programs.mtr.enable = true;
    # programs.gnupg.agent = {
      #   enable = true;
      #   enableSSHSupport = true;
      # };

      # List services that you want to enable:

      # Enable the OpenSSH daemon.
      # services.openssh.enable = true;

      # Open ports in the firewall.
      # networking.firewall.allowedTCPPorts = [ ... ];
      # networking.firewall.allowedUDPPorts = [ ... ];
      # Or disable the firewall altogether.
      networking.firewall.enable = false;

      # This value determines the NixOS release from which the default
      # settings for stateful data, like file locations and database versions
      # on your system were taken. It‘s perfectly fine and recommended to leave
      # this value at the release version of the first install of this system.
      # Before changing this value read the documentation for this option
      # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
      system.stateVersion = "23.11"; # Did you read the comment?

}
