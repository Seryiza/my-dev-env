# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, nixpkgs-unstable, lib, zen-browser, rep, ... }@inputs:
let unstable-pkgs = nixpkgs-unstable.legacyPackages."x86_64-linux";
in {
  nixpkgs.config.permittedInsecurePackages = [ ];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # >= 6.15 (audio, wifi)
  boot.kernelPackages = pkgs.linuxKernel.packages.linux_6_18;

  boot.kernelParams = [ ];
  boot.initrd.kernelModules = [ "amdgpu" ];
  boot.supportedFilesystems = [ "ntfs" ];

  hardware.cpu.amd.updateMicrocode = true;
  hardware.enableRedistributableFirmware = true;
  services.fwupd.enable = true;

  nixpkgs.overlays = [
    (final: prev: {
      jdk = pkgs.zulu21;
      clojure = prev.clojure.override { jdk = pkgs.zulu21; };
    })
  ];

  imports = [ # Include the results of the hardware scan.
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
  networking.networkmanager.wifi.backend = "iwd";
  services.nscd.enableNsncd = true;

  systemd.tmpfiles.rules =
    [ "L+    /opt/rocm/hip   -    -    -     -    ${pkgs.rocmPackages.clr}" ];

  time.timeZone = "Asia/Bishkek";
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.supportedLocales = [
    "all"
    #"C.UTF-8/UTT-8"
    #"en_US.UTF-8/UTF-8"
    #"ru_RU.UTF-8/UTF-8"
  ];

  services.xserver.enable = true;
  services.xserver.videoDrivers = [ "amdgpu" "modesetting" "fbdev" ];
  services.xserver.displayManager.lightdm.enable = false;
  services.xserver.displayManager.lightdm.greeters.gtk.enable = false;

  services.greetd = {
    enable = true;
    settings = rec {
      initial_session = {
        command = "${pkgs.sway}/bin/sway";
        user = "seryiza";
      };
      default_session = initial_session;
    };
  };

  programs.sway = { enable = true; };
  programs.gnupg.agent.enable = true;

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    nix-direnv.enable = true;
  };

  services = { dbus = { enable = true; }; };

  qt = {
    enable = true;
    style = "adwaita";
    platformTheme = null;
  };

  environment.pathsToLink =
    [ "/share/xdg-desktop-portal" "/share/applications" ];

  services.xserver = {
    dpi = 180;
    xkb = {
      variant = "";
      layout = "us";
    };
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  services.emacs = {
    enable = true;
    package = (pkgs.emacsPackagesFor pkgs.emacs-pgtk).emacsWithPackages
      (epkgs: [ epkgs.mu4e epkgs.melpaPackages.telega ]);
  };

  environment.localBinInPath = true;

  # Samba
  services.samba = { enable = true; };

  services.gvfs.enable = true;

  networking.wireless.iwd.enable = true;
  networking.wireless.iwd.settings = {
    General = {
      RoamThreshold = -75;
      RoamThreshold5G = -80;
      RoamRetryInterval = 20;
    };
  };

  # Enable sound with pipewire.
  # sound.enable = false;
  services.pulseaudio.enable = false;

  security.rtkit.enable = true;
  security.polkit.enable = true;
  services.seatd.enable = true;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # sudo
  security.sudo.extraConfig = ''
    Defaults timestamp_timeout=-1
  '';

  services.clamav = {
    daemon.enable = true;
    updater.enable = false;
    updater.interval = "yearly";
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

  hardware.uinput.enable = true;
  services.udev = {
    # NOTE: Xremap requires the following:
    # https://github.com/xremap/xremap?tab=readme-ov-file#running-xremap-without-sudo
    extraRules = ''
      KERNEL=="uinput", GROUP="input", TAG+="uaccess"
    '';
  };

  services.xremap = {
    enable = true;
    serviceMode = "user";
    withWlroots = true;
    userName = "seryiza";
    watch = true;
    debug = false;

    config.modmap = [{
      name = "Global";
      remap = {
        "CapsLock" = "Control_L";
        "KEY_F23" = "Control_R";
      };
    }];

    config.keymap = [{
      name = "Escape on Ctrl+[";
      remap = { "C-KEY_LEFTBRACE" = "Esc"; };
    }];
  };

  virtualisation.docker.enable = true;
  virtualisation.docker.rootless = {
    enable = true;
    setSocketVariable = true;
  };

  fonts.packages = [ ] ++ builtins.filter lib.attrsets.isDerivation
    (builtins.attrValues pkgs.nerd-fonts);

  services.flatpak.enable = true;
  services.gnome.gnome-keyring.enable = true;
  services.gnome.gcr-ssh-agent.enable = false;

  users.users.seryiza = {
    isNormalUser = true;
    description = "Sergey Zaborovsky";
    extraGroups = [
      "audio"
      "video"
      "render"
      "seat"
      "input"
      "uinput"
      "networkmanager"
      "wheel"
      "docker"
      "tty"
      "sway"
    ];
    packages = [
      pkgs.firefox
      pkgs.google-chrome
      pkgs.kubectl
      pkgs.dropbox
      pkgs.bun
      pkgs.telegram-desktop
      pkgs.enpass
      pkgs.google-cloud-sdk
      pkgs.postgresql_17
      pkgs.onlyoffice-desktopeditors
      pkgs.gh
      pkgs.evince
      pkgs.inkscape
      pkgs.comfortaa
      pkgs.less
      pkgs.dbeaver-bin
      pkgs.gparted
      pkgs.p7zip
      pkgs.unrar
      pkgs.hyprpicker
      pkgs.gimp
      pkgs.wireguard-tools
      pkgs.python3
      pkgs.python312Packages.pip
      pkgs.wf-recorder
      pkgs.ffmpeg-full
      pkgs.video-trimmer
      pkgs.libvterm
      pkgs.cmake
      pkgs.libtool
      pkgs.polylith
      pkgs.cifs-utils
      pkgs.celluloid
      pkgs.terraform
      pkgs.sops
      pkgs.uxn
      pkgs.leiningen
      pkgs.act
      pkgs.appimage-run
      pkgs.lsb-release
      pkgs.dmidecode
      pkgs.jq
      pkgs.qbittorrent
      pkgs.rclone
      pkgs.wl-clipboard
      pkgs.grim
      pkgs.slurp
      pkgs.silver-searcher
      unstable-pkgs.clj-kondo
      unstable-pkgs.cljfmt
      pkgs.ntfs3g
      pkgs.libnotify
      pkgs.iw
      pkgs.brightnessctl
      pkgs.drawing
      pkgs.xfce.thunar
      pkgs.clojure-lsp
      pkgs.lua-language-server
      pkgs.nil
      pkgs.ripgrep
      pkgs.jdk
      pkgs.zprint
      pkgs.geckodriver
      pkgs.amberol
      pkgs.tuigreet
      pkgs.gcc
      pkgs.nodejs_22
      pkgs.unzip
      pkgs.clojure
      pkgs.babashka
      pkgs.emacs-lsp-booster
      pkgs.zed-editor-fhs
      pkgs.htop
      pkgs.evtest
      pkgs.chntpw
      pkgs.nixfmt-classic
      zen-browser.packages."x86_64-linux".default
      rep.packages.${pkgs.stdenv.hostPlatform.system}.default
      pkgs.wev
    ];
  };

  environment.systemPackages = with pkgs; [ glibcLocales busybox iputils ];

  environment.variables = {
    LIBVA_DRIVER_NAME = "iHD";
    QT_QPA_PLATFORM = "wayland";
  };

  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
      libvdpau-va-gl
      libva-vdpau-driver
      rocmPackages.clr
      rocmPackages.clr.icd
    ];
  };

  hardware.amdgpu.initrd.enable = true;

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };

  services.blueman.enable = true;

  zramSwap = {
    enable = true;
    memoryPercent = 200;
  };

  boot.kernel.sysctl = {
    "vm.swappiness" = 100;
    "vm.page-cluster" = 0;
  };

  services.openssh.enable = true;
  programs.ssh = { startAgent = true; };
  networking.firewall.enable = false;

  system.stateVersion = "23.11";
}
