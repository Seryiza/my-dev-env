# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, nixpkgs-unstable, lib, zen-browser, rep, ... }@inputs:
let
  unstable-pkgs = nixpkgs-unstable.legacyPackages."x86_64-linux";
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
  nixpkgs.config.permittedInsecurePackages = [ ];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # >= 6.15 (audio, wifi)
  boot.kernelPackages = pkgs.linuxKernel.packages.linux_6_18;

  boot.kernelParams = [
    #"i915.enable_dpcd_backlight=1"
    #"nvidia.NVreg_EnableBacklightHandler=0"
    #"nvidia.NVreg_RegistryDwords=EnableBrightnessControl=0"
    #"amd_pstate=guided"
    #"disable_aspm=1"
    #"amdgpu"
    #"snd-intel-dspcfg.dsp_driver=1"
    "pci-stub.ids=10de:28e0,10de:22be"
  ];

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

  # Set your time zone.
  time.timeZone = "Asia/Bishkek";
  #time.timeZone = "America/Denver";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  #  i18n.extraLocaleSettings = {
  #    LC_ADDRESS = "ru_RU.UTF-8";
  #    LC_IDENTIFICATION = "ru_RU.UTF-8";
  #    LC_MEASUREMENT = "ru_RU.UTF-8";
  #    LC_MONETARY = "ru_RU.UTF-8";
  #    LC_NAME = "ru_RU.UTF-8";
  #    LC_NUMERIC = "ru_RU.UTF-8";
  #    LC_PAPER = "ru_RU.UTF-8";
  #    LC_TELEPHONE = "ru_RU.UTF-8";
  #    LC_TIME = "ru_RU.UTF-8";
  #  };

  i18n.supportedLocales = [
    "all"
    #"C.UTF-8/UTT-8"
    #"en_US.UTF-8/UTF-8"
    #"ru_RU.UTF-8/UTF-8"
  ];

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.videoDrivers = [ "amdgpu" "modesetting" "fbdev" ];

  services.xserver.displayManager.lightdm.enable = false;
  services.xserver.displayManager.lightdm.greeters.gtk.enable = false;

  services.displayManager.gdm.enable = false;
  services.displayManager.gdm.wayland = true;
  services.displayManager.gdm.debug = false;

  services.displayManager.ly.enable = false;

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
  # programs.swaylock.enable = true;

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    nix-direnv.enable = true;
  };

  services = { dbus = { enable = true; }; };

  # Enable the GNOME Desktop Environment.
  services.desktopManager.gnome.enable = true;
  services.desktopManager.gnome.debug = false;
  qt = {
    enable = true;
    style = "adwaita";
    platformTheme = null;
  };

  environment.pathsToLink =
    [ "/share/xdg-desktop-portal" "/share/applications" ];

  # Configure keymap in X11
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
    # package = pkgs.emacs-pgtk;
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

  services.input-remapper.enable = false;

  hardware.uinput.enable = true;
  services.udev = {
    # NOTE: Xremap requires the following:
    # https://github.com/xremap/xremap?tab=readme-ov-file#running-xremap-without-sudo
    extraRules = ''
      KERNEL=="uinput", GROUP="input", TAG+="uaccess"
    '';
  };

  services.asusd.enable = false;

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

  # Outdated:
  #fonts.packages = [ pkgs.nerdfonts ];

  services.flatpak.enable = true;
  services.gnome.gnome-keyring.enable = true;
  services.gnome.gcr-ssh-agent.enable = false;

  # Define a user account. Don't forget to set a password with ‘passwd’.
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
      #pkgs.firefox-bin
      pkgs.google-chrome
      pkgs.kubectl
      pkgs.dropbox
      pkgs.bun
      pkgs.telegram-desktop
      pkgs.enpass
      pkgs.google-cloud-sdk
      pkgs.postgresql_17
      pkgs.onlyoffice-desktopeditors
      pkgs.masterpdfeditor
      pkgs.gh
      pkgs.evince
      pkgs.monaspace
      pkgs.inkscape
      pkgs.comfortaa
      pkgs.less
      pkgs.delta
      #pkgs.cinny-desktop
      #pkgs.planify
      pkgs.codex
      pkgs.dbeaver-bin
      pkgs.gparted
      pkgs.p7zip
      pkgs.unrar
      pkgs.anki-bin
      pkgs.hyprpicker
      pkgs.gimp
      pkgs.gtk4-layer-shell
      pkgs.wireguard-tools
      pkgs.tangram
      pkgs.python3
      pkgs.python312Packages.pip
      pkgs.kooha
      pkgs.wf-recorder
      pkgs.ffmpeg-full
      pkgs.video-trimmer
      pkgs.kdePackages.kdenlive
      pkgs.footage
      #pkgs.logseq
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
      pkgs.jq
      pkgs.qbittorrent
      pkgs.rclone
      pkgs.wl-clipboard
      pkgs.grim # screenshot functionality
      pkgs.slurp # screenshot functionality
      pkgs.silver-searcher
      unstable-pkgs.clj-kondo
      unstable-pkgs.cljfmt
      pkgs.mediawriter
      pkgs.easyeffects
      pkgs.ntfs3g
      pkgs.libnotify
      #pkgs.linux-firmware
      pkgs.iw
      inputs.bzmenu.packages.${pkgs.stdenv.hostPlatform.system}.default
      inputs.iwmenu.packages.${pkgs.stdenv.hostPlatform.system}.default
      pkgs.brightnessctl
      pkgs.drawing
      #pkgs.pass
      #pkgs.qtpass
      pkgs.xfce.thunar

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
      pkgs.amberol
      pkgs.tuigreet

      pkgs.dconf2nix
      pkgs.gcc
      pkgs.nodejs_22
      pkgs.unzip
      pkgs.clojure
      pkgs.babashka
      pkgs.emacs-lsp-booster
      pkgs.code-cursor
      #pkgs.zed-editor
      pkgs.zed-editor-fhs

      pkgs.htop
      #pkgs.obsidian-wayland
      #pkgs.obsidian
      pkgs.transmission_4-gtk
      pkgs.evtest
      pkgs.ulauncher
      pkgs.chntpw
      pkgs.nixfmt-classic
      zen-browser.packages."x86_64-linux".default
      rep.packages.${pkgs.stdenv.hostPlatform.system}.default
      pkgs.wev
      #my-obsidian

      #  thunderbird
    ];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    #  vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    #  wget
    glibcLocales

    busybox
    iputils
  ];

  environment.variables = {
    LIBVA_DRIVER_NAME = "iHD";
    #GDK_SCALE = "2";
    #GDK_DPI_SCALE = "0.5";
    #QT_AUTO_SCREEN_SCALE_FACTOR = "1";
    QT_QPA_PLATFORM = "wayland";
  };

  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  hardware.graphics = {
    enable = true;
    #extraPackages = [ pkgs.intel-media-driver pkgs.intel-vaapi-driver pkgs.libvdpau-va-gl ];
    extraPackages = with pkgs; [
      libvdpau-va-gl
      libva-vdpau-driver
      rocmPackages.clr
      rocmPackages.clr.icd
    ];
    #extraPackages32 = with pkgs; [ driversi686Linux.amdvlk ];
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

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  services.openssh.enable = true;
  programs.ssh = { startAgent = true; };

  #networking.networkmanager.dns = "systemd-resolved";
  #networking.useNetworkd = true;
  #networking.firewall.checkReversePath = "loose";
  #networking.firewall.allowedTCPPorts = [ 22 25 53 465 587 ];
  #networking.firewall.allowedUDPPorts = [ 53 51820 ];
  #services.resolved.enable = true;

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
