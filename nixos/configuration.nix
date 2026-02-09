# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }: {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  nixpkgs.config = {
    allowUnfree = true;
    permittedInsecurePackages = [ ];
  };

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # >= 6.15 (audio, wifi)
  boot.kernelPackages = pkgs.linuxKernel.packages.linux_6_18;
  boot.kernelParams = [ ];
  boot.initrd.kernelModules = [ "amdgpu" ];
  boot.supportedFilesystems = [ "ntfs" ];
  boot.blacklistedKernelModules = [
    "nouveau"
    "nvidia"
    "nvidiafb"
    "nvidia-drm"
    "nvidia-uvm"
    "nvidia-modeset"
  ];

  # Bootloader
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot";
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "nodev";
  boot.loader.grub.useOSProber = true;
  boot.loader.grub.efiSupport = true;

  zramSwap = {
    enable = true;
    memoryPercent = 200;
  };

  boot.kernel.sysctl = {
    "vm.swappiness" = 100;
    "vm.page-cluster" = 0;
  };

  hardware.amdgpu.initrd.enable = true;

  hardware.cpu.amd.updateMicrocode = true;
  hardware.enableRedistributableFirmware = true;
  services.fwupd.enable = true;

  systemd.tmpfiles.rules =
    [ "L+    /opt/rocm/hip   -    -    -     -    ${pkgs.rocmPackages.clr}" ];

  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
      libvdpau-va-gl
      libva-vdpau-driver
      rocmPackages.clr
      rocmPackages.clr.icd
    ];
  };

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };

  services.blueman.enable = true;

  time.timeZone = "Asia/Bishkek";
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.supportedLocales = [ "all" ];

  networking.hostName = "yuri-alpha";

  networking.networkmanager.enable = true;
  networking.networkmanager.wifi.backend = "iwd";

  networking.wireless.iwd.enable = true;
  networking.wireless.iwd.settings = {
    General = {
      RoamThreshold = -75;
      RoamThreshold5G = -80;
      RoamRetryInterval = 20;
    };
  };

  services.nscd.enableNsncd = true;

  networking.firewall = {
    enable = true;

    allowedTCPPorts = [
      # SSH
      22

      # Samba
      139
      445

      # CUPS (printing)
      631

      # Syncthing (transfer)
      22000
    ];

    allowedUDPPorts = [
      # Samba
      137
      138

      # mDNS (printer discovery)
      5353

      # Syncthing (local discovery + QUIC)
      21027
      22000
    ];
  };

  security.rtkit.enable = true;
  security.polkit.enable = true;

  # sudo
  security.sudo.extraConfig = ''
    Defaults timestamp_timeout=-1
  '';

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    nix-direnv.enable = true;
  };

  programs.dconf.enable = true;
  programs.autojump.enable = true;
  programs.nix-ld.enable = true;

  programs.appimage = {
    enable = true;
    binfmt = true;
  };

  programs.ssh = { startAgent = true; };

  services.openssh.enable = true;

  environment.localBinInPath = true;

  environment.systemPackages = with pkgs; [ glibcLocales busybox iputils ];

  environment.variables = {
    QT_QPA_PLATFORM = "wayland";
  };

  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  virtualisation.docker.enable = true;
  virtualisation.docker.rootless = {
    enable = true;
    setSocketVariable = true;
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  services.emacs = {
    enable = true;
    package = (pkgs.emacsPackagesFor pkgs.emacs-pgtk).emacsWithPackages
      (epkgs: [ epkgs.mu4e epkgs.melpaPackages.telega ]);
  };

  # Samba
  services.samba = { enable = true; };

  services.clamav = {
    daemon.enable = true;
    updater.enable = true;
    updater.interval = "daily";
  };

  services.flatpak.enable = true;

  services.xserver.enable = true;
  services.xserver.videoDrivers = [ "amdgpu" ];
  services.xserver.displayManager.lightdm.enable = false;
  services.xserver.displayManager.lightdm.greeters.gtk.enable = false;

  services.xserver = {
    dpi = 180;
    xkb = {
      variant = "";
      layout = "us";
    };
  };

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

  services.dbus.enable = true;

  qt = {
    enable = true;
    style = "adwaita";
    platformTheme = null;
  };

  environment.pathsToLink =
    [ "/share/xdg-desktop-portal" "/share/applications" ];

  services.gvfs.enable = true;

  # Enable sound with pipewire.
  services.pulseaudio.enable = false;

  services.seatd.enable = true;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  xdg.portal = {
    enable = true;
    wlr.enable = true;
    extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
  };

  hardware.uinput.enable = true;
  services.udev = {
    # NOTE: Xremap requires the following:
    # https://github.com/xremap/xremap?tab=readme-ov-file#running-xremap-without-sudo
    extraRules = ''
      KERNEL=="uinput", GROUP="input", TAG+="uaccess"
    '';
  };

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
  };

  system.stateVersion = "23.11";
}
