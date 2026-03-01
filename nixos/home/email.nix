{ config, pkgs, lib, ... }:
let
  maildir = "${config.home.homeDirectory}/Maildir";
  offlineimapBin = lib.getExe pkgs.offlineimap;
in {
  home.packages = [
    pkgs.offlineimap
    pkgs.mu
  ];

  programs.offlineimap.enable = true;
  programs.mu.enable = true;

  accounts.email = {
    maildirBasePath = maildir;
    accounts = {
      fastmail = {
        address = "seryiza@fastmail.com";
        userName = "seryiza@fastmail.com";
        primary = true;
        flavor = "fastmail.com";
        realName = "Sergey Zaborovsky";
        aliases = [ "/@seryiza\\.xyz$/" ];
        passwordCommand = "${pkgs.pass}/bin/pass mail/seryiza@fastmail.com";
        mu = { enable = true; };
        offlineimap = {
          enable = true;
          extraConfig.account = { autorefresh = 0; };
        };
      };
    };
  };

  # Run OfflineIMAP every 5 minutes as a user service.
  systemd.user.services.offlineimap-sync = {
    Unit.Description = "OfflineIMAP sync";
    Service = {
      Type = "oneshot";
      ExecStart = offlineimapBin;
      Environment =
        "PATH=${lib.makeBinPath [ pkgs.coreutils pkgs.mu pkgs.offlineimap ]}";
    };
    Install.WantedBy = [ "default.target" ];
  };

  systemd.user.timers.offlineimap-sync = {
    Unit.Description = "Run OfflineIMAP periodically";
    Timer = {
      OnBootSec = "1m";
      OnUnitActiveSec = "5m";
      Persistent = true;
    };
    Install.WantedBy = [ "timers.target" ];
  };
}
