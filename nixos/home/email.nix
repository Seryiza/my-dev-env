{ config, pkgs, lib, ... }:
{
  home.packages = [
    pkgs.offlineimap
    pkgs.mu
  ];

  programs.offlineimap.enable = true;
  programs.mu.enable = true;

  accounts.email = {
    maildirBasePath = "${config.home.homeDirectory}/Maildir";
    accounts = {
      fastmail = {
        address = "seryiza@fastmail.com";
        userName = "seryiza@fastmail.com";
        primary = true;
        flavor = "fastmail.com";
        realName = "Sergey Zaborovsky";
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
    Service = {
      ExecStart = "${pkgs.offlineimap}/bin/offlineimap";
      Environment =
        "PATH=${lib.makeBinPath [ pkgs.coreutils pkgs.mu pkgs.offlineimap ]}";
    };
    Unit.Description = "OfflineIMAP sync";
    Install.WantedBy = [ "default.target" ];
  };

  systemd.user.timers.offlineimap-sync = {
    Unit.Description = "Run OfflineIMAP periodically";
    Timer = {
      OnBootSec = "1m";
      OnUnitActiveSec = "5m";
    };
    Install.WantedBy = [ "timers.target" ];
  };
}
