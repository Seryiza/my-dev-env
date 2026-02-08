{ pkgs, lib, ... }:
{
  systemd.user.services.ics-sync = {
    Unit.Description = "Run ics-sync";
    Service = {
      Type = "oneshot";
      ExecStart = "%h/.local/bin/hs-ical2org";
      WorkingDirectory = "%h";
      Environment = [
        "PATH=${
          lib.makeBinPath [ pkgs.bash pkgs.gawk pkgs.wget pkgs.coreutils ]
        }:%h/.local/bin"
      ];
    };
  };

  systemd.user.timers.ics-sync = {
    Unit.Description = "Run ics-sync every 15 minutes";
    Timer = {
      OnCalendar = "*:0/15";
      Persistent = true;
      Unit = "ics-sync.service";
    };
    Install.WantedBy = [ "timers.target" ];
  };
}
