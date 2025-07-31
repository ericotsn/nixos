{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.programs.polkit-gnome;
in
{
  options.programs.polkit-gnome = {
    enable = mkEnableOption "Legacy polkit authentication agent from GNOME";

    package = mkPackageOption pkgs "polkit_gnome" { };

    systemd.target = mkOption {
      type = types.str;
      description = ''
        The systemd target that will automatically start the authentication agent.
      '';
      default = "graphical-session.target";
    };
  };

  config = mkIf cfg.enable {
    security.polkit.enable = true;

    systemd.user.services.polkit-gnome-authentication-agent-1 = {
      description = "polkit-gnome-authentication-agent-1";
      wantedBy = [ cfg.systemd.target ];
      wants = [ cfg.systemd.target ];
      after = [ cfg.systemd.target ];

      serviceConfig = {
        Type = "simple";
        ExecStart = "${cfg.package}/libexec/polkit-gnome-authentication-agent-1";
        Restart = "on-failure";
        RestartSec = 1;
        TimeoutStopSec = 10;
      };
    };
  };
}
