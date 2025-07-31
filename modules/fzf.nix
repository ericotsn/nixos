{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.programs.fzf;

  fishIntegration = ''
    ${cfg.package}/bin/fzf --fish | source
  '';
in
{
  disabledModules = [ "programs/fzf.nix" ];

  options.programs.fzf = {
    enable = mkEnableOption "A command-line fuzzy finder";

    package = mkPackageOption pkgs "fzf" { };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    programs.fish.interactiveShellInit = mkIf config.programs.fish.enable (mkOrder 200 fishIntegration);
  };
}
