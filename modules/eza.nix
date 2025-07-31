{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.eza;

  aliases = builtins.mapAttrs (_name: value: lib.mkDefault value) {
    ls = "eza";
    ll = "eza -l";
    la = "eza -la";
    lt = "eza --tree";
  };
in
{
  options.programs.eza = {
    enable = lib.mkEnableOption "A modern alternative to ls";

    package = lib.mkPackageOption pkgs "eza" { };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    programs.fish.shellAliases = lib.mkIf config.programs.fish.enable (lib.mkForce aliases);
  };
}
