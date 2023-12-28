{ config, lib, pkgs, ... }:
let
  cfg = config.module.sublime;

  pkg = pkgs.sublime4-dev.overrideAttrs (old: {
    # sauce https://gist.github.com/skoqaq/3f3e8f28e23c881143cef9cf49d821ff?permalink_comment_id=4786328#gistcomment-4786328
    sublime_text = old.sublime_text.overrideAttrs
      (old: {
        postFixup =
          ''
            sed -i 's/\x80\x78\x05\x00\x0f\x94\xc1/\xc6\x40\x05\x01\x48\x85\xc9/g' "$out/sublime_text"
            ${old.postFixup}
          '';
      });
  });
in
{
  options.module.sublime.enable = lib.mkEnableOption "Enable sublime module";

  config = lib.mkIf cfg.enable {
    home.packages = [ pkg ];
    nixpkgs.config.permittedInsecurePackages = [ "openssl-1.1.1w" ];
  };
}
