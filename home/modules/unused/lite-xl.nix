# albeit lite-xl is really cool, it is not a proper tool for development in 
# any language but lua...

# default plugins:
#   autocomplete
#   autorelaod
#   contextmenu
#   detectindent
#   drawwhitespace
#   lineguide
#   linewrapping
#   macro
#   projectsearch
#   quote
#   reflow
#   tabularize
#   toolbarview
#   treeview
#   trimwhitespace
#   workspace
# installed plugins:
#   Plugin Manager
#   settings
{ config, lib, pkgs, ... }:
let inherit (config.modules.lite-xl) enable;
  colors = pkgs.fetchFromGitHub {
    owner = "lite-xl";
    repo = "lite-xl-colors";
    rev = "315f1233351ec1176f2177e1cca53e6db145e6eb";
    sha256 = "sha256-boxeC8QDJ4+G330LGLrUGJhBhjW3U38031D6jNGJ8ag=";
  };

  /*
  fonts = pkgs.symlinkJoin {
    name = "lite-xl-fonts";
    paths = [
      "${pkgs.noto-fonts}/share/fonts/noto"
      "${pkgs.cascadia-code}/share/fonts/truetype"
    ];
  };
  */
in
{
  options.modules.lite-xl.enable = lib.mkEnableOption "Enable lite-xl module";

  config = lib.mkIf enable {
    home.packages = [ pkgs.lite-xl pkgs.extra.lpm ];
    xdg.configFile."lite-xl/colors".source = "${colors}/colors";
    # xdg.configFile."lite-xl/fonts".source = fonts;
  };
}

# plugin_manager --assume-yes

# settings
# search_ui
# gui_filepicker



# scm

# lsp
# lsp_snippets
# lspkind

# lintplus

# editorconfig
# gitblame
# gitopen




# smoothcaret
# fontconfig
# nerdicons
# indentguide
# evergreen
# rainbowparen	
# selectionhighlight	

# tab_switcher	

# endwise # lfautoinsert	




# ephemeral_tabs	
# restoretabs
# navigate	

# open_ext	
# openfilelocation	
# sort	
# force_syntax	
