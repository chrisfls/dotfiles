{ config, lib, pkgs, ... }:
let
  cfg = config.modules.codium;
  ext = pkgs.vscode-extensions;
in
{
  options.modules.codium.enable = lib.mkEnableOption "Enable codium module";

  config = lib.mkIf cfg.enable {
    programs.vscode = {
      enable = true;
      package = pkgs.vscodium;
      # not working because tries to write to the store:
      #
      # { name = "csdevkit"; publisher = "ms-dotnettools"; version = "1.2.5"; sha256 = "16xkvfmznjhkqypdg51mw9lbsqmhqqnfvi99n5v936npnivxh977"; }
      # { name = "csharp"; publisher = "ms-dotnettools"; version = "2.14.8"; sha256 = "0d345y1gvxxrx98mz5rqkwhsmabl96y6b94y6prcq9c9vki5gcb8"; }
      # { name = "vscodeintellicode-csharp"; publisher = "ms-dotnettools"; version = "0.1.26"; sha256 = "0rdr6kp580drk35iypqa0xfqvj215fanq6vxkgigdnhjhvhwfw3x"; }
      # { name = "vscode-dotnet-runtime"; publisher = "ms-dotnettools"; version = "2.0.0"; sha256 = "1sn454mv5vb9qspaarr8wp0yqx4g20c1mf0mjhhzmj9x92r9adx1"; }
      #
      # not playing nice with home-manager:
      #
      # { name = "vscode-sundial"; publisher = "muuvmuuv"; version = "3.4.1"; sha256 = "0mv4y1dspn0zgx37bpki1jdpkl8j74kf789b630kg32rc4l6mfa7"; }
      # { name = "nixpkgs-fmt"; publisher = "B4dM4n"; version = "0.0.1"; sha256 = "1gvjqy54myss4w1x55lnyj2l887xcnxc141df85ikmw1gr9s8gdz"; }
      #
      # not needed anymore:
      #
      # { name = "remote-explorer"; publisher = "ms-vscode"; version = "0.5.2023121309"; sha256 = "1n6ni7b1x2vzjqwb59ss0r6xdk8hagcfmkxm0c2iqw7ifsq4y962"; }
      # { name = "remote-ssh-edit"; publisher = "ms-vscode-remote"; version = "0.86.0"; sha256 = "0cmh2d73y1kmp6a92h3z7gams7lnqvb7rgib52kqslm4hyhdmii6"; }
      # { name = "remote-ssh"; publisher = "ms-vscode-remote"; version = "0.108.2023122115"; sha256 = "0wpyrnbjmy101c56jl44cm5ryshx48w7qap4rk7qqsr0bgs4misl"; }
      #
      # deprecated:
      #
      # { name = "theme-timer"; publisher = "raymcclain"; version = "1.0.1"; sha256 = "0gck2v5jz5c8n2gd4pfnxi7s9fsfbz65in5np4dpfl5qcqywr1y7"; }
      extensions = pkgs.vscode-utils.extensionsFromVscodeMarketplace
        (import ./codium/extensions.nix).extensions;
      enableUpdateCheck = false;
      enableExtensionUpdateCheck = true;
      userSettings = {
        "[csharp]" = { "editor.defaultFormatter" = "csharpier.csharpier-vscode"; };
        "[json]" = { "editor.defaultFormatter" = "vscode.json-language-features"; };
        "[jsonc]" = { "editor.defaultFormatter" = "vscode.json-language-features"; };
        "[markdown]" = { "editor.defaultFormatter" = "denoland.vscode-deno"; };
        "[typescript]" = { "editor.defaultFormatter" = "denoland.vscode-deno"; };
        "[nix]" = { "editor.defaultFormatter" = "jnoortheen.nix-ide"; };
        "autoHide.autoHideReferences" = false;
        "autoHide.autoHideSideBar" = false;
        "autoHide.hideOnOpen" = false;
        "autoHide.panelDelay" = 0;
        "autoHide.sideBarDelay" = 0;
        "breadcrumbs.icons" = false;
        "breadcrumbs.showArrays" = false;
        "breadcrumbs.showBooleans" = false;
        "breadcrumbs.showClasses" = false;
        "breadcrumbs.showConstants" = false;
        "breadcrumbs.showConstructors" = false;
        "breadcrumbs.showEnumMembers" = false;
        "breadcrumbs.showEnums" = false;
        "breadcrumbs.showEvents" = false;
        "breadcrumbs.showFields" = false;
        "breadcrumbs.showFiles" = false;
        "breadcrumbs.showFunctions" = false;
        "breadcrumbs.showInterfaces" = false;
        "breadcrumbs.showKeys" = false;
        "breadcrumbs.showMethods" = false;
        "breadcrumbs.showModules" = false;
        "breadcrumbs.showNamespaces" = false;
        "breadcrumbs.showNull" = false;
        "breadcrumbs.showNumbers" = false;
        "breadcrumbs.showObjects" = false;
        "breadcrumbs.showOperators" = false;
        "breadcrumbs.showPackages" = false;
        "breadcrumbs.showProperties" = false;
        "breadcrumbs.showStrings" = false;
        "breadcrumbs.showStructs" = false;
        "breadcrumbs.showTypeParameters" = false;
        "breadcrumbs.showVariables" = false;
        "breadcrumbs.symbolPath" = "off";
        "codesnap.roundedCorners" = false;
        "codesnap.showLineNumbers" = false;
        "codesnap.showWindowControls" = false;
        "codesnap.showWindowTitle" = false;
        "codesnap.shutterAction" = "copy";
        "codesnap.target" = "window";
        "diffEditor.ignoreTrimWhitespace" = false;
        "dotnet.server.useOmnisharp" = true;
        "editor.acceptSuggestionOnCommitCharacter" = false;
        "editor.acceptSuggestionOnEnter" = "off";
        "editor.accessibilitySupport" = "off";
        "editor.bracketPairColorization.independentColorPoolPerBracketType" = true;
        "editor.codeLens" = false;
        #"editor.codeLensFontSize" = 12;
        "editor.colorDecorators" = false;
        "editor.cursorBlinking" = "smooth";
        "editor.cursorSurroundingLines" = 12;
        "editor.cursorWidth" = 2;
        "editor.experimental.asyncTokenization" = true;
        "editor.fontFamily" = "'Cascadia Mono'";
        "editor.fontLigatures" = "'ss01'";
        #"editor.fontSize" = 19;
        "editor.fontWeight" = "400";
        "editor.hover.delay" = 150;
        "editor.inlayHints.enabled" = "offUnlessPressed";
        # "editor.inlayHints.fontSize" = 16;
        # "editor.lineHeight" = 28;
        "editor.minimap.enabled" = true;
        "editor.minimap.maxColumn" = 80;
        "editor.minimap.renderCharacters" = false;
        "editor.minimap.showSlider" = "always";
        "editor.quickSuggestions" = { "comments" = "on"; "other" = "on"; "strings" = "on"; };
        "editor.quickSuggestionsDelay" = 0;
        "editor.rulers" = [ 79 99 119 ];
        "editor.semanticHighlighting.enabled" = false;
        "editor.snippets.codeActions.enabled" = false;
        "editor.snippetSuggestions" = "none";
        "editor.suggest.insertMode" = "replace";
        "editor.suggest.localityBonus" = true;
        "editor.suggest.matchOnWordStartOnly" = false;
        "editor.suggest.showSnippets" = false;
        # "editor.suggestFontSize" = 17;
        "editor.tabSize" = 2;
        "editor.trimAutoWhitespace" = true;
        "editor.wordWrap" = "on";
        "editor.wrappingIndent" = "indent";
        "errorLens.gutterIconsEnabled" = true;
        "errorLens.statusBarMessageEnabled" = true;
        "explorer.excludeGitIgnore" = true;
        "explorer.sortOrder" = "type";
        "extensions.autoCheckUpdates" = true;
        "extensions.autoUpdate" = false;
        "extensions.ignoreRecommendations" = true;
        "files.associations" = { "*.json" = "jsonc"; "package.json" = "json"; "*.just" = "just"; };
        "files.eol" = "\n";
        "files.trimFinalNewlines" = true;
        "git.autofetch" = true;
        "git.autorefresh" = true;
        "git.autoRepositoryDetection" = false;
        "git.confirmSync" = false;
        "git.detectSubmodules" = false;
        "git.suggestSmartCommit" = false;
        "javascript.suggest.classMemberSnippets.enabled" = false;
        "javascript.updateImportsOnFileMove.enabled" = "always";
        "omnisharp.disableMSBuildDiagnosticWarning" = true;
        "omnisharp.enableAsyncCompletion" = true;
        "omnisharp.organizeImportsOnFormat" = true;
        "omnisharp.projectFilesExcludePattern" = "**/node_modules/**;**/.git/**;**/bower_components/**;**/paket-files/**;**/bin/**;**/obj/**";
        "omnisharp.sdkIncludePrereleases" = true;
        "omnisharp.sdkPath" = "C:\\Program Files\\dotnet\\sdk\\8.0.100-preview.7.23376.3";
        "omnisharp.useModernNet" = true;
        # "scm.inputFontSize" = 16;
        "sundial.autoLocale" = false;
        "sundial.sunrise" = "06:00";
        "sundial.sunset" = "18:00";
        "terminal.integrated.defaultProfile.linux" = "fish";
        "terminal.integrated.fontFamily" = "'Cascadia Mono'"; # JetBrains Mono
        #"terminal.integrated.fontSize" = 12;
        "terminal.integrated.profiles.windows" = { "Command Prompt" = null; "Git Bash" = null; "PowerShell" = { "icon" = "terminal-powershell"; "path" = [ "C:\\Program Files\\PowerShell\\7\\pwsh.exe" ]; "source" = "PowerShell"; }; };
        "terminal.integrated.sendKeybindingsToShell" = true;
        "terminal.integrated.tabs.enabled" = false;
        "typescript.suggest.classMemberSnippets.enabled" = false;
        "typescript.suggest.objectLiteralMethodSnippets.enabled" = false;
        "typescript.updateImportsOnFileMove.enabled" = "always";
        "update.enableWindowsBackgroundUpdates" = false;
        "update.mode" = "start";
        "window.zoomLevel" = 0;
        "workbench.activityBar.iconClickBehavior" = "focus";
        "workbench.colorCustomizations" = { "[Solarized Light]" = { "editorBracketHighlight.foreground1" = "#002b36"; "editorBracketHighlight.foreground2" = "#586e75"; "editorBracketHighlight.foreground3" = "#586e75"; "editorBracketHighlight.foreground4" = "#586e75"; "editorBracketHighlight.foreground5" = "#586e75"; "editorBracketHighlight.foreground6" = "#586e75"; "editorBracketHighlight.unexpectedBracket.foreground" = "#dc322fff"; }; };
        "workbench.colorTheme" = "Gruvbox Dark Hard";
        "workbench.editor.closeEmptyGroups" = false;
        "workbench.editor.enablePreview" = false;
        "workbench.editor.limit.enabled" = true;
        "workbench.editor.limit.excludeDirty" = true;
        "workbench.editor.limit.perEditorGroup" = true;
        "workbench.editor.limit.value" = 1;
        "workbench.editor.openPositioning" = "first";
        "workbench.editor.showTabs" = "single";
        "workbench.iconTheme" = "material-icon-theme";
        "workbench.preferredDarkColorTheme" = "Gruvbox Dark Hard";
        "workbench.preferredLightColorTheme" = "Gruvbox Light Medium";
      };
    };
  };
}
