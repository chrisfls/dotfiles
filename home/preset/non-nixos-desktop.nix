{ pkgs, ... }:
let
  wrap = wrapper: { package, exe }:
    pkgs.symlinkJoin {
      name = exe;
      paths = [
        (pkgs.writeShellScriptBin exe "#!/bin/sh\n${wrapper} ${package}/bin/${exe}\n")
        package
      ];
    };

  # TODO: generalize which wrapper to use
  wrapGL = wrap "${pkgs.nixgl.nixGLIntel}/bin/nixGLIntel";

  wrapVulkan = wrap "${pkgs.nixgl.nixGLIntel}/bin/nixVulkanIntel";
in
{
  home.packages = [
    pkgs.nixgl.nixGLIntel
    pkgs.nixgl.nixVulkanIntel
  ];

  extra =
    let
      lxqt = pkgs.lxqt;
    in
    {
      xdg-desktop-portal.package = wrapGL lxqt.xdg-desktop-portal-lxqt;
      notifications.package = wrapGL lxqt.lxqt-notificationd;
      polkit-agent.package = wrapGL lxqt.lxqt-policykit;
      gui-sudo.package = lxqt.lxqt-sudo;
      ssh-askpass.package = wrapGL lxqt.lxqt-openssh-askpass;
      file-manager.package = wrapGL lxqt.pcmanfm-qt;
      volume-mixer.package = wrapGL lxqt.pavucontrol-qt;
      system-monitor.package = wrapGL lxqt.qps;
      clipboard-manager.package = wrapGL lxqt.qlipper;
      screenshot.package = wrapGL lxqt.screengrab;
    };
}
