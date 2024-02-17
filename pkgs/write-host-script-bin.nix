pkgs: name: text:
pkgs.writeTextFile {
  inherit name;
  executable = true;
  destination = "/bin/${name}";
  text =
    ''
      #!/usr/bin/env bash
      ${text}
    '';
}
