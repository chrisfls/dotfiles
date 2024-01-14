{ lib, python3, mopidy, fetchFromGitHub, ... }:
python3.pkgs.buildPythonApplication rec {
  pname = "mopidy-youtube";
  version = "git";
  format = "setuptools";

  src = fetchFromGitHub {
    owner = "natumbri";
    repo = pname;
    rev = "f14535e6aeec19d5a581aebe4b8143211b462cc4";
    hash = "sha256-h3CYGtIOl95ZlLZprjc1wssWW9Wzr6zvikLX95Osahg=";
  };

  propagatedBuildInputs = with python3.pkgs; [
    beautifulsoup4
    cachetools
    pykka
    requests
    youtube-dl
    ytmusicapi
  ] ++ [
    mopidy
  ];

  nativeCheckInputs = with python3.pkgs; [
    vcrpy
    pytestCheckHook
  ];

  doCheck = false;

  pythonImportsCheck = [
    "mopidy_youtube"
  ];

  meta = {
    description = "Mopidy extension for playing music from YouTube";
    homepage = "https://github.com/natumbri/mopidy-youtube";
  };
}
