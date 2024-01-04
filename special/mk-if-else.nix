{ lib, ... }:
predicate: yes: no: lib.mkMerge [
  (lib.mkIf predicate yes)
  (lib.mkIf (!predicate) no)
]
