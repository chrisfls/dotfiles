{
  replaceVars = attrs: str:
    builtins.replaceStrings
      (map (key: "\$${key}") (builtins.attrNames attrs))
      (builtins.attrValues attrs)
      str;

  u = str: builtins.fromJSON "\"${str}\"";
}
