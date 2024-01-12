{
  replaceVars = attrs: string:
    builtins.replaceStrings
      (map (name: "\$${name}") (builtins.attrNames attrs))
      (builtins.attrValues attrs)
      string;
}
