let
  replaceStrings = builtins.replaceStrings;
  map = builtins.map;
  attrNames = builtins.attrNames;
  attrValues = builtins.attrValues;
in
{
  replaceVars = attrs: string:
    replaceStrings
      (map (name: "\$${name}") (attrNames attrs))
      (attrValues attrs)
      string;
}
