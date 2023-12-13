let
  fromJSON = builtins.fromJSON;
in
{
  unicode = string: fromJSON "\"${string}\"";
}
