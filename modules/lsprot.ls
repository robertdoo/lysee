def TV2VT:string |TV:string|
  S = TV.trim();
  T = S.extractName(" ").trim();
  if T then
    V = S.extractValue(" ").trim();
  else
    T = S;
    V = S;
  end
  return (T == "variant") ? V : V + ":" + T;
end

def prototypeC2L |prototype:string|
  L = TV2VT(prototype.extractName("(")) + " |";
  prototype = prototype.extractValue("(").replace(")", ",") + ",";
  R = TV2VT(prototype.extractName(","));
  if R != ":" then
    L += R;
    prototype = prototype.extractValue(",");
    R = TV2VT(prototype.extractName(","));
    while R != ":" do
      L += ", " + R;
      prototype = prototype.extractValue(",");
      R = TV2VT(prototype.extractName(","));
    end
  end
  return L + "|";
end

= prototypeC2L("int a(string b, variant c, varlist d)");

