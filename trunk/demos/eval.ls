= "Input expression to evaluate or \"quit\" to leave!\n";

= "\nlysee> ";
for input:string in sys::readln if input.trim() do
  = input, "==> ";
  break if input.trim() == "quit";
  try 
    = sys::eval("return " + input), eol;
  except 
    = __error__.message.extractValue(" - ").extractValue(") "), eol;
  end
  = "\nlysee> ";
end

= "bye-bye!\n";
