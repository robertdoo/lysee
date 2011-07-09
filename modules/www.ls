{import sh}

{def wgets:string |URL:string|
  {return sh::system(@'wget --quiet --output-document - %(URL)')}}
