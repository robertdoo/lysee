{import sh}

{def wgets |URL|
    {system @'wget --quiet --output-document - %(URL)'}}
