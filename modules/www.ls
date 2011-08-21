{import sh}

{fun wgets URL ->
    {system @'wget --quiet --output-document - %(URL)'}}
