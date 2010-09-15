def wgets |URL| 
  return `wget --quiet --output-document - %(URL)`;
end