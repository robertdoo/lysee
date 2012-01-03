load('sh')

def wgets(URL)
    system(format('wget --quiet --output-document - %(URL)'))
end
