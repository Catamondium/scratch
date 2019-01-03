# find the longest words representable in a 7-segment display
import re
from urllib.request import urlopen

url = "https://raw.githubusercontent.com/dwyl/english-words/master/"
engsource = ["words_alpha.txt", "words.txt"]
blocked = "[gkmqvwxzio]+" # arguably gios are okie
longest = [""]


with urlopen(url + engsource[0]) as response: # download words
    words = set(response.read().decode("utf-8") # conv to str
            .lower().split("\r\n")) # parse

for testword in words:
    if len(testword) < len(longest[0]): # filter shorter
        continue
    
    #validate
    if not re.search(blocked, testword):
        if len(testword) > len(longest[0]):
            longest = []
        longest.append(testword)

print("%d words" % len(words))
print("%d longest representable:\n\t%s" %
        (len(longest), "\n\t".join(longest)))
