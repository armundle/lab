from bs4 import BeautifulSoup
import requests
import urllib
import os
from constants import URL, PAGES, PREFIX, DIRECTORY
import sys
reload(sys)
sys.setdefaultencoding('utf8')

ID = "???"
IMG = "???"

for idx in PAGES:
    r = urllib.urlopen(URL + '/' + idx).read()
    soup = BeautifulSoup(r, 'html.parser')
    #print soup.prettify()
    link = soup.find_all(id=ID)
    imgLink = PREFIX + link[0].img[IMG]
    #print imgLink
    #urllib.urlretrieve(urllib.quote(imgLink), 'test.jpg')
    res = requests.get(imgLink)

    if not os.path.exists(DIRECTORY):
        os.makedirs(DIRECTORY)

    print "Saving " + DIRECTORY + "/" + idx + ".jpg"
    imageFile = open(os.path.join(DIRECTORY, (idx + '.jpg')), 'w+')
    for chunk in res.iter_content(100000):
        imageFile.write(chunk)
    imageFile.close()
