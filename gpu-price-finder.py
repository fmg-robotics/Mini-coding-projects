from bs4 import BeautifulSoup
import requests

prices = []
priceSplit = []


def split(word):
    return list(word)


def convert(lists):
    c = int("".join(map(str, lists)))
    return c


name_gpu = input("What gpu are you looking for?")
gpu = name_gpu.replace(" ", "+")
url = "https://www.newegg.com/p/pl?d=" + gpu + "&N=8000"
result = requests.get(url)
doc = BeautifulSoup(result.text, "html.parser")
price = doc.find_all(text="$")

for i in range(len(price)):
    parent = price[i].parent
    strong = parent.find("strong")
    prices.append(strong.string)

for i in prices:
    split(i)
    priceSplit.append(split(i))

pricing = []
a = 0
for i in range(len(priceSplit)):
    for j in priceSplit[i]:
        if j == ",":
            priceSplit[i].remove(j)
    pricing.append(convert(priceSplit[i]))
    a += 1

print(f'The price of a {name_gpu} is ${min(pricing)} on newegg')
