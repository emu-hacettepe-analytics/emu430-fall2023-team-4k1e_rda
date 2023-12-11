import pandas

tempRead = pandas.read_excel("suc turu ve egitim durumuna gore ceza infaz kurumuna giren hukumluler.xls", "temp", header = None)
tempRead.dropna(how = "all", axis = 1, inplace=True)
tempRead.dropna(how = "all", inplace=True)
tempRead.replace("-", 0, inplace=True)

headers = pandas.read_excel("headers.xlsx", header=None)
headers.dropna(axis=1, inplace=True)
headersNested = headers.values.tolist()
headersList = [item for sublist in headersNested for item in sublist]

indexes = pandas.read_excel("new indexes.xlsx", usecols="A", header=None)
indexesNested = indexes.values.tolist()
indexesList = [item for sublist in indexesNested for item in sublist]
tempRead.index = indexesList

tempRead.to_excel("suc 2011.xlsx",header=headersList)