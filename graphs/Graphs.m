<< ./Start.m

<< ./Lib.m

ext = ".pdf"

plotDataSet[data, "Damage", 2, inputFile <> "-damage", ext]
plotDataSet[data, "Heals", 3, inputFile <> "-heals", ext]
(* plotDataSet[data, "Damage Taken", 4, inputFile <> "-damagetaken", ext] *)
