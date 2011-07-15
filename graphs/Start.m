Needs["PlotLegends`"]

If [ $CommandLine // Length != 2, Print["Must pass path to a data file produced by parser!"]; Exit[-1]]

inputFile = $InitialDirectory <> "/" <> $CommandLine[[2]]

Print["Reading file: " <> inputFile]

Get[inputFile]
