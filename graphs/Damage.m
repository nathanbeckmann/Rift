<< ./Start.m

(* Select only the data with points, otherwise ListPlot complains *)

nonEmptyQ[index_] := Length[ #[[index]] ] > 0 &

filtered = Select[data, nonEmptyQ[2]];

(* Fill in the data with 0's for missing entries *)

  (* Get global min and max *)
minTime = filtered[[All, 2, 1, 1]] // Min
maxTime = filtered[[All, 2, -1, 1]] // Max 

  (* Fill the space between the list and the next entry *)
fillGap[a_, b_] := a ~Join~ Table[{i, 0}, {i, a[[-1,1]]+1, b[[1]]-1}] ~Join~ {b}

  (* Fill in a single data set (list) *)
fillList[list_] := Fold[fillGap, {{minTime-1,0}}, list ~ Join ~ {{maxTime+1,0}}]

  (* Fill in a particular data set *)
fillDataSet[index_] := fillList[#[[index]]] &

  (* Fill in full data set, and extract it *)
filled = fillDataSet[2] /@ filtered;

(* Now smooth the values in the data set using MovingAverage, and re-zip the data to get the times back *)

  (* Smooth a single data set *)
smooth[w_] := Block[ { times = #[[All, 1]], values = #[[All, 2]], window, trimmedTimes, smoothedValues},
                     window = Min[Length[values], w];
                     trimmedTimes = Drop[times, -window+1];
                     smoothedValues = MovingAverage[values, window];
                     { trimmedTimes, smoothedValues } // Flatten[#, {{2},{1}}] & ] &

smoothed = smooth[30] /@ filled;

names = filtered[[All, 1]]

plot = ListPlot[smoothed,
                Joined -> True,
                Frame -> {{True,False},{True,False}},
                FrameLabel -> {{"Damage",""},{"Time (sec)",""}},
                PlotLegend->names,
                LegendPosition -> {1.1,-0.4},
                LegendSize -> {0.3, 1.0},
                LegendShadow -> None];

Export[inputFile <> ".pdf", plot]
