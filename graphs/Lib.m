(* ::Package:: *)

Needs["PlotLegends`"]

<< "./graphs/Legend.m"

colors = Join @@ (ColorData[#, "ColorList"] & /@ {3, 10, 15});

(* { Red, Green, Blue, Black, White, Gray, Cyan, Magenta,
Yellow, Brown, Orange, Pink, Purple, LightRed, LightGreen, LightBlue,
LightGray, LightCyan, LightMagenta, LightYellow, LightBrown,
LightOrange, LightPink, LightPurple } *)

lightColors = Lighter /@ colors;

darkColors = Darker /@ colors;

  (* Select only the data with points, otherwise ListPlot complains *)
nonEmptyQ[index_] := Length[ #[[index, 2]] ] > 0 &

(* Fill in the data with 0's for missing entries *)

  (* Fill the space between the list and the next entry *)
fillGap[a_, b_] := a ~Join~ Table[{i, 0}, {i, a[[-1,1]]+1, b[[1]]-1}] ~Join~ {b}

  (* Fill in a single data set (list) *)
fillList[list_] := Fold[fillGap, {{minTime-1,0}}, list ~ Join ~ {{maxTime+1,0}}]

  (* Fill in a particular data set *)
fillDataSet[index_] := fillList[#[[index,2]]] &

  (* Smooth a single data set *)
smooth[w_] := Block[ { times = #[[All, 1]], values = #[[All, 2]], window, trimmedTimes, smoothedValues},
                     window = Min[Length[values], w];
                     trimmedTimes = Drop[times, -window+1];
                     smoothedValues = MovingAverage[values, window];
                     { trimmedTimes, smoothedValues } // Flatten[#, {{2},{1}}] & ] &

  (* Stacked data *)
stack[data_] := 
    Block[ {values, acc},
           values = data[[All, All, 2]];
           acc = Accumulate[values] ]

  (* Normalize a table to the last value [for stacked graphs] *)
normalize[table_] :=
    Table[ #[[i]] / table[[-1,i]], {i, Length[#]} ] & /@ table

  (* Common plot options *)
plotOpts[label_, names_] = Sequence[ 
    legendStyle,
    FrameLabel -> {{label,""},{"Time (sec)",""}},
    FrameTicksStyle -> Directive[Tiny],
    Joined -> True,
    Frame -> {{True,False},{True,False}},
    PlotRange -> Full,
    PlotStyle -> darkColors,
    PlotLabel -> headline,
    AspectRatio -> 1/2
                   ];

plotOptsStacked[data_, label_, names_] := Sequence[
    plotOpts[label, names],
    Filling -> If[ Length[data] > 1, Table[ i -> {{i-1}, lightColors[[i]]}, {i, 2, Length[data]} ], None]
                          ]

  (* Generate and save a plot *)
plot[filename_, label_, data_] := 
    Block[{plot},
          plot = showLegend[
              makeLegend2 @@ names,
              ListPlot[data,
                       plotOpts[label,names]
                      ]];
          Export[filename, plot, ImageResolution -> 150]]

plotStacked[filename_, label_, data_] := 
    Block[{plot},
          plot = showLegend[
              makeLegend2 @@ names,
              ListPlot[data,
                       PlotRange -> {0, Full},
                       plotOptsStacked[data, label, names]
                      ]];
          Export[filename, plot, ImageResolution -> 150]]

plotStackedN[filename_, label_, data_] := 
    Block[{plot},
          plot = showLegend[
              makeLegend2 @@ names,
              ListPlot[data,
                       PlotRange -> {0, 1},
                       plotOptsStacked[data, label, names]
                      ]];
          Export[filename, plot, ImageResolution -> 150]]

(* Isolate a data set, format it, and plot it *)
processDataSet[data_, label_, index_, smoothing_] :=
    Block[{sorted, filtered, minTime, maxTime, filled, smoothed, names},
          (* Sort data by total stat *)
          sorted = Sort[data, #1[[index, 1]] < #2[[index, 1]] &];

          (* Remove empty sets *)
          filtered = Select[sorted, nonEmptyQ[index]];

          (* Get global min and max *)
          minTime = filtered[[All, index, 2, 1, 1]] // Min;
          maxTime = filtered[[All, index, 2, -1, 1]] // Max;

          (* Fill in full data set, and extract it *)
          filled = fillDataSet[index] /@ filtered;

          (* Now smooth the values in the data set using
          MovingAverage, and re-zip the data to get the times back *)
          smoothed = smooth[smoothing] /@ filled;

          names = filtered[[All, 1]];

          {names, smoothed}]

plotDataSet[data_, label_, index_, plotfile_, ext_] :=
    Block[{names, smoothed, stacked, normalized, fineSmooth, coarseSmooth},

          fineSmooth = 10;
          coarseSmooth = 30;

          (* basic graph with coarse smoothing *)
          {names, smoothed} = processDataSet[data, label, index, coarseSmooth];
          plot[plotfile <> ext, label, smoothed];

          (* use fine grain for stacked graphs *)
          {names, smoothed} = processDataSet[data, label, index, fineSmooth];

          (* Stacked graph *)
          stacked = stack[smoothed];
          plotStacked[plotfile <> "-stack" <> ext, label, stacked];

          (* Normalized stacked graphs *)
          normalized = normalize[stacked];

          plotStackedN[plotfile <> "-stackn" <> ext, label, normalized];

          Null]
