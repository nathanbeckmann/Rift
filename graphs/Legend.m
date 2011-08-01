(* ::Package:: *)

(* Legends *)

legendItem[c_, t_] := 
    {Style[t,Medium], Graphics[legendStyle[[c]]~Join~{Line[{{0, 0}, {1, 0}}]}~Join~{Inset[legendMarkers[[c,1]]]}]}

makeLegend[items__] := 
    GraphicsGrid[legendItem @@ # & /@ {items}, ImageSize -> Automatic, Frame->True, Spacings->{-100,-200} ]

makeLegend2[items__] := 
 makeLegend @@ Table[{i, {items}[[i]]}, {i, Length[{items}]}]

showLegend[legend_, plot_] := 
 GraphicsColumn[{legend, plot}, Spacings -> {0, -75}, ImageSize -> 400]

insetLegend[legend_] := 
    Epilog->Inset[legend, Scaled[{0.00, 1.05}], {Left, Top}, Scaled[{1.5,.5}]]

legendColors = Darker /@ {Blue, Red, Yellow, Green, Orange }
legendStyle = {legendColors} // Transpose

(*legendDashing = {Directive[{}]}~Join~(Dashing/@{Small, Medium})~Join~{Dotted,DotDashed}
legendStyle = {legendColors, legendDashing, ConstantArray[Thick,Length[legendColors]]} // Transpose*)

legendMarkers = { Disk[], Rectangle[], Polygon[{{1, 0}, {0, Sqrt[3]}, {-1, 0}}], Polygon[{{1, Sqrt[3]}, {0, 0}, {-1, Sqrt[3]}}], Polygon[{{-1,0},{0,1},{1,0},{0,-1}}] };
legendMarkers = { legendColors, legendMarkers } // Transpose;
legendMarkers = Graphics[#, ImageSize->5] & /@ legendMarkers;
legendMarkers = { legendMarkers } // Transpose; (* workaround for mathematica 7 bug *)

(*errorBars = ErrorBarFunction->Function[{coords, errs}, {Opacity[0.2],Rectangle[coords+{-.01,errs[[2,1]]},coords+{.01,errs[[2,2]]}]}]*)

style := Sequence[Joined->True, PlotStyle->legendStyle, BaseStyle->{Medium}, AspectRatio->2/3, PlotMarkers->legendMarkers, Axes->False, Frame->{True,True,False,False}, ImageSize->300];
