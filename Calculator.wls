#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Title::Closed:: *)
(*Optimal Star Distribution Calculator*)


(* ::Subtitle:: *)
(*by Eaux Tacous#1021*)


(* ::Section::Closed:: *)
(*Constants*)


(* ::Text:: *)
(*Taken from exidle wiki.*)


symbTable = {"y", "z", "s", "u", "v", "w", "\[Alpha]", "\[Beta]", "\[Gamma]", "\[Delta]", "\[CurlyEpsilon]", "\[Zeta]", "\[Eta]", "\[Theta]", "\[Iota]", "\[Kappa]", "\[Lambda]", "\[Nu]", "\[Xi]"};


(* ::Section::Closed:: *)
(*Helper Functions*)


myprint = Sow @* (Style[#, RGBColor["#ccc"], Bold]&) @* Row @* List;


(* Prevent Mathematica from complaining *)
externLP = Flatten[LinearProgramming[##]]&;


(* ::Section::Closed:: *)
(*Main algorithm*)


(* ::Text:: *)
(*2021-09: rewritten from the original compiled calcoptBLPComp (WDX blob) into plain
Wolfram Language, with one behavioural addition: an "unlocked variables" gate.
The optimizer previously assumed all 19 star variables were purchasable, so it
could recommend star levels for variables the player has not unlocked yet
(variables are unlocked with stars in the star shop, which persists across
supremacies while f(t) resets - so f(t) alone cannot infer unlocks).
The 4th argument is the number of unlocked variables (2..19, y and z included).
Automatic infers it from the f(t) price frontier (matches a fresh run, and is
the safe default). Passing 19 reproduces the original behaviour exactly.*)


calcoptBLPWrapper[in___] :=
    Print[
        StringReplace[
            "data:image/png;base64," ~ StringJoin ~ ExportString[
                im = Rasterize[
                    Block[{time, eval, $Messages, Print = myprint},
                        MemoryConstrained[
                            TimeConstrained[
                                Print["Inputs: ", ""[in]];
                                eval = EvaluationData[Catch[calcoptBLP[in]]];
                                Print["Total CPU time: ", eval["Timing"]];
                                If[eval["Success"],
                                    eval["Result"]
                                    ,
                                    Print["Something went really wrong."];
                                    Print["Please report your input and the following error(s) to Eaux Tacous#1021"];
                                    Print /@ eval["Messages"];
                                ],
                                10,
                                Print @ Style["Error: Timeout", Hue[{0, .8, .9}]]
                            ],
                            100*^6,
                            Print @ Style["Error: Out of memory", Hue[{0, .8, .9}]]
                        ]
                    ] // Reap // Last // Last // Column,
                    Background -> RGBColor["#222"], BaselinePosition -> Center, ImageFormattingWidth -> Infinity, ImageResolution -> 100
                ],
                {"Base64", "PNG"},
                "ColorMapLength" -> 24,
                CompressionLevel -> 1,
                Dithering -> None,
                IncludeMetaInformation -> {}
            ],
            {"\n" -> ""}
        ]
    ];


calcoptBLP[___] :=
    Throw[Print @ Style["Invalid Input", Hue[{0, .8, .9}]]];


calcoptBLP[llf_, initstars:Except[_Integer]?NumericQ, x_:Automatic, u_:Automatic] :=
    calcoptBLP[llf, Round[initstars], x, u];


calcoptBLP[llf_?(NumericQ @ # && 10 <= # <= 1*^6&), initstars_Integer?(0 <= # <= 1*^9&), Optional[Automatic, Automatic], Optional[u_, Automatic]] :=
    Block[{lpsi, level},
        lpsi = (llf / 25 - 1) / Log2[10];
        level = Clip[Quotient[lpsi, 20, 90], {0, 24}];
        Print["Automatic third argument used: ", level];
        calcoptBLP[llf, initstars, level, u]
    ];


calcoptBLP[llf_?(NumericQ @ # && 10 <= # <= 1*^6&), initstars_Integer?(0 <= # <= 1*^9&), ex_Integer /; 0 <= ex <= 24, u_:Automatic] :=
    calcoptBLP[llf, initstars, Prepend[Table[Clip[1 + 0.04 * (ex - 2 n), {1, 1.08}], {n, 0, 17}], 9], u];


calcoptBLP[llf_?(NumericQ @ # && 10 <= # <= 1*^6&), initstars_Integer?(0 <= # <= 1*^9&), ex_List /; Length[ex] == 19, u_Integer /; ! (2 <= u <= 19)] :=
    calcoptBLP[llf, initstars, ex, Clip[u, {2, 19}]];


calcoptBLP[llf_?(NumericQ @ # && 10 <= # <= 1*^6&), initstars_Integer?(0 <= # <= 1*^9&), ex_List /; Length[ex] == 19, Optional[Automatic, Automatic]] :=
    Block[{count = Length[Select[frontierLevel[llf, #1, #2]& @@@ priceTab1, # > 0&]]},
        count = Clip[count, {2, 19}];
        Print["Automatic unlock count used: ", count, " (better: enter the new-variable level from the Star Bonuses panel)"];
        calcoptBLP[llf, initstars, ex, count]
    ];


calcoptBLP[llf_?(NumericQ @ # && 10 <= # <= 1*^6&), initstars_Integer?(0 <= # <= 1*^9&), ex_List /; Length[ex] == 19, unlocked_Integer /; 2 <= unlocked <= 19] :=
    Block[{res},
        Print["unlocked variables: ", unlocked, " (up to ", symbTable[[unlocked]], "), next: ", If[unlocked < 19, symbTable[[unlocked + 1]], "none"]];
        res = calcoptBLPComp[llf, initstars, ex, unlocked];
        printCalcoptGrid[res, unlocked, initstars]
    ];


(* ::Section::Closed:: *)
(*Model data (extracted verbatim from the original compiled function)*)


priceA1 = {0.5, 0.625, 0.78125, 0.8931739295455593, 1.0211323755774153, 1.16742248509526, 1.3346705004190407, 1.459278106369075, 1.6315225219592102, 1.865258729062898, 2.085422656904751, 2.384185791015625, 2.725750517411985, 3.1162487047650607, 3.407188146764981, 3.9832092821269787, 4.453363361722031, 5.206251464550825, 5.820766091346741};
priceB1 = {-4.143474048424998, -3.1709615006770515, 1.732020845644619, 15.973438351860704, 35.10114920824737, 62.78631425389338, 99.28201254141936, 144.78860583366378, 199.47274109054294, 263.4775066411068, 336.92853801096174, 512.6072746595227, 727.2885715481912, 849.4650465308255, 1123.859483302632, 1438.7509481751554, 1794.6202607002178, 2191.902179662476, 2630.993842401342};
priceA2 = {2, 2.2, 2.42, 2.662, 2.9282, 3.22102, 3.543122, 3.8974342, 4.28717762, 4.715895382, 5.1874849202, 5.70623341222, 6.276856753442, 6.9045424287862, 7.59499667166483, 8.354496338831312, 9.189945972714444, 10.108940569985888, 11.119834626984478};
priceB2 = {0.6992669815829572, 0.7336524096644791, 2.7382112580862277, 16.973438351860707, 36.10114920785727, 63.786314253893394, 100.28201254141943, 145.78860583366372, 200.47274109054277, 264.47750664110686, 337.92853801096135, 513.6072746595223, 728.2885715481901, 850.4650465308251, 1124.859483302631, 1439.7509481751538, 1795.620260700215, 2192.9021796624766, 2631.993842401341};
starMult = {1, 0.27, 0.09, 0.1, 0.12, 0.17, 0.22, 0.5, 0.7, 0.9, 2, 3, 4, 4.5, 6, 7, 8, 9, 10};
priceTab1 = Transpose[{priceA1, priceB1}];
priceTab2 = Transpose[{priceA2, priceB2}];


(* ::Section::Closed:: *)
(*Core computation (plain WL equivalent of the original compiled function)*)


(* Levels already purchased of a variable, inferred from the f(t) price frontier *)
frontierLevel[llf_, a_, b_] := Max[0, Floor[((llf - Log[10, Log[10, 2]])/Log[10, 2] - b)/a + 1]];

(* Effective exponent of a variable at level l (game formula) *)
gexp[l_] := Which[l < 10, 0, l < 25, 1, l < 1500, l/25 + 1, l < 6000, l/50 + 31, l < 10000, l/100 + 91, l < 24000, l/200 + 141, True, l/400 + 201];

powFn[l_, e_] := 2.^(e*Floor[gexp[l]])*l^e;

(* Number of levels to the next power-regime boundary *)
deltaLevelFn[l_] := Which[l < 10, 10 - l, l < 25, 25 - l, l < 1500, Mod[-l, 25, 1], l < 6000, Mod[-l, 50, 1], l < 10000, Mod[-l, 100, 1], l < 24000, Mod[-l, 200, 1], True, Mod[-l, 400, 1]];

(* Star cost of dlv permanent levels starting at star-level cur *)
starCost[a_, cur_, dlv_] := Sum[Floor[a*(cur + i)*Exp[a*(cur + i)/200] + 1], {i, 0, dlv - 1}];


calcoptBLPComp[llf_, initstars_, ex_List, unlocked_] :=
    Module[{ex2, totex, exps, vars, ups, upsc, initpows, fincosts, finlevs, finpows,
        costbag, powerbag, levelbag, lengthbag, length, lengthlist, acc, costlist,
        powerlist, levellist, m, b, c, x, maxp, weakThreshold, strongFlags, stars},
        ex2 = FoldList[Times, 1, Rest[ex]];
        totex = Last[ex2];
        exps = ex2*totex^-1;
        vars = frontierLevel[llf, #1, #2]& @@@ priceTab1;
        ups = frontierLevel[llf, #1, #2] + 1& @@@ priceTab2;
        upsc = FoldList[Times, 1, Power[Rest[ups], Most[exps]]];
        initpows = MapThread[#2*powFn[#1, #3]&, {vars, upsc, exps}];
        (* variables beyond the unlocked count do not exist for this player *)
        initpows = Table[If[i <= unlocked, initpows[[i]], 0.], {i, 19}];
        fincosts = Table[0, {19}];
        finlevs = Table[0, {19}];
        finpows = Table[0., {19}];
        maxp = 0.;
        length = 0;
        costbag = Internal`Bag[Most[{1}]];
        powerbag = Internal`Bag[];
        levelbag = Internal`Bag[Most[{1}]];
        lengthbag = Internal`Bag[Most[{1}]];
        strongFlags = Table[1, {19}];

        (* ---- pass 1: one cumulative option per power regime, LP picks <= 1 per variable ---- *)
        Do[
            If[i <= unlocked,
                Block[{var$ = vars[[i]], up$ = upsc[[i]], exp$ = exps[[i]], star$ = starMult[[i]],
                    initpow$ = initpows[[i]], prevCost = 0, prevLevel = 0, deltaLevel, cost = 0, power, level},
                    While[cost < initstars,
                        deltaLevel = deltaLevelFn[var$ + prevLevel];
                        cost = prevCost + starCost[star$, prevLevel, deltaLevel];
                        level = prevLevel + deltaLevel;
                        power = up$*powFn[var$ + level, exp$] - initpow$;
                        If[cost <= initstars,
                            Internal`StuffBag[costbag, cost];
                            Internal`StuffBag[powerbag, power];
                            Internal`StuffBag[levelbag, level];
                            length++;
                            prevLevel = level;
                            prevCost = cost;
                            Null]
                    ]
                ],
                Null
            ];
            Internal`StuffBag[lengthbag, length],
            {i, 19}
        ];
        If[length > 0,
            lengthlist = Internal`BagPart[lengthbag, All];
            costlist = Internal`BagPart[costbag, All];
            powerlist = Internal`BagPart[powerbag, All];
            maxp = Max[powerlist];
            levellist = Internal`BagPart[levelbag, All];
            c = Chop[(-powerlist)/maxp];
            m = Table[0, {20}, {length}];
            acc = 1;
            Fold[(Do[m[[acc, i]] = -1, {i, #1 + 1, #2}]; acc++; #2)&, 0, lengthlist];
            m[[20]] = -costlist;
            b = Table[If[i == 20, -initstars, -1], {i, 20}];
            Print["LP size (pass 1): ", Dimensions[m]];
            x = Round[externLP[c, m, b, Automatic, Integers, Rule[Tolerance, Power[10, -9]]]];
            acc = 1;
            Fold[(Do[If[x[[i]] == 1,
                fincosts[[acc]] = costlist[[i]];
                finlevs[[acc]] = levellist[[i]];
                finpows[[acc]] = powerlist[[i]];
                Null], {i, #1 + 1, #2}]; acc++; #2)&, 0, lengthlist];
            weakThreshold = 2*Max[Table[
                If[vars[[i]] + finlevs[[i]] > 0, (initpows[[i]] + finpows[[i]])/(vars[[i]] + finlevs[[i]]), 0.], {i, 19}]];
            strongFlags = Table[Boole[initpows[[i]] + finpows[[i]] > weakThreshold], {i, 19}];
            fincosts = strongFlags*fincosts;
            finlevs = strongFlags*finlevs;
            finpows = strongFlags*finpows;
            Null
        ];

        (* ---- pass 2: refine with the remaining stars ---- *)
        costbag = Internal`Bag[Most[{1}]];
        powerbag = Internal`Bag[];
        levelbag = Internal`Bag[Most[{1}]];
        lengthbag = Internal`Bag[Most[{1}]];
        length = 0;
        stars = initstars - Total[fincosts];
        Do[
            If[i <= unlocked,
                Block[{var$ = vars[[i]], up$ = upsc[[i]], exp$ = exps[[i]], star$ = starMult[[i]],
                    initpow$ = initpows[[i]] + finpows[[i]], lev$ = finlevs[[i]], flag$ = strongFlags[[i]],
                    prevCost = 0, prevLevel = lev$, deltaLevel, cost = 0, power, level},
                    While[cost < stars,
                        If[flag$ == 0,
                            deltaLevel = deltaLevelFn[var$ + prevLevel];
                            cost = prevCost + starCost[star$, prevLevel, deltaLevel];
                            level = prevLevel + deltaLevel,
                            cost = prevCost + Floor[star$*prevLevel*Exp[star$*prevLevel/200] + 1];
                            level = prevLevel + 1
                        ];
                        power = up$*powFn[var$ + level, exp$] - initpow$;
                        If[cost <= stars,
                            Internal`StuffBag[costbag, cost];
                            Internal`StuffBag[powerbag, power];
                            Internal`StuffBag[levelbag, level - lev$];
                            length++;
                            prevLevel = level;
                            prevCost = cost;
                            Null]
                    ]
                ],
                Null
            ];
            Internal`StuffBag[lengthbag, length],
            {i, 19}
        ];
        If[length > 0,
            lengthlist = Internal`BagPart[lengthbag, All];
            costlist = Internal`BagPart[costbag, All];
            powerlist = Internal`BagPart[powerbag, All];
            levellist = Internal`BagPart[levelbag, All];
            c = Chop[(-powerlist)/Max[powerlist]];
            m = Table[0, {20}, {length}];
            acc = 1;
            Fold[(Do[m[[acc, i]] = -1, {i, #1 + 1, #2}]; acc++; #2)&, 0, lengthlist];
            m[[20]] = -costlist;
            b = Table[If[i == 20, -stars, -1], {i, 20}];
            Print["LP size (pass 2): ", Dimensions[m]];
            x = Round[externLP[c, m, b, Automatic, Integers, Rule[Tolerance, Power[10, -9]]]];
            acc = 1;
            Fold[(Do[If[x[[i]] == 1,
                fincosts[[acc]] += costlist[[i]];
                finlevs[[acc]] += levellist[[i]];
                finpows[[acc]] += powerlist[[i]];
                Null], {i, #1 + 1, #2}]; acc++; #2)&, 0, lengthlist];
            Null
        ];

        Print["dump y: ", Power[Total[initpows] + maxp, totex]];
        Print["found y: ", Power[Total[finpows] + Total[initpows], totex]];
        {fincosts, vars, finlevs, initpows, finpows}
    ];


(* ::Section::Closed:: *)
(*Result table*)


printCalcoptGrid[{fincosts_, vars_, finlevs_, initpows_, finpows_}, unlocked_, initstars_] :=
    Block[{totpow, pows, powsd, grid, len = 19, prio},
        totpow = Total[initpows + finpows];
        pows = (initpows + finpows)/totpow;
        powsd = finpows/totpow;
        Print["stars used: ", Total @ fincosts];
        Print["stars left: ", initstars - Total @ fincosts];
        prio[i_] := If[i > unlocked, "locked",
            Which[
                finlevs[[i]] == 0, "none",
                powsd[[i]] >= 0.01, "top",
                powsd[[i]] >= 0.001, "high",
                powsd[[i]] >= 0.0001, "medium",
                True, "low"
            ]];
        grid = {
            symbTable, NumberForm[#, Infinity, NumberSigns -> {"-", "+"}]& /@ finlevs, fincosts, vars, finlevs + vars, PercentForm[#, {Infinity, 4}]& /@ pows, Table[prio[i], {i, len}]
        } // Transpose;
        grid = Prepend[grid, {"variable", "\[CapitalDelta] level", "stars used", "level before", "level after", "power after", "priority"}];
        Print[Grid[
            grid, Frame -> All, FrameStyle -> Thin, ItemStyle -> {
                Automatic, Switch[Last @ #,
                    "top",
                        Hue[{0, .8, .9}]
                    ,
                    "high",
                        Hue[{55 / 360, .65, .8}]
                    ,
                    "medium",
                        Hue[{1 / 3, .6, .6}]
                    ,
                    "low",
                        Hue[{200 / 360, .1, .7}]
                    ,
                    "none",
                        RGBColor["#888"]
                    ,
                    "locked",
                        RGBColor["#555"]
                    ,
                    _,
                        Automatic
                ]& /@ grid
            }
        ]];
    ]
