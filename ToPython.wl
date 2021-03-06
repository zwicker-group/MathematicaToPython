(* ::Package:: *)

(*This package provides a function to convert a Mathematica expression to numpy
----------------------------------------------------;
INPUT ARGUMENTS;
expression: your mathematica expression, it can be numbers, literals, complexes or lists;
numpy\[LetterSpace]prefix: string defining your Numpy import prefix, e.g.:
if your used "import numpy as np", your prefix should be the string "np"
if your used "from numpy import *", your prefix should be the empty string ""
;
OUTPUT;
the Numpy python-ready expression (to be copied as a string);
!The formatted expression will be copied ot your clipboard, ready to paste on Python!;
------------------------------------------------------;
Not tested for every possible combination; use at your risk, by Gustavo Wiederhecker with
modifications by David Zwicker
*)


BeginPackage["ToPython`"]

ToPython::usage = "ToPython[expression, numpyprefix, copy]
	converts Mathematica expression to a Numpy compatible expression. Because Numpy can
	be imported in several ways, numpyprefix is a string that will be added to appended
	to function names, e.g., Cos->np.cos. If copy==True, the result is copied to the clipboard"
 
ToPythonEquation::usage = "ToPythonEquation[equation, numpyprefix, copy] converts a
    Mathematica equation to a Numpy compatible express"
 


Begin["Private`"]


(* list of function heads that do not need to be enclosed in brackets *)
singleFunctions={Log, Sin, Cos, Tan, Sinh, Cosh, Tanh};


ToPython[expression_, numpyprefix_:"np", copy_:False] := Module[
	{result, greekrule, format, PythonForm, np, br, brackets, a, b, l, m, args},

(* determine the correct numpy prefix *)
If[numpyprefix=="", np=numpyprefix, np=numpyprefix<>"."];

(* general function for formating output *)
format[pattern_String, args__] := Module[{s},
	s = StringReplace[pattern, "numpy."->np];
	ToString @ StringForm[s, Sequence @@ PythonForm /@ List[args]]
];

(* helper function deciding when to use brackets *)
br[a_] := If[AtomQ[a] || MemberQ[singleFunctions, Head[a]], a, brackets[a]];
PythonForm[brackets[a_]] := format["(``)", a];

(* special forms that are recognized *)
PythonForm[Times[-1, a_]] := format["-``", br[a]];
PythonForm[Power[a_, Rational[1, 2]]] := format["numpy.sqrt(``)", a];
PythonForm[Times[a_, Power[b_, -1]]] := format["`` / ``", br[a], br[b]];

(* Simple math *)
PythonForm[Rational[a_, b_]] := format["`` / ``", br[a], br[b]];
PythonForm[Complex[a_, b_]] := format["complex(``, ``)", a, b];
PythonForm[a_ * b__] := Module[{fs, bl={b}},
	fs = StringRiffle[ConstantArray["``", 1 + Length@bl], " * "];
	format[fs, br@a, Sequence @@ br /@  bl]
];
PythonForm[a_ + b_] := format["`` + ``", a, b];
PythonForm[Power[a_, b_]] := format["`` ** ``", br[a], br[b]];
PythonForm[Exp[a_]] := format["numpy.exp(``)", a];

(* Some special functions *)
PythonForm[Arg[a_]] := format["numpy.angle(``)", a];
PythonForm[SphericalHarmonicY[l_, m_, a_, b_]] := format[
    "special.sph_harm(``, ``, (``) % (2 * numpy.pi), (``) % numpy.pi)",
    m, l, b, a];
PythonForm[Gamma[a_]] := format["special.gamma(``)", a];
PythonForm[Gamma[a_, b_]] := format["special.gamma(`1`) * special.gammaincc(`1`, `2`)", a, b];
PythonForm[BesselI[a_, b_]] := format["special.iv(``, ``)", a, b];
PythonForm[BesselJ[a_, b_]] := format["special.jv(``, ``)", a, b];
PythonForm[BesselK[a_, b_]] := format["special.kn(``, ``)", a, b];
PythonForm[BesselY[a_, b_]] := format["special.yn(``, ``)", a, b];

(* Some functions that are not defined in numpy *)
PythonForm[Csc[a_]] := format["1 / numpy.sin(``)", a];
PythonForm[Sec[a_]] := format["1 / numpy.cos(``)", a];
PythonForm[Cot[a_]] := format["1 / numpy.tan(``)", a];
PythonForm[Csch[a_]] := format["1 / numpy.sinh(``)", a];
PythonForm[Sech[a_]] := format["1 / numpy.cosh(``)", a];
PythonForm[Coth[a_]] := format["1 / numpy.tanh(``)", a];

(* Handling arrays *)
PythonForm[a_NumericArray] :=
	np<>"array("<>StringReplace[ToString@Normal@a, {"{"-> "[", "}"-> "]"}]<>")";
PythonForm[List[args__]] :=
	np<>"array(["<>StringRiffle[PythonForm/@List[args], ", "]<>"])";

(* Constants *)
PythonForm[\[Pi]] = np<>"pi";
PythonForm[E] = np<>"e";

(* Greek characters *)
greekrule={
    "\[Alpha]"->"alpha","\[Beta]"->"beta","\[Gamma]"->"gamma","\[Delta]"->"delta",
    "\[CurlyEpsilon]"->"curlyepsilon","\[Zeta]"->"zeta","\[Eta]"->"eta",
    "\[Theta]"->"theta","\[Iota]"->"iota","\[Kappa]"->"kappa","\[Lambda]"->"lambda",
    "\[Mu]"->"mu","\[Nu]"->"nu","\[Xi]"->"xi","\[Omicron]"->"omicron","\[Pi]"->"pi",
    "\[Rho]"->"rho","\[FinalSigma]"->"finalsigma","\[Sigma]"->"sigma","\[Tau]"->"tau",
    "\[Upsilon]"->"upsilon","\[CurlyPhi]"->"curlyphi","\[Chi]"->"chi","\[Phi]" -> "phi",
    "\[Psi]"->"psi",
    "\[Omega]"->"omega","\[CapitalAlpha]"->"Alpha","\[CapitalBeta]"->"Beta",
    "\[CapitalGamma]"->"Gamma","\[CapitalDelta]"->"Delta",
    "\[CapitalEpsilon]"->"CurlyEpsilon","\[CapitalZeta]"->"Zeta",
    "\[CapitalEta]"->"Eta","\[CapitalTheta]"->"Theta","\[CapitalIota]"->"Iota",
    "\[CapitalKappa]"->"Kappa","\[CapitalLambda]"->"Lambda","\[CapitalMu]"->"Mu",
    "\[CapitalNu]"->"Nu","\[CapitalXi]"->"Xi","\[CapitalOmicron]"->"Omicron",
    "\[CapitalPi]"->"Pi","\[CapitalRho]"->"Rho","\[CapitalSigma]"->"Sigma",
    "\[CapitalTau]"->"Tau","\[CapitalUpsilon]"->"Upsilon","\[CapitalPhi]"->"CurlyPhi",
    "\[CapitalChi]"->"Chi","\[CapitalPsi]"->"Psi","\[CapitalOmega]"->"Omega"};

(* Everything else *)
PythonForm[h_[args__]] := np<>ToLowerCase[PythonForm[h]]<>"("<>PythonForm[args]<>")";
PythonForm[allOther_] := StringReplace[ToString[allOther, FortranForm], greekrule];

result = StringReplace[PythonForm[expression], greekrule];
(* Copy results to clipboard *)
If[copy, CopyToClipboard[result]];
result
]


ToPythonEquation[Equal[a_, b_], numpyprefix_:"np", copy_:True] := ToPython[a - b, numpyprefix, copy]


End[]
EndPackage[]
