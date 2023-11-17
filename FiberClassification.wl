(* ::Package:: *)

BeginPackage["FiberClassification`"];


Needs["Singular`"];


MorphismImage::usage="Effective Chevalley's Theorem on Images of Morphisms";


FiberClassify::usage="Fiber Classification";


Begin["`Private`"];


(*The following part about LC, LM, LT is taken from https://library.wolfram.com/infocenter/MathSource/4489/#downloads by Garry Helzer, with some modifications*)


Multidegree[t_,vars_]:=Exponent[t,#]&/@vars;


IdealContainment[f_,g_,y_]:=Module[{GB},
	GB=GroebnerBasis[g,y,MonomialOrder->DegreeReverseLexicographic];
	Return[Length[Select[f,PolynomialReduce[#,GB,y,MonomialOrder-> DegreeReverseLexicographic][[2]]=!=0&]]==0]
];


LeadingTerm[vars_,ord_][p_]:=
 Module[{A=ord[vars]},
  Return[First@Sort[MonomialList[Collect[p,vars],vars],
      OrderedQ[{Multidegree[#2,vars].A,
              Multidegree[#1,vars].A}]&]];
]


BreakoutMonomial[vars_,phldr_][m_]:=Module[{v,w},
  {v,w}=Multidegree[m,#]&/@{vars,phldr};
  v=Inner[Power,vars,v,Times];
  w=Inner[Power,phldr,w,Times];
  Return[{Coefficient[m,v],v/w,w}];
]


BreakoutLead[vars_,phldr_,ord_][p_]:=
 Module[{A=ord[vars],l},
  l=LeadingTerm[vars,ord][p];
  Return[BreakoutMonomial[vars,phldr][l]];
]


LeadingCoefficient[vars_,ord_][p_]:=First@BreakoutLead[vars,{},ord][p];


LeadingMonomial[vars_,ord_][p_]:=Times@@(BreakoutLead[vars,{},ord][p])[[{2,3}]];


lex[vars_]:=
     IdentityMatrix[Length[Flatten[vars]]]

ColJoin[a_,b_]:=Transpose[Join@@Transpose/@{a,b}]

ColDrop[a_,n_]:=Transpose[Drop[Transpose[a],n]]

deglex[vars_]:=
 With[{fvars=Flatten[vars]},
 If[Length[fvars]==1,{{1}},
  ColJoin[Table[1,{Length[fvars]},{1}],
         ColDrop[IdentityMatrix[Length[fvars]],-1]]
  ]
  ]
               
degrevlex[vars_]:=
 With[{fvars=Flatten[vars]},
 If[Length[fvars]==1,{{1}},
  ColJoin[Table[1,{Length[fvars]},{1}],
   -ColDrop[
      Reverse[IdentityMatrix[Length[fvars]]],-1]]
 ]
 ]
   
grlex[vars_]:=deglex[vars]

grevlex[vars_]:=degrevlex[vars]

ProductOrder[ord1_,ord2_][vars_]:=
  Module[{A=ord1[vars[[1]] ],B=ord2[vars[[2]] ],
   a,b,p,q},
   {a,p}=Dimensions[A];{b,q}=Dimensions[B];
   Join[ColJoin[A,Table[0,{a},{q}]], 
         ColJoin[Table[0,{b},{p}],B]]
  ]


(*End of LC, LT, LM*)


(*Takes a Groebner Basis GB w.r.t. grevlex in x, return True if the ideal is zero-dimensional*)
IsZeroDimensional[GB_,x_]:=Module[{lms},
	lms=LeadingMonomial[x,grevlex][#]&/@GB;
	Return[SubsetQ[Variables/@lms,{#}&/@x]];
];


(*Return the Groebner staircase of a zero-dimensional ideal*)
MonomialBasis[GB_,x_]:=Module[{lms,univmono,mdeg,tmpvars,expbase},
	lms=LeadingMonomial[x,grevlex][#]&/@GB;
	univmono=Select[lms,Length[Variables[#]]==1&];
	mdeg=Min/@Table[Exponent[#,x[[i]]]&/@Select[univmono,Variables[#]=={x[[i]]}&],{i,1,Length[x]}];
	tmpvars=Array[a,Length[x]];
	expbase=Flatten[Table@@Join[{tmpvars},Table[{tmpvars[[i]],0,mdeg[[i]]-1},{i,1,Length[x]}]],Length[x]-1];
	expbase=Select[expbase,And@@Table[Min@@(#-Multidegree[lms[[i]],x])<0,{i,1,Length[lms]}]&];
	Return[Inner[Power,x,#,Times]&/@expbase];
];


(*Compute the trace of multiplication by f in quotient ring*)
TraceMap[GB_,f_,B_,x_]:=Module[{},
	Return[Simplify[Plus@@Table[If[MemberQ[First/@(CoefficientRules[PolynomialReduce[f*B[[i]],GB,x,MonomialOrder->DegreeReverseLexicographic][[2]],x]),Multidegree[B[[i]],x]], Multidegree[B[[i]],x]/.CoefficientRules[PolynomialReduce[f*B[[i]],GB,x,MonomialOrder->DegreeReverseLexicographic][[2]],x],0],{i,1,Length[B]}]]];
	(*Coefficient[] cannot extract the constant term, we need a discussion here*)
]


MorphismImage[f_,y_,x_]:=Module[{MatMonoOrder,IGB,JGB,lcs,ret},
	MatMonoOrder=Transpose[ProductOrder[grevlex,grevlex][{x,y}]];
	GB=GroebnerBasis[f,Join[x,y],MonomialOrder->MatMonoOrder,Method->"GroebnerWalk"];
	If[GB=={1},Return[{}]];
	JGB=Select[GB,SubsetQ[y,Variables[#]]&];
	IGB=Select[GB,Not[MemberQ[JGB,#]]&];
	lcs=Select[LeadingCoefficient[x,grevlex][#]&/@IGB,Length[Variables[#]]>0&];
	ret={{JGB,{Times@@lcs}}};
	Return[Join[ret,Join@@(MorphismImage[Append[f,#],y,x]&/@lcs)]];
]


LocallyClosedSetIntersect[L1_,L2_]:=Return[{Join[L1[[1]],L2[[1]]],Flatten[Outer[Times,L1[[2]],L2[[2]]]]}];


ConstructibleSetComplement[L]:=Module[{ret1,ret2},
	If[Length[L]==1,
		Return[{{L[[1,2]],{1}},{{},L[[1,1]]}}],
		ret1=ConstructibleSetComplement[L[[1;;-1]]];
		ret2=ConstructibleSetComplement[L[[-1]]];
		Return[Outer[LocallyClosedSetIntersect,ret1,ret2]];
	]
]


FiberClassify[f_,g_,h_,y_,x_]:=Module[{MatMonoOrder,GB,IGB,JGB,ret,Js,w,B,H,minors,P,N,ret1,ret2,z},
	PrintTemporary["Fib Class",f,",",g];
	MatMonoOrder=Transpose[ProductOrder[grevlex,grevlex][{x,y}]];
	GB=GroebnerBasis[f,Join[x,y],MonomialOrder->MatMonoOrder,Method->"GroebnerWalk"];
	If[GB=={1},Return[{{g,{1},0}}]];
	JGB=Select[GB,SubsetQ[y,Variables[#]]&];
	IGB=Select[GB,Not[MemberQ[JGB,#]]&];
	PrintTemporary["JGB=",JGB,"; IGB=",IGB];
	If[Not[IdealContainment[JGB,g,y]],
		Js=SingularMinAssGTZ[JGB,y];
		Return[Prepend[Join@@(FiberClassify[Join[f,#],#,h,y,x]&/@Js),{g,JGB,0}]];
	];
	w=Times@@(LeadingCoefficient[x,grevlex][#]&/@IGB);
	PrintTemporary["w=",w];
	If[Not[IsZeroDimensional[IGB,x]],
		If[Length[Variables[h]]==0,
			ret={{g,{w},Infinity}};
			Js=SingularMinAssGTZ[Append[JGB,w],y];
			Return[Join[ret,Join@@(FiberClassify[Join[GB,#],#,h,y,x]&/@Js)]]
			,
			Return[FiberClassify[Append[f,z*h-1],g,1,y,Append[x,z]]];
		]
	];
	B=MonomialBasis[IGB,x];
	PrintTemporary["Staircase=",B];
	H=Table[TraceMap[IGB,B[[i]]*B[[j]]*h,B,x],{i,1,Length[B]},{j,1,Length[B]}];
	PrintTemporary["H=",H];
	minors=Table[GroebnerBasis[Join[JGB,Numerator[Together[Flatten[Minors[H,i]]]]],y,MonomialOrder->DegreeReverseLexicographic],{i,1,Length[B]+1}];
	PrintTemporary["Minors Computation Done"];
	ret=Reverse[Prepend[Table[{minors[[i+1]],minors[[i]]*w,i},{i,1,Length[B]}],{minors[[1]],{w},0}]];
	Js=SingularMinAssGTZ[Append[JGB,w],y];
	Return[Join[ret,Join@@(FiberClassify[Join[f,#],#,h,y,x]&/@Js)]]
]


End[];


EndPackage[];
