/*
Here we adapt the flat parser to a structured island parser.

TODO:
- parses most of the rascal files, but contains ambiguities; need to fix these first?
- next, want to use implode() to get AST and then write a pretty printer to check results
*/

module islands::structure

import IO; // println
import String; // endsWith, intercalate
import List; // size
import util::FileSystem; // crawl

layout Whitespace = [\t-\n\r\ ]* !>> [\t-\n\r\ ] ; // greedy

lexical Char
  = "\'" CharCharacter "\'"
  ;

lexical CharCharacter
  = ![\'\\]
  | [\\] Any
  ; 

lexical String
  = "\"" StringCharacter* "\""
  ;

lexical StringCharacter
  = ![\"\\]
  | [\\] Any
  ; 

lexical Any
  = [a-z]
  |	![a-z]
  ;

// stolen from Rascal grammar
lexical Comment
	= "/*" (![*] | [*] !>> [/])* "*/" 
	| "//" ![\n]* !>> ![\n] $
	;

lexical Word
  = word: [a-zA-Z_][a-zA-Z0-9_\-]* !>> [a-zA-Z0-9_\-] // greedy
  ;

syntax Noise // numbers and operators
  = (![a-zA-Z_(){}\[\]\"\'/\t-\n\r\ ])+ !>> ![a-zA-Z_(){}\[\]\"\'/\t-\n\r\ ]
  | "/" !>> [*/]
  ;

lexical Paren = [ ( ) { } \[ \] ] ;

syntax Island
  = Word
  | Struct
  ;

syntax Struct
  = round: "(" Code ")"
  | curly: "{" Code "}"
  | square: "[" Code "]"
  ;

syntax Water
  = String
  | Char
  | Comment
  | Noise
  ;

syntax Stuff
  = Water
  | Island
  ;

start syntax Code
  = code: Stuff*
  ;

// data Code = code

/* === TESTING === */

@doc { Return (recursively) all files within a project. }
public set[loc] allFiles(loc proj) = { f | /file(loc f) := crawl(proj) };

@doc { Return all Java files in a project. }
public set[loc] javaFiles(loc proj) =
	{ f | f <- allFiles(proj), endsWith(f.path, ".java") };

public set[loc] removeRascalParser(set[loc] files) =
	{ f | f <- files, !endsWith(f.path, "RascalParser.java") }; // HACK -- avoid stack overflow

@doc { Return true/false whether all files parse. }
public bool parseFiles(set[loc] files, bool verbose=false) {
	try
		for (loc f <- files) {
			if (verbose) println(f);
			parse(#start[Code], f);
		}
	catch :
		return false;
	return true;
}

@doc { Report whcih files parse ambiguously }
public void findAmbiguous(set[loc] files) {
	try
		for (loc f <- files) {
			if (/amb(_) := parse(#start[Code], f))
				println("AMBIGUOUS: <f>");
			else
				println("NOT AMBIGUOUS: <f>");
		}
	catch :
		println("PARSE ERROR: <f>");
}

test bool testStringEmpty() = /lit("\"") := parse(#String, "\"\"");
test bool testString1Char() = /lit("\"") := parse(#String, "\"a\"");
test bool testStringEscape() = /lit("\"") := parse(#String, "\"\\n\"");


test bool testString1() = /lit("\"") := parse(#String, "\"`\"");
test bool testString2() = /lit("\"") := parse(#String, "\"\\\"\"");
test bool testString3() = /lit("\"") := parse(#String, "\"\\\"\"");
test bool testString4() = /lit("\"") := parse(#String, "\"\\\\\\\\\\\"\"");
test bool testString5() = /lit("\"") := parse(#String, "\"\<\"");
test bool testString6() = /lit("\"") := parse(#String, "\"\\\\\\\\\<\"");


test bool testComment() = /lit("//") := parse(#Comment, "// ...");
test bool testCommentQuote() = /lit("//") := parse(#Comment, "// can\'t parse?");
test bool testCommentMultiLine() = /lit("/*") := parse(#Comment, "/* ...\n* more\n */");

test bool testNoise1() = /sort("Noise") := parse(#Noise, "1");
test bool testNoise2() = /sort("Code") := parse(#start[Code], "1%^$$*^$");
test bool testNoise3() = /sort("Noise") := parse(#Noise, "/");

test bool testCodeWithCommentAndQuote1() = /sort("Code") := parse(#start[Code], "1 // can\'t parse");
test bool testWater1() = /sort("Water") := parse(#start[Code], "1");
test bool testWater2() = /sort("Water") := parse(#start[Code], "// can\'t parse");


test bool testCodeWithCommentAndQuote2() = /lex("Comment") := parse(#start[Code], "1 // can parse");

test bool testStuff0() = /sort("Stuff") := parse(#start[Code], "1 ");
test bool testStuff1() = /sort("Stuff") := parse(#Stuff, "// can\'t parse");
test bool testStuff2() = /sort("Stuff") := parse(#Stuff, "// can parse");

//ok
test bool testCodeWithCommentNoQuote() = /sort("Code") := parse(#start[Code], "1 // can parse");

test bool testChar1() = /lit("\'") := parse(#Char, "\'a\'");
test bool testChar2() = /lit("\'") := parse(#Char, "\'\"\'");
test bool testChar3() = /lit("\'") := parse(#Char, "\'\\n\'");
test bool testChar4() = /lit("\'") := parse(#Char, "\'\\\\\'");

// test bool testAmbWater1() = /sort("Water") := parse(#start[Code], "\>0;");

test bool testPackage() = /sort("Code") := parse(#start[Code], "package snakes;");

test bool testSnakes() = parseFiles(javaFiles(|project://p2-SnakesAndLadders|));

// SLOW TEST
// test bool testRascalEclipse() = parseFiles(javaFiles(|project://rascal-eclipse|));

// VERY SLOW TEST
// test bool testRascalClone() = parseFiles(removeRascalParser(javaFiles(|project://rascal-clone|)));

/* === DEBUGGING === */

@doc { Return a minimal substring that gives a parse error. }
public str minErr(type[&T<:Tree] begin, loc input) {
	try
		parse(begin, input);
	catch :
		return intercalate("\n", binSearchErrs(begin, readFileLines(input)));
	println("No errors in this file");
	throw("No errors in this file");
}

@doc { Perform binary search to find smallest sublist of lines giving a parse error. }
private list[str] binSearchErrs(type[&T<:Tree] begin, list[str] input) {
	int len = size(input);
	if (len == 1) return input;
	int mid = len/2;
	list[str] low = slice(input,0,mid);
	list[str] high = slice(input,mid,len-mid);
	assert(low+high == input);
	try
		parse(begin, intercalate("\n",low));
	catch :
		return binSearchErrs(begin, low);
	try
		parse(begin, intercalate("\n",high));
	catch :
		return binSearchErrs(begin, high);
	return input; // failed to find a substring with the error
}

/*

parseFiles(javaFiles(|project://rascal-eclipse|), verbose=true);
parseFiles(removeRascalParser(javaFiles(|project://rascal-clone|)), verbose=true);

parseFiles(javaFiles(|project://p2-SnakesAndLadders|), verbose=true);

parse(#start[Code], |project://p2-SnakesAndLadders/src/snakes/Player.java|);
minErr(#start[Code], |project://p2-SnakesAndLadders/src/snakes/Player.java|);

import util::ValueUI;
text(parse(#start[Code], |project://p2-SnakesAndLadders/src/snakes/Player.java|));

import Ambiguity;
diagnose(parse(#start[Code], |project://p2-SnakesAndLadders/src/snakes/Player.java|));


This file is very slow to parse:
|project://rascal-clone/src/org/rascalmpl/library/vis/figure/graph/lattice/LatticeGraphNode.java|

*/

/*
findAmbiguous(javaFiles(|project://p2-SnakesAndLadders|));

import ParseTree;
loc f;
f = |project://p2-SnakesAndLadders/src/snakes/Die.java|;
pt = parse(#start[Code], f);
/amb(_) := pt;

import Ambiguity;
diagnose(pt);

ast = implode(#value, pt);
ast = implode(#node, pt);


import  vis::ParseTree;
renderParsetree(pt);

*/
