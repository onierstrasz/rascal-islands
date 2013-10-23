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
import Node; // getChildren
import util::FileSystem; // crawl
import  vis::ParseTree; // renderParsetree

layout Whitespace = [\t-\n\r\ ]* !>> [\t-\n\r\ ] ; // greedy

// LEXICAL

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

lexical Comment
	= "/*" (![*] | [*] !>> [/])* "*/" 
	| "//" ![\n]* !>> ![\n] $
	;

lexical Word
  = word: [a-zA-Z_][a-zA-Z0-9_\-]* !>> [a-zA-Z0-9_\-] // greedy
  ;

// WHOOPS! -- this must be lexical, not syntax!
lexical Noise // numbers and operators
  = (![a-zA-Z_(){}\[\]\"\'/])+ !>> ![a-zA-Z_(){}\[\]\"\'/]
  | "/" !>> [*/]
  ;

// SYNTAX

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

//public set[loc] removeRascalParser(set[loc] files) =
//	{ f | f <- files, !endsWith(f.path, "RascalParser.java") }; // HACK -- avoid stack overflow

@doc { Return true/false whether all files parse unambiguously. }
public bool parseFiles(set[loc] files) {
	for (loc f <- files) {
		try {
			if (/amb(_) := parse(#start[Code], f))
				return false;
		} catch :
			return false;
	}
	return true;
}

public bool unambiguous(str src) = !(/amb(_) := parse(#start[Code], src));
public bool unambiguous(loc src) = !(/amb(_) := parse(#start[Code], src));

public bool parses(str src) {
	try
		parse(#start[Code], src);
	catch :
		return false;
	return true;
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

test bool testNoise1() = /lex("Noise") := parse(#Noise, "1");
test bool testNoise2() = /sort("Code") := parse(#start[Code], "1%^$$*^$");
test bool testNoise3() = /lex("Noise") := parse(#Noise, "/");

test bool testCode0() = unambiguous("");
test bool testCode1() = unambiguous("int x");
test bool testCode2() = unambiguous(";");
test bool testCode3() = unambiguous(":=");
test bool testCode3a() = unambiguous(":= ");
test bool testCode3b() = parses(":= ");
test bool testCode4() = unambiguous(": =");
test bool testCode5() = unambiguous("int FACES = 6;"); 
test bool testCode6() = unambiguous("1"); 
test bool testCode6a() = unambiguous("1 "); 
test bool testCode7() = unambiguous("1 //"); 
test bool testCode7a() = parses("1 //"); 
test bool testCode8() = unambiguous("//"); 
test bool testCode9() = unambiguous("1 // comment");
test bool testCode9a() = parses("1 // comment"); 


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

@doc { Report which files parse ambiguously }
public void findAmbiguous(set[loc] files, bool verbose=false) {
	for (loc f <- files) {
		try
			if (/amb(_) := parse(#start[Code], f))
				println("AMBIGUOUS: <f> (<sloc(f)>)");
			else
				if (verbose)
					println("OK: <f> (<sloc(f)>)");
		catch :
			println("PARSE ERROR: <f> (<sloc(f)>)");
	}
}

@doc { Returns count of Source Lines of Code for a method or other source entity. }
// public int sloc(loc src) = 1 + src.end.line - src.begin.line;
public int sloc(loc src) = size(readFileLines(src));

public void showTree(str src) = renderParsetree(parse(#start[Code], src));
public void showTree(loc src) = renderParsetree(parse(#start[Code], src));

/*
parseFiles(javaFiles(|project://rascal-eclipse|), verbose=true);
parseFiles(javaFiles(|project://rascal-clone|), verbose=true);

parseFiles(javaFiles(|project://p2-SnakesAndLadders|), verbose=true);

parse(#start[Code], |project://p2-SnakesAndLadders/src/snakes/Player.java|);
minErr(#start[Code], |project://p2-SnakesAndLadders/src/snakes/Player.java|);

import util::ValueUI;
text(parse(#start[Code], |project://p2-SnakesAndLadders/src/snakes/Player.java|));

import Ambiguity;
diagnose(parse(#start[Code], |project://p2-SnakesAndLadders/src/snakes/Player.java|));

*/

/*
findAmbiguous(javaFiles(|project://p2-SnakesAndLadders|), verbose=true);
findAmbiguous(javaFiles(|project://p2-SnakesAndLadders|));
findAmbiguous(javaFiles(|project://rascal-eclipse|));
findAmbiguous(javaFiles(|project://rascal-clone|));

import ParseTree;
loc f;
f = |project://p2-SnakesAndLadders/src/snakes/Die.java|;
pt = parse(#start[Code], f);
/amb(_) := pt;

import  vis::ParseTree;
renderParsetree(pt);

import Ambiguity;
diagnose(pt);

ast = implode(#value, pt);
ast = implode(#node, pt);

rascal>if(/(Word)`<Word w>` := pt) println("<w>");
package
ok

*/

public void pretty(Tree pt) {
	switch(pt) {
		case (Word) `<Word w>` :
			print("<w> ");
		case (Struct) `{ <Code c> }` :
			print("{ <pretty(c)> }");
		default :
			for (val <- getChildren(pt)) {
				switch (val) {
				case Tree subtree:
					pretty(subtree);
				default :
					println("UNKNOWN <val>");
				}
			}
	}
}
