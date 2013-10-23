/*
Here we just try to recognize Java files as flat sequences of "Stuff".

TODO: Trying to implode the cod einto a list of words, but this sodes not work.

*/

module islands::flat

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

lexical Comment
	= "/*" (![*] | [*] !>> [/])* "*/" 
	| "//" ![\n]* !>> ![\n] $
	;

lexical Word
  = [a-zA-Z_][a-zA-Z0-9_\-]* !>> [a-zA-Z0-9_\-] // greedy
  ;

lexical Noise // numbers and operators
  = (![a-zA-Z_(){}\[\]\"\'/])+ !>> ![a-zA-Z_(){}\[\]\"\'/]
  | "/" !>> [*/]
  ;

lexical Paren = [ ( ) { } \[ \] ] ;

syntax Stuff
  = String
  | Char
  | Comment
  | Word
  | Noise
  | Paren // flat, no structure
  ;

start syntax Code
  = Stuff+
  ;

data Code = code(list[Word]);
data Word = word(str);

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

// parse error
test bool testCodeWithCommentAndQuote1() = /sort("Code") := parse(#start[Code], "1 // can\'t parse");

// should pass
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

private loc snakes = |project://p2-SnakesAndLadders|;
test bool testSnakes() = parseFiles(javaFiles(snakes));

// SLOW TEST
test bool testRascalEclipse() = parseFiles(javaFiles(|project://rascal-eclipse|));

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
NOT USEFUL
@doc { Return a minimal substring that shows ambiguity. }
public str minAmb(type[&T<:Tree] begin, loc input) {
	if(/amb(_) := parse(begin, input)) {
		return intercalate("\n", binSearchAmbiguity(begin, readFileLines(input)));
	} else {
		println("No ambiguity in this file");
		throw("No ambiguity in this file");
	}
}

@doc { Perform binary search to find smallest ambiguous sublist of lines. }
private list[str] binSearchAmbiguity(type[&T<:Tree] begin, list[str] input) {
	int len = size(input);
	if (len == 1) return input;
	int mid = len/2;
	list[str] low = slice(input,0,mid);
	list[str] high = slice(input,mid,len-mid);
	assert(low+high == input);
	if(/amb(_) := parse(begin, intercalate("\n",low)))
		return binSearchErrs(begin, low);
	if(/amb(_) := parse(begin, intercalate("\n",high)))
		return binSearchErrs(begin, high);
	return input; // failed to find a substring with the error
}
*/

/*

parseFiles(removeRascalParser(javaFiles(|project://rascal-clone|)), verbose=true);

CAUGHT THESE PROBLEM CASES:

minErr(#start[Code], |project://rascal-clone/src/org/rascalmpl/library/util/SystemAPI.java|);

			IList r = readLines(a, "`", "\"", "\"", "\\\\\"", "<", "\\\\<",


parse(#start[Code], |project://rascal-clone/src/org/rascalmpl/tutor/TutorHttpServlet.java|);
minErr(#start[Code], |project://rascal-clone/src/org/rascalmpl/tutor/TutorHttpServlet.java|);

str: "\t   *  \'\\\"\' and the escape character itself."


STACK OVERFLOW

|project://rascal-clone/src/org/rascalmpl/library/lang/rascal/syntax/RascalParser.java|

minErr(#start[Code], |project://rascal-clone/src/org/rascalmpl/library/lang/rascal/syntax/RascalParser.java|);


*/

/*
loc snakes = |project://p2-SnakesAndLadders|;
for (f <- javaFiles(snakes)) {
	pt = parse(#start[Code], f);
	if (/amb(_) := pt) {
		println("AMBIGUOUS: <f>");
	}
}

import Ambiguity;

f = |project://p2-SnakesAndLadders/src/snakes/Player.java|;
pt = parse(#start[Code], f);
implode(#value, pt);
implode(#Code, pt);

diagnose(pt);

eg = minAmb(#start[Code], f);


*/

