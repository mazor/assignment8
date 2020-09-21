// Part 1 about Regular Expression Matching
//==========================================


object CW8a {

abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp   // alternative 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp   // sequence
case class STAR(r: Rexp) extends Rexp             // star


// some convenience for typing in regular expressions

import scala.language.implicitConversions    
import scala.language.reflectiveCalls 

def charlist2rexp(s: List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}
implicit def string2rexp(s: String): Rexp = charlist2rexp(s.toList)

implicit def RexpOps (r: Rexp) = new {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp) = SEQ(r, s)
}

implicit def stringOps (s: String) = new {
  def | (r: Rexp) = ALT(s, r)
  def | (r: String) = ALT(s, r)
  def % = STAR(s)
  def ~ (r: Rexp) = SEQ(s, r)
  def ~ (r: String) = SEQ(s, r)
}

// (1a) Complete the function nullable according to
// the definition given in the coursework; this 
// function checks whether a regular expression
// can match the empty string and Returns a boolean
// accordingly.

//def nullable (r: Rexp) : Boolean = ...

def nullable (r: Rexp) : Boolean =r match {
  case  ZERO => false //if r is zero then false
  case  ONE => true //if r is one then true
  case  CHAR(_) => false //if r is a char then false 
  case  ALT(r1,r2) => ((nullable(r1)) || (nullable(r2)) ) //if r is alt(r1,r2) then the result of 'or' on nullable(r1) and nullable(r2)
  case SEQ(r1,r2)  =>  ((nullable(r1)) && (nullable(r2))) //if r is seq(r1,r2) then the result of 'and' on nullable(r1) and nullable(r2)
  case  STAR(_) => true // if star then true

}

// (1b) Complete the function der according to
// the definition given in the coursework; this
// function calculates the derivative of a 
// regular expression w.r.t. a character.

//def der (c: Char, r: Rexp) : Rexp = ...
def der (c: Char, r: Rexp) : Rexp = r match {

case ZERO => ZERO
case ONE => ZERO
case CHAR(d) => if (c==d) ONE else ZERO
case  ALT(r1,r2) => ALT((der(c, r1)), (der(c, r2)))
case SEQ(r1,r2)  =>   if (nullable(r1)) ALT((SEQ((der (c, r1)),r2)),(der (c, r2))) else SEQ((der(c, r1)),r2)
case  STAR(a)=> SEQ(der(c,a),STAR(a))

}

// (1c) Complete the simp function according to
// the specification given in the coursework; this
// function simplifies a regular expression from
// the inside out, like you would simplify arithmetic 
// expressions; however it does not simplify inside 
// STAR-regular expressions.

//def simp(r: Rexp) : Rexp = ... 

def simp(r: Rexp) : Rexp =r match{

case SEQ(a, ZERO) => ZERO
case SEQ(ZERO, a) => ZERO
case SEQ(a, ONE) => simp(a)
case SEQ(ONE, a) => simp(a)
case SEQ(b, a) if (b==a)=> simp(a)
//case SEQ(b, c) if ((simp(b)!=b)||(simp(c)!=c))=> simp(SEQ(simp(b),simp(c)))
case SEQ(b, c) =>{
val temp1=simp(b);
val temp2 = simp(c);
if ((temp1!=b)||(temp2!=c)) simp(SEQ(temp1,temp2))
else SEQ(temp1,temp2);
}
//case SEQ(b, a) => SEQ(simp(b),simp(a))
case ALT(a, ZERO) => simp(a)
case ALT(ZERO, a) => simp(a)
case ALT(a, ONE) if(simp(a)!=a) => simp(ALT(simp(a),ONE))
case ALT(ONE, a) if(simp(a)!=a)=> simp(ALT(ONE,simp(a)))
case ALT(b, c) if (b==c) =>  simp(b) //should be ALT(b,b) but gives compiler error
//case ALT(b, c) if ((simp(b)!=b)||(simp(c)!=c))=>  simp(ALT(simp(b),simp(c)))
case ALT(b, c) =>{
val temp1=simp(b);
val temp2 = simp(c);
if ((temp1!=b)||(temp2!=c)) simp(ALT(temp1,temp2))
else ALT(temp1,temp2);
}
//case ALT(b, c)=>  ALT(simp(b),simp(c))
case _ => r

}

// (1d) Complete the two functions below; the first 
// calculates the derivative w.r.t. a string; the second
// is the regular expression matcher taking a regular
// expression and a string and checks whether the
// string matches the regular expression

//def ders (s: List[Char], r: Rexp) : Rexp = ... 
def ders (s: List[Char], r: Rexp)  : Rexp = s match{

case Nil => r 
case (c :: cs) => ders(cs, (simp(der(c, r))))

}


//def matcher(r: Rexp, s: String): Boolean = ...

def matcher(r: Rexp, s: String): Boolean = {

val sList = s.toList;
val temp = ders(sList, r);
nullable(temp);

}


// (1e) Complete the size function for regular
// expressions according to the specification 
// given in the coursework.

//def size(r: Rexp): Int = ...
def size(r: Rexp): Int = r match {

case ZERO => 1
case ONE => 1
case CHAR(_) => 1
case ALT(r1,r2) => 1 + size(r1) + size(r2)
case SEQ(r1,r2) => 1 + size(r1) + size(r2)
case STAR(r0) => 1 +size(r0)

}

// some testing data

/*
matcher(("a" ~ "b") ~ "c", "abc")  // => true
matcher(("a" ~ "b") ~ "c", "ab")   // => false

// the supposedly 'evil' regular expression (a*)* b
val EVIL = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

matcher(EVIL, "a" * 1000 ++ "b")   // => true
matcher(EVIL, "a" * 1000)          // => false

// size without simplifications
size(der('a', der('a', EVIL)))             // => 28
size(der('a', der('a', der('a', EVIL))))   // => 58

// size with simplification
size(simp(der('a', der('a', EVIL))))           // => 8
size(simp(der('a', der('a', der('a', EVIL))))) // => 8

// Java needs around 30 seconds for matching 28 a's with EVIL. 
//
// Lets see how long it takes to match strings with 
// 0.5 Million a's...it should be in the range of some
// seconds.

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start)/(i * 1.0e9)
}

for (i <- 0 to 5000000 by 500000) {
  println(i + " " + "%.5f".format(time_needed(2, matcher(EVIL, "a" * i))))
}

*/
def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start)/(i * 1.0e9)
}

/*
def main(args: Array[String]){

val EVIL = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'));
println( simp(ALT((CHAR('a') | ZERO) ~ ONE,
          ((ONE | CHAR('b')) | CHAR('c')) ~ (CHAR('d') ~ ZERO))) == CHAR('a'));
		  
		  println( simp(SEQ(ZERO|ZERO, CHAR('a')))==ZERO);
		  println(simp(ZERO | ONE) == ONE);
 println(simp(STAR(ZERO | ONE)) == STAR(ZERO | ONE));
 println(simp(ONE ~ (ONE ~ (ONE ~ CHAR('a')))) == CHAR('a'));
 println(simp(ONE ~ (ONE ~ (ONE ~ ZERO))) == ZERO);
 println(simp(ALT(ONE ~ (ONE ~ (ONE ~ ZERO)), CHAR('a'))) == CHAR('a'));
 println(simp(CHAR('a') | CHAR('a')) == CHAR('a'));
 println(simp(ONE | CHAR('a')) == (ONE | CHAR('a')));
 println(simp(ALT((CHAR('a') | ZERO) ~ ONE,
          ((ONE | CHAR('b')) | CHAR('c')) ~ (CHAR('d') ~ ZERO))) == CHAR('a'));
		  println(simp(SEQ(ZERO,CHAR('a')))==ZERO);
		   println(nullable(ZERO) == false);
 println( nullable(ONE) == true);
  println(nullable(CHAR('a')) == false);
  println(nullable(ZERO | ONE) == true);
  println(nullable(ZERO | CHAR('a')) == false);
 println( nullable(ONE ~  ONE) == true);
 println( nullable(ONE ~ CHAR('a')) == false);
  println(nullable(STAR(ZERO)) == true);
 println( der('a', ZERO | ONE) == (ZERO | ZERO));
 println(der('a', (CHAR('a') | ONE) ~ CHAR('a')) );
  println(der('a', (CHAR('a') | ONE) ~ CHAR('a')) == ALT((ONE | ZERO) ~ CHAR('a'), ONE));
 println( der('a', STAR(CHAR('a'))) == (ONE ~ STAR(CHAR('a'))));
  println(der('b', STAR(CHAR('a'))) == (ZERO ~ STAR(CHAR('a'))));
  println(ders(List.fill(5)('a'),EVIL) == SEQ(SEQ(STAR(CHAR('a')),STAR(STAR(CHAR('a')))),CHAR('b')));
  
 
println( ders(List('b'),EVIL) == ONE);
 println( ders(List('b','b'),EVIL) == ZERO);
  println(matcher(EVIL, "a" * 5 ++ "b") == true);
  println(matcher(EVIL, "b") == true);
 println( matcher(EVIL, "bb") == false);
 println( matcher("abc", "abc") == true);
  println(matcher(("ab" | "a") ~ (ONE | "bc"), "abc") == true);
 println( matcher(ONE, "") == true);
  println(matcher(ZERO, "") == false);
  println(matcher(ONE | CHAR('a'), "") == true);
  println(matcher(ONE | CHAR('a'), "a") == true);
 
 println("test data from comments");
 println(matcher(("a" ~ "b") ~ "c", "abc")==true);  // => true
println(matcher(("a" ~ "b") ~ "c", "ab")==false);   // => false
val EVIL0 = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'));
 println(size(der('a', der('a', EVIL0))) == 28);
 println(size(der('a', der('a', der('a', EVIL0)))) == 58);
 println(size(ders("aaaaaa".toList, EVIL0)));
println(size(ders("aaaaaa".toList, EVIL0)) == 8);



// the supposedly 'evil' regular expression (a*)* b
 println(ders(List.fill(5)('a'),EVIL) == SEQ(SEQ(STAR(CHAR('a')),STAR(STAR(CHAR('a')))),CHAR('b')));
  println(ders(List.fill(5)('a'),EVIL));
 println(ders(List('b'),EVIL) == ONE);
 println(ders(List('b','b'),EVIL) == ZERO);
 println(matcher(EVIL, "a" * 5 ++ "b") == true);
 println(matcher(EVIL, "b") == true);
 println(matcher(EVIL, "bb") == false);
 println(matcher("abc", "abc") == true);

 println(matcher(ONE, "") == true);
 println(matcher(ZERO, "") == false);
println( matcher(ONE | CHAR('a'), "") == true);
 println(matcher(ONE | CHAR('a'), "a") == true);
 println(matcher(("a" ~ "b") ~ "c", "abc") ==true);
  println(matcher(("a" ~ "b") ~ "c", "ab") ==false);
  println(matcher(("ab" | "a") ~ (ONE | "bc"), "abc") == true);


println(matcher(EVIL, "a" * 1000 ++ "b"));   // => true
println(matcher(EVIL, "a" * 1000)    ==false);      // => false

println(size(der('a', der('a', EVIL)))    ==28);         // => 28
println(size(der('a', der('a', der('a', EVIL)))) ==58);  // => 58

// size with simplification
println(size(simp(der('a', der('a', EVIL))))  ==8);         // => 8
println(size(simp(der('a', der('a', der('a', EVIL))))) ==8); // => 8
println("hi");
println(matcher(EVIL, "a" * 1000000 ++ "b"));
for (i <- 0 to 5000000 by 500000) {
  println(i + " " + "%.5f".format(time_needed(2, matcher(EVIL, "a" * i))))
}
}*/
}
