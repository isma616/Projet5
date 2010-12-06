//package project5

/**
 * Premiere Partie
 */

object streams {
  /**
   * Fonctions utiles du cours pour l'exercise 1
   */

  def partialSums(s: Stream[Double]): Stream[Double] =
    Stream.cons(s.head, partialSums(s.tail) map { _ + s.head })


  def eulerTransform(s: Stream[Double]): Stream[Double] = {
    val s0 = s apply 0
    val s1 = s apply 1
    val s2 = s apply 2
    Stream.cons(s2 - ((s2 - s1) * (s2 - s1)) / (s0 - 2*s1 + s2), eulerTransform(s.tail))
  }

  def tableau(transform: Stream[Double] => Stream[Double], s: Stream[Double]): Stream[Stream[Double]] =
    Stream.cons(s, tableau(transform, transform(s)))


  /**
   * Exercise 1
   */

  def ln2Summands(n: Int): Stream[Double] = {
	  Stream.cons(1.0 / n, ln2Summands(n + 1) map {x => -x})
  }

  val ln2 = partialSums(ln2Summands(1))

  val ln2Euler = eulerTransform(ln2)

  val ln2Tableau = tableau(eulerTransform, ln2)



  /**
   * Exercise 2
   */

  def fromTo(lower: Int, upper: Int): Stream[Int] = {
	  if(lower > upper) Stream.empty
	  else 				Stream.cons(lower, fromTo(lower + 1, upper))
  }

  def intPairs: Stream[(Int, Int)] = {
	  for (j <- Stream.from(0); i <- Stream.from(0))
		  yield (i, j)
  }

  def intPairs2: Stream[(Int, Int)] = {
	  def intPairs20(m: Int, n: Int): Stream[(Int, Int)] = {
	 	  Stream.cons((m, n), intPairs20(m + 1, n))
	  }
	  def intPairs21(n: Int): Stream[Stream[(Int, Int)]] = {
	 	  Stream.cons(intPairs20(0, n), intPairs21(n + 1))
	  }
	   
	  intPairs21(0).flatten;
  }



  /**
   * Exercise 3
   */

  def countStream(s: Stream[Int]): Stream[Int] = {
	  def countStream0(s: Stream[Int], current: Int, count: Int): Stream[Int] = {
	 	  if (s.isEmpty)
	 	 	  Stream.cons(count, Stream.cons(current, Stream.empty));
	 	  else if (s.head == current)
	 		  countStream0(s.tail, current, count + 1)
	 	  else
	 	      Stream.cons(count, Stream.cons(current, 
	 	      countStream0(s.tail, s.head, 1)
	 	      ))
	  }
	  countStream0(s.tail, s.head, 1);
  }

  def allCountStreams: Stream[Stream[Int]] = {
	  def allCountStreams0(s: Stream[Int]): Stream[Stream[Int]] = {
	 	  Stream.cons(s, allCountStreams0(countStream(s)))
	  }
	  allCountStreams0(Stream.cons(1, Stream.empty))
  }

  def main(args: Array[String]) {
	
	  /* Testing Exercise 1 */
	  print("ln2 = "); ln2 take 10 print;
	  println;
	   
	  print("ln2 Euler = "); ln2Euler take 10 print;
	  println;
	   
	  //calculate the number of iterations needed to get the 10e decimal of ln(2) using Euler formula  
	  def isGood(n: Int, s: Stream[Double]) {	 	 
	 	  if (Math.abs(s.head - s.tail.head) < 0.000000001)
	 	 	  println("Needs " + n + " iterations to get it.")
	 	  else isGood(n + 1, s.tail)
	  }
	  
	  isGood(1, ln2Euler)

	  print("ln2 Tableau = "); ln2Tableau take 10 print;
	  println;
	   
	  /* Testing Exercise 2 */
	  intPairs  take 20 print;
	  println;
		
	  intPairs2 take 20 print;
	  println;
	  
	  /* Testing Exercise 3 */
	  print("count streams: "); allCountStreams take 10 map (_.mkString("-")) print;
	  println;
  }
}


/**
 * Seconde Partie
 */

object solver {

  trait Constraint {
    def newValue: Unit
    def dropValue: Unit
  }

  object NoConstraint extends Constraint {
    def newValue { error("NoConstraint.newValue") }
    def dropValue { error("NoConstraint.dropValue") }
  }

  case class Adder(a1: Quantity, a2: Quantity, sum: Quantity) extends Constraint {
    def newValue {
      (a1.getValue, a2.getValue, sum.getValue) match {
        case (Some(x1), Some(x2), _      ) => sum.setValue(x1 + x2, this)
        case (Some(x1), _       , Some(r)) => a2.setValue(r - x1, this)
        case (_       , Some(x2), Some(r)) => a1.setValue(r - x2, this)
        case _                             =>
      }
    }
    def dropValue {
      a1.forgetValue(this); a2.forgetValue(this); sum.forgetValue(this)
    }
    a1 connect this
    a2 connect this
    sum connect this
  }

  case class Constant(q: Quantity, v: Double) extends Constraint {
    def newValue { error("Constant.newValue") }
    def dropValue { error("Constant.dropValue") }
    q connect this
    q.setValue(v, this)
  }

  case class Probe(name: String, q: Quantity) extends Constraint {
    def newValue { printProbe(q.getValue) }
    def dropValue { printProbe(None) }
    private def printProbe(v: Option[Double]) {
      val vstr = v match {
        case Some(x) => x.toString()
        case None => "?"
      }
      println("Probe: " + name + " = " + vstr)
    }
    q connect this
  }


  /**
   * Exercise 1
   */

  case class Multiplier(a1: Quantity, a2: Quantity, product: Quantity) extends Constraint {
    def newValue {
      (a1.getValue, a2.getValue, product.getValue) match {
    	case (Some(0) , _		, _		 ) => product.setValue(0, this)
    	case (_       , Some(0)	, _		 ) => product.setValue(0, this)
        case (Some(x1), Some(x2), _      ) => product.setValue(x1 * x2, this)
        case (Some(x1), _       , Some(r)) => a2.setValue(r / x1, this)
        case (_       , Some(x2), Some(r)) => a1.setValue(r / x2, this)
        case (_       , _       , Some(r)) => if(a1 == a2) {
        	val root = Math.sqrt(r)
        	a1.setValue(root)
        	a2.setValue(root)
        }
        case _                             =>
      }
    }
    def dropValue {
      a1.forgetValue(this); a2.forgetValue(this); product.forgetValue(this)
    }
    a1 connect this
    a2 connect this
    product connect this
  }
  
  case class Equality(a: Quantity, b: Quantity) extends Constraint {
    def newValue {
      (a.getValue, b.getValue) match {
        case (Some(a), _       ) => b.setValue(a, this)
        case (_       , Some(b)) => a.setValue(b, this)
        case _                             =>
      }
    }
    def dropValue {
      a.forgetValue(this); b.forgetValue(this)
    }
    a connect this
    b connect this
  }


  /**
   * Exercise 2
   */

  case class Square(a: Quantity, b: Quantity) extends Constraint {
	def newValue {}
	def dropValue {
		a.forgetValue(this); b.forgetValue(this)
	}
	
    b === (a * a)
  }


  class Quantity {

    private var value: Option[Double] = None
    private var constraints: List[Constraint] = List()
    private var informant: Constraint = null

    def getValue: Option[Double] = value

    def setValue(v: Double, setter: Constraint) {
      value match {
        case Some(v1) =>
          if (v != v1) error("Error! contradiction: " + v + " and " + v1)
        case None =>
          informant = setter
          value = Some(v)
          for (c <- constraints if c != informant) c.newValue
      }
    }
    def setValue(v: Double) { setValue(v, NoConstraint) }

    def forgetValue(retractor: Constraint) {
      if (retractor == informant) {
        value = None
        for (c <- constraints if c != informant) c.dropValue
      }
    }
    def forgetValue { forgetValue(NoConstraint) }

    def connect(c: Constraint) {
      constraints = c :: constraints
      value match {
        case Some(_) => c.newValue
        case None    =>
      }
    }

    def +(that: Quantity): Quantity = {
      val sum = new Quantity
      Adder(this, that, sum)
      sum
    }


    /**
     * Exercise 1
     */

    def *(that: Quantity): Quantity = {
    	val product = new Quantity
    	Multiplier(this, that, product)
    	product
    }
    
    def ===(that: Quantity) {
    	Equality(this, that)
    }


    /**
     * Exercise 2
     */

    def square: Quantity = {
    	val result = new Quantity
    	Square(this, result)
    	result
    }
    
    def sqrt: Quantity = {
    	val result = new Quantity
    	Square(result, this)
    	result
    }


    override def toString(): String = value match {
      case None    => "  ?"
      case Some(v) => v.toString()
    }

  }

  def main(args: Array[String]) {
    def adderExample {
      println("Adder example")
      val q1, q2, q3 = new Quantity
      Adder(q1, q2, q3)
      Probe("q1", q1)
      Probe("q2", q2)
      Probe("q3", q3)

      q1.setValue(20)
      q3.setValue(22)
      q1.forgetValue
      q3.forgetValue
    }

    adderExample
    
    def multiplierExample {
      println("Multiplier example")
      val q1, q2, q3 = new Quantity
      Multiplier(q1, q2, q3)
      Probe("q1", q1)
      Probe("q2", q2)
      Probe("q3", q3)

      q1.setValue(6)
      q3.setValue(42)
      q3.forgetValue
      
      q1.forgetValue
      q1.setValue(0)
      q1.forgetValue
    }

    multiplierExample
    
    def equalityExample {
      println("Equality example")
      val q1, q2 = new Quantity
      Equality(q1, q2)
      Probe("q1", q1)
      Probe("q2", q2)
    
      q1.setValue(36)
      q1.forgetValue
      
      q2.setValue(52)
    }

    equalityExample
    
    def squareExample {
      println("Square example")
      val q1, q2 = new Quantity
      Square(q1, q2)
      Probe("q1", q1)
      Probe("q2", q2)
    
      q2.setValue(25)
    }
    
    squareExample
    
    def opsExample {
    	println("Ops example");
    	{
    		val a, b = new Quantity
    		val c = a + b
    		Probe("1 + 2", c)
    		a setValue 1
    		b setValue 2
    	}
    	
    	{
    		val a, b, c = new Quantity
    		c === a + b
    		Probe("5 + 6", c)
    		a setValue 5
    		b setValue 6
    	}
    	
    	{
    		val a, b = new Quantity
    		val c = a * b
    		Probe("123 * 456", c)
    		a setValue 123
    		b setValue 456
    	}
    	
    	{
    		val a = new Quantity
    		val b = a sqrt;
    		Probe("sqrt(121)", b)
    		a setValue 121
    	}
    	
    	{
    		val a = new Quantity
    		val b = a square;
    		Probe("7^2", b)
    		a setValue 7
    	}
    }
    
    opsExample
  }
}
