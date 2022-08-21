case class Rational(numerator: Int, denominator: Int):
    require(denominator!=0)

    def this(numerator: Int) = this(numerator, 1)


    def +(other: Rational):Rational = 
        if this.denominator == other.denominator then
            Rational(this.numerator + other.numerator, denominator)
        else
            Rational((this.numerator*other.denominator + this.denominator * other.numerator),this.denominator * other.denominator)

    def +(other: Int) = 
        Rational(numerator + other * denominator, denominator)

    def * (other: Rational):Rational = 
        Rational(this.numerator * other.numerator, this.denominator * other.denominator)

    def * (other: Int): Rational = 
        Rational(this.numerator * other, this.denominator)

    def - (other: Rational):Rational = 
        if this.denominator == other.denominator then
            Rational(this.numerator - other.numerator, denominator)
        else
            Rational((this.numerator*other.denominator - this.denominator * other.numerator),this.denominator * other.denominator)

    def -(other: Int) = 
        Rational(numerator - other * denominator, denominator)

    def /(other: Int) = 
        Rational(numerator, denominator * other)

    def / (other: Rational):Rational = 
        Rational(this.numerator * other.denominator, this.denominator * other.numerator)
        

    def < (other: Rational):Boolean = 
        this.numerator * other.denominator < this.denominator * other.numerator

    def max(other: Rational):Rational = 
        if this < other then other else this

    override def toString(): String = 
        
        s"$numerator/$denominator"

object Rational:

    def gcd(numer:Int, denom: Int): Int = 
        def loop(a: Int, b: Int):Int = 
            val modVal = a % b
            if modVal == 0 then
                b
            else
                loop(b, modVal)
        loop(numer, denom)


    def apply(numerator: Int, denominator: Int): Rational = 
        val g = gcd(numerator, denominator)
        new Rational(numerator/g, denominator/g)


extension(x: Int)
    def + (y:Rational) = Rational(x, 1) + y
    def - (y:Rational) = Rational(x, 1) - y
    def * (y:Rational) = Rational(x, 1) * y
    def / (y:Rational) = Rational(x, 1) / y

val r = Rational(1, 2)

println(1 + r)


