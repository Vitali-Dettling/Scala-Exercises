/**
  * Created by vitali on 1/9/17.
  */
class NatImpl extends App {


  if(0 == Zero) true



}


abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}



object Zero extends Nat{
  def isZero: Boolean = true
  def predecessor: Nat = throw new IndexOutOfBoundsException
  def successor: Nat = this
  def + (that: Nat): Nat = that
  def - (that: Nat): Nat = this

}

class Succ(n: Nat) extends Nat{

  def isZero: Boolean = false
  def predecessor: Nat = this
  def successor: Nat = n
  def + (that: Nat): Nat = n + that
  def - (that: Nat): Nat = (n - that) //< 0) throw new IndexOutOfBoundsException
}