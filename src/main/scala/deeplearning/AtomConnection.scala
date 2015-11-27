package deeplearning

/**
  * Created by Julian Liebl on 26.11.15.
  *
  * Class which represents an atom in the Ising Model.
  */
case class AtomConnection(connectedAtom:Atom, weight:Double)

object AtomConnection{
  /**
    * Creates a weighted connection between two atoms.
    *
    * @param a1 first atom
    * @param a2 second atom
    * @param weight weight of the connection
    */
  def fuse(a1:Atom, a2:Atom, weight:Double): Unit ={
    a1.addConnection(new AtomConnection(a2, weight))
    a2.addConnection(new AtomConnection(a1, weight))
  }
}
