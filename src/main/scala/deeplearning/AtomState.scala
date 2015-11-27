package deeplearning

/**
  * Created by Julian Liebl on 25.11.15.
  *
  * Class which represents an atom state in the Ising Model.
  */
object AtomState extends Enumeration {
  type AtomState = Value
  val Up, Down = Value

  /**
    * Helper method which returns the numerical state value.
    *
    * Up = 1
    * Down = -1
    *
    * @param atomState atom state
    * @return the numerical representation of the atom state
    */
  def getAtomStateValue(atomState: AtomState): Int ={
    if(atomState equals Up) 1 else -1
  }

  /**
    * Helper method which returns a random atom state.
    * @return the random atom state
    */
  def getRandomAtomState(): AtomState ={
    val r = scala.util.Random
    if(r.nextInt(2) equals 0) Up else Down
  }

  /**
    * Helper method which return the opposite atom state.
    *
    * @param atomState atom state
    * @return the opposite atom state
    */
  def getOppositeState(atomState: AtomState) ={
    if(atomState equals Up) Down else Up
  }
}
