package deeplearning

import deeplearning.AtomState.AtomState

import scala.collection.mutable.ListBuffer

/**
  * Created by Julian Liebl on 26.11.15.
  *
  * Class which represents an atom in the Ising Model.
  */
class Atom(var atomState: AtomState) {
  var connections:ListBuffer[AtomConnection] = ListBuffer()

  def addConnection(atomConnection: AtomConnection): Unit ={
    connections += atomConnection
  }

  def removeConnection(atomConnection: AtomConnection): Unit ={
    connections -= atomConnection
  }

  def removeConnection(atom:Atom): Unit ={
    connections = connections.filter(connection => !(connection.connectedAtom equals atom))
  }

  def removeConnections(atoms:Seq[Atom]): Unit ={
    connections = connections.filter(connection => !(atoms contains connection.connectedAtom))
  }

  def getConnections(): Seq[AtomConnection]  ={
    connections
  }

  /**
    * Creates a weighted connection between the atom and anotherAtom. Returns the other atom in order to be able to
    * chain the creation of a model.
    *
    * @param weight weight of the connection
    * @param otherAtom other atom
    * @return other atom
    */
  def fuse(weight:Double, otherAtom:Atom): Atom ={
    AtomConnection.fuse(this, otherAtom, weight)

    otherAtom
  }
}
