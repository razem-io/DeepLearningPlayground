package deeplearning

import java.util.concurrent.atomic.AtomicInteger

import deeplearning.AtomState.AtomState
import deeplearning.AtomState._


/**
  * Created by Julian Liebl on 25.11.15.
  *
  * All inspiration taken from youtu.be/hvIptUuUCdU. This class reproduces and proves the result shown in minute ~15:40.
  */
class IsingModelSmall {
  case class MinMax(val min:Double, val max:Double)

  val x1 = new Atom(Up)   .fuse(-50 , new Atom(Up))   .fuse(99, new Atom(Up))
  val x2 = new Atom(Down) .fuse(-50 , new Atom(Up))   .fuse(99, new Atom(Up))
  val x3 = new Atom(Up)   .fuse(-50 , new Atom(Down)) .fuse(99, new Atom(Up))
  val x4 = new Atom(Up)   .fuse(-50 , new Atom(Up))   .fuse(99, new Atom(Down))
  val x5 = new Atom(Down) .fuse(-50 , new Atom(Down)) .fuse(99, new Atom(Up))
  val x6 = new Atom(Up)   .fuse(-50 , new Atom(Down)) .fuse(99, new Atom(Down))
  val x7 = new Atom(Down) .fuse(-50 , new Atom(Up))   .fuse(99, new Atom(Down))
  val x8 = new Atom(Down) .fuse(-50 , new Atom(Down)) .fuse(99, new Atom(Down))

  /**
    * Calculates the stable state of a Ising Model according to youtu.be/hvIptUuUCdU.
    * It takes a random atom from the model as parameter and parses from there all atoms and sub(n) atoms it is
    * connected to.
    *
    *
    * Here is an example how the stable state is calculated:
    *
    * Model =  a1(Up) <- w1(-50) -> a2(Down) <- w2(99) -> a3(Down)
    *       => x = -((a1 * w1 * a2) + (a2 * w2 * a3))
    *       => x = -((1 * -50 * -1) + (-1 * 99 * -1))
    *       => x = -(50 + 99)
    *       => x = -
    *
    * @param atom A random atom form the model. Needs at least one connection. Otherwise stable state will be zero.
    * @return stable state value
    */
  def calcStableState(atom:Atom, touchedAtoms:Set[Atom] = Set()): Double ={
    var sum:Double = 0

    val a1v = getAtomStateValue(atom.atomState)
    atom.getConnections().foreach(connection => {
      val connectedAtom = connection.connectedAtom

      if(!(touchedAtoms contains connectedAtom)){
        val a2v = getAtomStateValue(connectedAtom.atomState)

        sum += a1v * a2v * connection.weight

        sum += calcStableState(connectedAtom, touchedAtoms + atom)
      }
    })

    - sum
  }

  /**
    * Retrieves the min and max weight for all atom connections in a model.
    * It takes a random atom from the model as parameter and parses from there all connections and sub(n) connections.
    *
    *
    * Example:
    *
    * Model =  a1(Up) <- w1(-50) -> a2(Down) <- w2(99) -> a3(Down) <- w3(20) -> a4(Up)
    *       => min = -50
    *       => max =  99
    *
    * @param atom A random atom form the model. Needs at least one connection. Otherwise min and max will be zero.
    * @return min and max weight
    */
  def getMinMaxWeight(atom:Atom, touchedAtoms:Set[Atom] = Set()): MinMax ={
    var minMax:MinMax = MinMax(0,0)

    atom.getConnections().foreach(connection => {
      val connectedAtom = connection.connectedAtom

      if(!(touchedAtoms contains connectedAtom)){
        val currentWeight = connection.weight

        if (currentWeight < minMax.min){
          minMax = minMax.copy(min = currentWeight)
        }
        else if (currentWeight > minMax.max) {
          minMax = minMax.copy(max = currentWeight)
        }

        val provisionalMinMax = getMinMaxWeight(connectedAtom, touchedAtoms + atom)
        if(provisionalMinMax.min < minMax.min) minMax = minMax.copy(min = provisionalMinMax.min)
        if(provisionalMinMax.max > minMax.max) minMax = minMax.copy(max = provisionalMinMax.max)
      }
    })

    minMax
  }

  /**
    * Atom vibration simulation.
    * It takes a random atom from the model as parameter and parses from there all connections. Simulating a random
    * initial atom state and regarding probability of all connections and sub connections. Resulting in the same
    * connections but may be with different states then before.
    *
    * @param atom A random atom form the model. Needs at least one connection. Otherwise the given atom will just be
    *             returned.
    * @return The new atom with the same connections but eventually different states.
    */
  def vibrate(atom:Atom): Atom ={
    var touchedAtoms:Set[Atom] = scala.collection.immutable.Set()

    val currentMinMaxWeight = getMinMaxWeight(atom)

    val minWeight = currentMinMaxWeight.min
    val maxWeight = currentMinMaxWeight.max

    val weightRange = if(Math.abs(minWeight) > Math.abs(maxWeight)) Math.abs(minWeight) else Math.abs(maxWeight)
    val scaledWeightRange = weightRange * 1.2

    val random = scala.util.Random

    def vibrateInner(innerAtom:Atom, currentAtomState:AtomState):Atom ={
      val newAtom = new Atom(currentAtomState)

      touchedAtoms += newAtom

      innerAtom.getConnections().foreach(connection => {
        val connectedAtom = connection.connectedAtom
        connectedAtom.removeConnection(innerAtom)

        if(!(touchedAtoms contains connectedAtom)){
          val weight = connection.weight

          val probability = Math.abs(weight) / scaledWeightRange
          val randomDouble = random.nextDouble()
          val isFollowing = probability - randomDouble >= 0

          if(weight != 0){
            var connectedAtomState:AtomState = null

            if(weight < 0) {
              connectedAtomState = if (isFollowing) getOppositeState(currentAtomState) else currentAtomState
            }else{
              connectedAtomState = if (isFollowing) currentAtomState else getOppositeState(currentAtomState)
            }

            connectedAtom.atomState = connectedAtomState

            newAtom.fuse(connection.weight, vibrateInner(connectedAtom, connectedAtomState))
          }else{
            println("Error: Weight should never be 0!")
            return newAtom
          }
        }
      })

      newAtom
    }

    vibrateInner(atom, getRandomAtomState())
  }
}


object IsingModelSmall{
  def main(args: Array[String]) {
    val model = new IsingModelSmall

    println("E(x1,w) = " + model.calcStableState(model.x1))
    println("E(x2,w) = " + model.calcStableState(model.x2))
    println("E(x3,w) = " + model.calcStableState(model.x3))
    println("E(x4,w) = " + model.calcStableState(model.x4))
    println("E(x5,w) = " + model.calcStableState(model.x5))
    println("E(x6,w) = " + model.calcStableState(model.x6))
    println("E(x7,w) = " + model.calcStableState(model.x7))
    println("E(x8,w) = " + model.calcStableState(model.x8))

    println(model.getMinMaxWeight(model.x1))

    val vibrationLoopCount:Int = 10000
    val atomicLoopIndex = new AtomicInteger()
    println("Simulating vibration of atom " + vibrationLoopCount + " times.")

    val statesToCount = (1 to vibrationLoopCount).toTraversable.par.map(loopIndex => {
      val vibratedX1 = model.vibrate(model.x1)

      if(atomicLoopIndex.incrementAndGet() % 10000 == 0) print("\r" + atomicLoopIndex.get())

      model.calcStableState(vibratedX1)
    }).groupBy(identity).mapValues(_.size)

    println("\r" + atomicLoopIndex.get())

    val states = statesToCount.keySet.toList.sorted

    states.foreach(state => println(state + "\t: " + statesToCount.get(state).get))
  }
}
