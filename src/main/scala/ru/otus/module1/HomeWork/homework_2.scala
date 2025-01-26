package ru.otus.module1.HomeWork

import scala.util.Random

trait Balls
case object White extends Balls
case object Black extends Balls
case object Unknown extends Balls

case class Bin(b: List[Balls])

object homework_2 {
    def findprobability(bin: List[Balls], bintries: Int = 2, tries: Int = 10000): Double = {
        val getSecondBall: List[Balls] => Balls = list => (1 to bintries).foldLeft[(List[(Balls, Int)], Balls)]((list.zipWithIndex, Unknown)){
            case ((b, acc), _) => {
                val index = Random.between(0, b.size - 1)
                (b.filterNot(e => e._2 == index), b(index)._1)
            }
        }._2

        val allWhite = (1 to tries).foldLeft(List[Balls]()){
            case (acc, _) => getSecondBall(bin) match {
                case x if x == White => acc :+ x
                case _ => acc
            }
        }.size

        1.toDouble - (tries.toDouble - allWhite.toDouble) / tries.toDouble
    }
}