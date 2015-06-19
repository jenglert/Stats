

import scala.util.Random
import scala.collection._

object Lines extends App {
  
  val people = (0 to 50).map(i => new Person("A", 1)) ++
               (0 to 40).map(i => new Person("B", 3)) ++
               (0 to 10).map(i => new Person("C", 7))
  
  var day = 0
  var reservationsPerDay = Map.empty[Int, List[Person]]
  
  var places = Map("A" -> mutable.ListBuffer.empty[Int], "B" -> mutable.ListBuffer.empty[Int], "C" -> mutable.ListBuffer.empty[Int])
 
  
  while (day < 10000) {
    
    people.foreach { p => 
      // give each person a 10% chance to make an order.
      if (Random.nextInt() % 10 == 0) {
        reservationsPerDay.get(p.leadTime + day) match { 
          case None =>
            reservationsPerDay = reservationsPerDay + ((p.leadTime + day, List(p)))
          case Some(personList) =>
            reservationsPerDay = reservationsPerDay + ((p.leadTime + day, personList :+ p))
        }
      } 
    }
    
    // Figure out each person's place in line.
    reservationsPerDay.get(day).foreach { personList => 
      personList.zipWithIndex.foreach { case (p, i) => 
        places.get(p.testGroup).get.append(i + 1)
      }
    }
      
    day = day + 1
  }
  
  places.foreach(p =>
    System.out.println("Group " + p._1 + " Average Place: " + (p._2.sum / p._2.size.toDouble))
  )
  
  
}

case class Person(
   testGroup: String,
   leadTime: Int
)