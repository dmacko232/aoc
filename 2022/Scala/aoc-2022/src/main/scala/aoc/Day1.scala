package aoc

import scala.io.Source
import scala.collection.mutable.ListBuffer

def parseCalories(caloriesRaw: List[String]): List[Int] =

    var calories: Int = 0
    var result = ListBuffer.empty[Int]
    for (line <- caloriesRaw)
        if line == "" then
            result += calories
            calories = 0
        else
            calories = calories + line.toInt
    return result.toList

@main
def calorieCounting(fpath: String) =
    // read file
    val lines = Source.fromFile(fpath).getLines.toList
    
    // cast to calories by elves
    val calories = parseCalories(lines)
    
    // get max and print
    val max = calories.max
    println(max)

    val top3 = calories.sortWith(_ > _).take(3)
    println(top3.sum)
