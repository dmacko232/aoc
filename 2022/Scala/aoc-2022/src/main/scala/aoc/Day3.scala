package aoc

import scala.io.Source
import scala.collection.immutable.Vector

// a - z 1 - 26
// A - z 27 - 52

case class Rucksack(items: Vector[Char])

def mapItem(item: Char): Int =

    val item_ascii = item.toInt
    if item_ascii >= 'a'.toInt then
        return item_ascii - 'a'.toInt + 1
    return item_ascii - 'A'.toInt + 27


def parseLine(line: String): Rucksack =

    val size = line.size
    val items: List[Char] = line.toList
    return Rucksack(
        Vector(items: _*)
    )

def sharedPriority(rucksack: Rucksack): Int =

    val a = rucksack.items.take(rucksack.items.size / 2)  
    val b = rucksack.items.drop(rucksack.items.size / 2)  
    a.intersect(b).map(mapItem)(0)

def sharedElvesPriority(rucksacks: List[Rucksack]): Int =

    rucksacks.map(r => r.items).fold(rucksacks.head.items)((a, b) => a.intersect(b)).map(mapItem)(0)

@main
def rucksackCommon(fpath: String) =

    // read
    val lines = Source.fromFile(fpath).getLines.toList

    // map to rucksacks
    val rucksacks = lines.map(parseLine)

    // part 1
    val sharedPriorityTotal = rucksacks.map(sharedPriority).sum
    println(sharedPriorityTotal)

    // part 2
    val sharedElvesPriorityTotal = rucksacks.grouped(3).map(sharedElvesPriority).sum
    println(sharedElvesPriorityTotal)