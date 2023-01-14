#!/usr/bin/env scala-cli

//> using scala "3.2.1"

import scala.util.chaining.*

final case class Resources(ore: Int, clay: Int, obsidian: Int, geode: Int):
  private def binaryOp(that: Resources, op: (Int, Int) => Int): Resources =
    Resources(op(ore, that.ore), op(clay, that.clay), op(obsidian, that.obsidian), op(geode, that.geode))
  def +(that: Resources): Resources = binaryOp(that, _ + _)
  def -(that: Resources): Resources = binaryOp(that, _ - _)
  def valid: Boolean = ore >= 0 && clay >= 0 && obsidian >= 0 && geode >= 0

object Resources:
  val zero = Resources(0, 0, 0, 0)

final case class Blueprint(
  id: Int,
  oreCost: Resources,
  clayCost: Resources,
  obsidianCost: Resources,
  geodeCost: Resources
):
  private val recipes: List[Resources] = List(oreCost, clayCost, obsidianCost, geodeCost)
  private val maxOreBots: Int = recipes.map(_.ore).max
  private val maxClayBots: Int = recipes.map(_.clay).max
  private val maxObsidianBots: Int = recipes.map(_.obsidian).max
  private def theoreticalMax(remainingTime: Int, bots: Resources, resources: Resources): Int =
    resources.geode + (remainingTime * bots.geode) + ((remainingTime-1) * remainingTime) / 2
  def score(time: Int, initialBots: Resources, initialResources: Resources): Int =
    def impl(currentMax: Int, t: Int, bots: Resources, resources: Resources): Int =
      if t == 0 then resources.geode
      else if currentMax >= theoreticalMax(t, bots, resources) then -1
      else
        val steps = List(
          (bots.copy(ore = bots.ore + 1), resources - oreCost),
          (bots.copy(clay = bots.clay + 1), resources - clayCost),
          (bots.copy(obsidian = bots.obsidian + 1), resources - obsidianCost),
          (bots.copy(geode = bots.geode + 1), resources - geodeCost),
          (bots, resources),
        )
          .filter(_._2.valid)
          .filter(_._1.ore <= maxOreBots)
          .filter(_._1.clay <= maxClayBots)
          .filter(_._1.obsidian <= maxObsidianBots)
          .map { case (bs, rs) => (bs, rs + bots) }
        steps.foldLeft(currentMax) { case (currMax, (bs, rs)) => Math.max(currMax, impl(currMax, t-1, bs, rs)) }

    impl(-1, time, initialBots, initialResources)

object Blueprint:
  def parse(str: String): Blueprint =
    val parts = str.split(" ")
    Blueprint(
      id = parts(1).takeWhile(_ != ':').toInt,
      oreCost = Resources(parts(6).toInt, 0, 0, 0),
      clayCost = Resources(parts(12).toInt, 0, 0, 0),
      obsidianCost = Resources(parts(18).toInt, parts(21).toInt, 0, 0),
      geodeCost = Resources(parts(27).toInt, 0, parts(30).toInt, 0)
    )

val source = io.Source.fromFile("data/19.txt")
val input = try source.mkString finally source.close()
val bps = input
  .split("\n")
  .map(Blueprint.parse)

bps
  .map(bp => bp.id * bp.score(24, Resources.zero.copy(ore = 1), Resources.zero))
  .sum
  .tap(println)

bps
  .take(3)
  .map(_.score(32, Resources.zero.copy(ore = 1), Resources.zero))
  .product
  .tap(println)