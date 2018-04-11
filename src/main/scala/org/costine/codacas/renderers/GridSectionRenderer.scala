package org.costine.codacas.renderers

import org.costine.codacas.{Ship, Star, Torp, Universe, UniverseObject}
import scala.annotation.tailrec

object GridSectionRenderer {
  def renderGridElement(objs: List[UniverseObject] ): Char = objs match {
    case Nil => '.'
    case _ =>
      val ships = objs.collect {case o: Ship => o }
      if (ships.size == 1) ships.head.name.toCharArray.headOption.getOrElse ('?')
      else if (ships.size > 1) '@'
      else if (objs.collect {case o: Star => o }.nonEmpty) '*'
      else if (objs.collect {case o: Torp => o }.nonEmpty) '+'
      else '.'
  }

  val defaultElementRenderer: (Universe, Double, Double, Double, Double) => String =
    (universe: Universe, rsv: Double, rsl: Double, csv: Double, csl: Double) =>
      GridSectionRenderer.renderGridElement(universe.locateObjectsInGrid(rsv, rsl, csv, csl)).toString
}

trait GridSectionRenderer {

  def renderGrid(universe: Universe,
             size: Int,
             fInitLocX: () => Double,
             fInitLocY: () => Double,
             fXgridStep: Int => Double,
             fYgridStep: Int => Double,
             fNextRow: (Double,Double) => Double,
             fNextCol: (Double,Double) => Double,
             elementRenderer: (Universe,Double,Double,Double,Double) => String
            ):Array[Array[String]] = {

    val a = Array.ofDim[String](size,size)
    val gridStepX = fXgridStep(size)
    val gridStepY = fYgridStep(size)
    val sizeList = (0 until size).toList

    @tailrec def doRows (rows: List[Int], rowStepVal: Double, rowStepLim: Double) : Unit = {

      @tailrec def doCols(row: Array[String], cols: List[Int], colStepVal: Double, colGridStep: Double) : Unit = {
        cols match {
          case Nil => ()
          case current :: restCols =>
            row(current) = elementRenderer(universe,rowStepVal, rowStepLim, colStepVal, colGridStep)
            val nextCol = fNextCol(colStepVal,gridStepY)
            doCols(row,restCols,nextCol,fNextCol(nextCol,gridStepY))
        }
      }

      rows match {
        case Nil => ()
        case current :: restRows =>
          a(current) = new Array[String](size)
          val nextCol= fInitLocY()
          doCols(a(current),sizeList,nextCol,fNextCol(nextCol,gridStepY))
          val nextRow = fNextRow(rowStepVal,gridStepX)
          doRows(restRows, nextRow,fNextRow(nextRow,gridStepX))
      }
    }

    val x = fInitLocX()
    doRows(sizeList,x, fNextRow(x,gridStepX))
    a
  }

}
