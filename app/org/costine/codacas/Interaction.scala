package org.costine.codacas

abstract class Interaction(s:UniverseObject, t:UniverseObject) {
  def source = s;
  def target = t;
  
  def interact():Result 
}
