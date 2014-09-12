/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */
package ch.usi.inf.l3

import scala.reflect.api.Trees
import scala.reflect.api.Types
import scala.tools.nsc.Global


package object mina {

  /**
   * Two identity functions, to tell the plugin to deal with the passed
   * expressions as a CT or RT value.
   */
  def CT[T](expr: => T) = expr
  def RT[T](expr: => T) = expr
}