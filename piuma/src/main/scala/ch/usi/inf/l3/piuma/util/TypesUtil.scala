/*
 * Copyright (c) <2014>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */

package ch.usi.inf.l3.piuma.util


import ch.usi.inf.l3.piuma.plugin.PiumaPlugin


trait TypesUtil { self: PiumaPlugin =>
  import self._
  import self.global._


  /**
    * A utility method to check whether a type is a type can be considered
    * as AnyVal.
    *
    * @param tpe the type to be checked
    *
    * @return true if tpe is AnyVal, and false otehrwise
    */
  def isAnyVal(tpe: Type) = {
    if (tpe <:< definitions.BooleanClass.tpe ||
      tpe <:< definitions.ByteClass.tpe ||
      tpe <:< definitions.ShortClass.tpe ||
      tpe <:< definitions.IntClass.tpe ||
      tpe <:< definitions.LongClass.tpe ||
      tpe <:< definitions.DoubleClass.tpe ||
      tpe <:< definitions.FloatClass.tpe ||
      tpe <:< definitions.CharClass.tpe ||
      tpe <:< definitions.StringClass.tpe) true
    else false
  }

}
