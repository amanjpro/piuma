package ch.usi.inf.l3

import scala.annotation.StaticAnnotation
import scala.annotation.ClassfileAnnotation

package object ascala {
  
  /**
   * The target of this annotation is a class field decleration (TODO only var?)
   * It indicates that the annotated field is a member of the atomic set
   * given as a parameter to the annotation.
   */
  class atomic(s: Symbol) extends StaticAnnotation
  
  /**
   * The target of this annotation is a method parameter. I indicates that
   * the atomic set given as a parameter is merged with the atomic sets of the
   * class which contains this method definition. i.e. the lock associated
   * with the parameter should be required as well.
   */
  class unitfor(s:Symbol) extends StaticAnnotation
  
  /**
   * Same as above, but all locks associated with the parameter are acquired.
   * TODO maybe I can make a vararg constructor if such a thing exists to 
   * pass multiple sets at once.  
   */
  class mergeall extends StaticAnnotation
  
  /**
   * Target is an instantiation statement. It has the effect of 
   * joining the atomic set s1 (first parameter) that belongs to the newly 
   * allocated object with an atomic set s2 of the owning object (second 
   * parameter) this means that instead of creating a new lock object for s1, 
   * the lock of set s2 will be used for s1 as well.
   */
  class alias(s1:Symbol, s2:Symbol) extends StaticAnnotation
  
  /**
   * Target is a class or object declaration. This annotation indicated that
   * the synchronization of instances of this class will be managed by some
   * owning object. All instances of a class annotated internal should have
   * their atomic sets aliased. No references to these instances can be made
   * other than within the owner.
   */
  class internal extends StaticAnnotation
}