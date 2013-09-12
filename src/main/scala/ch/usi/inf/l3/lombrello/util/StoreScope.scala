package ch.usi.inf.l3.lombrello.util

class StoreScope[T] {
  private val env = Map.empty[Symbol, T]
}