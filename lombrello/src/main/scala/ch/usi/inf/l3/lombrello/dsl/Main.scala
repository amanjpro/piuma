package ch.usi.inf.l3.lombrello.dsl


/**
 * @author Amanj Sherwany
 * @date 2 May 2014
 */

/**
 * What I need:
 * 1- To throw away the boiler plate code needed for writing a plugin
 * 2- To limit the user from accessing the current compiler API
 * 3- To provide a ``safer'' and ``easier'' compiler API, so we can
 *    make sure that plugins do not do bad things
 */
object Main {

  // TODO: Implement this
  def usage: String = "Usage message"


  def main(args: Array[String]): Unit = {
    require(args.length > 0, usage)
    // TODO: This is a very naiive approach to parse optons,
    // a better approach is to use ``scopt'' library which is
    // hosted on: https://github.com/scopt/scopt

    val r = new Compiler().compile(args.toList)
    System.exit(if(r == 0) 0 else 1)
  }
}
