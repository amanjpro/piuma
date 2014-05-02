package ch.usi.inf.l3.lombrello.dsl


/**
 * @author Amanj Sherwany
 * @date 2 May 2014
 */

object Main {



  def main(args: Array[String]): Unit = {
    // TODO: This is a very naiive approach to parse optons,
    // a better approach is to use ``scopt'' library which is
    // hosted on: https://github.com/scopt/scopt

    val r = new Compiler().compile(args.toList)
    System.exit(if(r == 0) r else 1)
  }
}
