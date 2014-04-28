/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */

package tests.plugins

import ch.usi.inf.l3.lombrello.transform.dsl._

class TestPlugin(override val global: TGlobal) extends TransformerPlugin(global) {

  val name: String = "test"
  val beforeFinder = utilities.PHASE_PATMAT
  override val description: String = """A compiler plugin!""";

  val pluginComponents: List[TransformerPluginComponent] = List(new TestPluginComponent(this),
    new TestPluginComponent2(this))
}

class TestPluginComponent(plgn: TestPlugin) extends TransformerPluginComponent(plgn) {

  override val runsRightAfter = Some(plgn.utilities.PHASE_SUPERACCESSORS)
  val runsAfter = List[String](plgn.utilities.PHASE_FLATTEN, plgn.utilities.PHASE_PATMAT, plgn.utilities.PHASE_TYPER)
  override val runsBefore = List[String](plgn.utilities.PHASE_ICODE)
  val phaseName = "test"

  import global._

  def transform(cmp: TransformerComponent, tree: Tree): Either[Tree, Tree] = {
    import cmp._
    tree match {
      //      case a @ ValDef(_, _, _, _) => null
      //      case a @ q"val" if true => null
      case x: ClassDef =>

        val newName = newTypeName("YESS")
        val vtree = mkVar(x.symbol, "xmass ", definitions.IntTpe)
        val strgtr = mkSetterAndGetter(vtree).get
        val ntmplt = treeCopy.Template(x.impl, x.impl.parents, x.impl.self, vtree :: strgtr._1 :: strgtr._2 :: x.impl.body)
        val nclazz = treeCopy.ClassDef(x, x.mods, x.name, x.tparams, ntmplt)

        Right(typer.typed(nclazz))
      case x: ValDef if (x.name == newTermName("b")) =>
        val newName = newTermName("hello")
        if (canRename(tree, newName)) {
          println("hello1")
          Right(rename(tree.asInstanceOf[ValDef], newName))
        } else {
          Right(tree)
        }
      case x: ValDef if (x.name == newTermName("c")) =>
        val newName = newTermName("local")
        if (canRename(tree, newName)) {
          println("hello1")
          Right(rename(tree.asInstanceOf[ValDef], newName))
        } else {
          Right(tree)
        }

      case x: ValDef if (x.name == newTermName("d")) =>
        val newName = newTermName("param")
        if (canRename(tree, newName)) {
          println("hello1")
          Right(rename(tree.asInstanceOf[ValDef], newName))
        } else {
          Right(tree)
        }
      case x => Right(x)
    }
  }
}

class TestPluginComponent2(plgn: TestPlugin) extends TransformerPluginComponent(plgn) {

  override val runsRightAfter = Some(plgn.utilities.PHASE_INLINER)
  override val runsAfter = List[String](plgn.utilities.PHASE_INLINER)
  val phaseName = "test2"

  import global._

  def transform(cmp: TransformerComponent, tree: Tree) = {
    import cmp._
    tree match {
      case x: ValDef =>
        println("HERE HERE " + x.symbol.attachments)
        Right(x)
      case x => Right(x)
    }
  }
}




/**
It took me quite some time to figure out how to get it working. There are some hints in this thread and also some on the official page about plugins. I am writing this message in order to hopefully be of some help to anyone wrestling with this in the future. 

I am running the Scala IDE plugin 2.0 in Eclipse 3.7.1. These were the steps I performed to be able to run a plugin using a 'launch configuration'. 

//Setting up the project 

1. Create a new Scala project (for example 'testPlugin'). 
2. Right click the 'testPlugin' project, go to 'Java Build Path', 'Libraries' and select 'Add Library'. Choose 'Scala Compiler'. 
3. Create a directory called 'libs' in your project. 
4. In the 'Package Explorer' open the 'Scala Library' element, right click the 'scala-library.jar', select 'Properties' and copy the directory you find there. 
5. Navigate to the directory you copied and copy the 'scala-library.jar' and 'scala-library-src.jar' to the 'libs' directory. 
6. Repeat step 4 and 5 for the 'scala-compiler' related jars. 
7. Open the project properties and 'Libraries' section again. Click 'Add JARs...' to add the 4 jars from the 'libs' directory. 
8. Remove the 'Scala Compiler' and 'Scala Libary' entry. Apparently launch configurations do not work very good with these types of libraries. 

//Creating the plugin jar 

9. Create a file named 'scalac-plugin.xml' and put the following information inside (replace the classname with the actual classname): 

<plugin>
  <name>testplugin</name>
  <classname>ee.test.TestPlugin</classname>
</plugin 

10. Rightclick the project and select 'Export...' -> 'JAR file'. Make sure the plugin xml is selected and set 'testPlugin/plugin.jar' as destination. 

//Create the plugin 

11. Create the Scala plugin class in the correct package, you can check the official page for the correct conventions (I copied the class I used at the bottom of the message). 

//Create the launch configuration 

12. Click 'Run' -> 'Debug configurations...'. Right click 'Scala Application' and select 'New'. 
13. Give your configuration a name and click 'Search' at the 'Main class' section. Type 'Main' and select 'scala.tools.nsc.Main' 
14. Go to the 'Arguments' tab and add '-Xplugin:plugin.jar Test.scala' (without the quotes) 
15. Go to the 'Classpath' tab, click 'Bootstrap Entries' and 'Add JARs...'. Add the 'scala-library.jar'. 

//Run the configuration 

16. Create a class called 'Test.scala' in the root of the 'testPlugin' project. 
17. Go back to the 'Debug configurations...', select the one you created and click 'Debug' (there are usually visual and keyboard shortcuts to do this). 


If anyone has any suggestions on how to make this work with the built-in 'Scala Libary' and 'Scala Compiler' libraries I would be very happy. 


Erik 


The plugin code I used to test: 

package ee.test 

import scala.tools.nsc.plugins.Plugin 
import scala.tools.nsc.Global 
import scala.tools.nsc.plugins.PluginComponent 
import scala.tools.nsc.Phase 

class TestPlugin(val global:Global) extends Plugin { 
    import global._ 
    
        val name = "testplugin" 
        val description = "test plugin" 
        val components = List[PluginComponent](Component) 
        
        private object Component extends PluginComponent { 
            
            val global: TestPlugin.this.global.type = TestPlugin.this.global 
            val phaseName = TestPlugin.this.name 
            val runsAfter:List[String] = List("pickler") 
            override val runsBefore:List[String] = List("refchecks") 
            
            def newPhase(prev:Phase) = new TestPluginPhase(prev) 
            
            class TestPluginPhase(prev:Phase) extends StdPhase(prev) { 
                def apply(unit:CompilationUnit) { 
                    println(unit.body); 
                    //throw new RuntimeException("test"); 
                } 
            } 
        } 
} 
**/