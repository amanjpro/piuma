package ch.usi.inf.l3.as.plugin

import ch.usi.inf.l3.lombrello.neve.NeveDSL._
import scala.collection.mutable.Map
import scala.reflect.internal.Flags._


object Commons {
  /**************************** Common Names ************************************/

  /**
   * base name for lock fields
   */
  val as_lock = "__as_lock"
    
  /**
   * Fully qualified name of @atomic 
   */
  val atomic_anno = "ch.usi.inf.l3.ascala.atomic"
    
  /**
   * Fully qualified name of @alias
   */
  val alias_anno = "ch.usi.inf.l3.ascala.alias"
    
  /**
   * Fully qualified name of @unitfor
   */
  val unitfor_anno = "ch.usi.inf.l3.ascala.unitfor"
    
  /**
   * Class of the lock object to be used
   */
  val lockClass = "ch.usi.inf.l3.as.plugin.OrderedLock"
}

import Commons._

/**
 * A port of Vaziri's 2012 proposal of Atomic sets for Scala
 * @author Nosheen Zaza
 *
 * General Notes and Considerations:
 * 1- I am using scala annotations at the moment because I want to be able to
 * use annotations everywhere. The negative side is that they are not available
 * at runtime in case I needed this in the future. I might change this decision
 * later. This code contains some extra, unused functions to support Java
 * annotations in case I needed to do so.
 *
 * General TODO:
 * 1- Support annotating fields declared in a class constructor, this requires
 * adding another phase that catches the @atomic annotation before the
 * compiler disposes it for good, this should be done after the parser
 * phase.
 * 3- Make sure we do not obtain a lock twice through acquiring an aliased lock
 * of a parameter that is an alias to a lock in the class itself.
 * also finish the work needed to avoid dereferencing an empty parameter for a lock.
 * 
 * some point.
 * 4- Support module classes and traits (later)
 * 5- I am only considering one atomic set per class at the moment (later)
 * 6- Add type checking (later)
 * 7- When supporting multiple locks, I need to insert an array
 * of locks of type OderedLock instead of Object as the lock. (later)
 * 9- Add a default value for the lock param so other compilation units do
 * not have to explicitely pass the lock, if this does not work, just keep
 * two constructors, which is what happens most likely on bytecode level.
 * fix that.
 */
@plugin(CheckAnnotationTargets,
        ClassSetsMapping,
        ClassAncestorsSetsMapping,
        AddLockFields,
        ModifyNewStmts,
        AddSync) class AtomicScala {
  
  val beforeFinder = "typer" // TODO: To get rid of
  val name = "atomic-scala"
  describe("Atomic sets for Scala")


  /**
   * TODO I will need to relate sets to fields and not
   * only to classes at some point in the future (e.g when supporing multiple
   * atomic sets.)
   */
  val classSetsMap: Map[Symbol, List[String]] = Map.empty
  

}

/******************* Phase 1: check annotation targets ************************/

/**
 * Check that each annotation is on the right target
 */
@checker("check-annotation-targets") class CheckAnnotationTargets {

  /* This is the earliest stage possible, each class will have a symbol after
   * this stage
   */
  // so my stuff vanish after superaccessors
  rightAfter("typer")
  
  def b(a: AtomicScala): AtomicScala = a

  plugin AtomicScala

  def check(unit: CompilationUnit) {
    //TODO fill in the implementation after more important tasks are done.
    //        unit.error(tree.pos, "Not the right target")
  }
}

/****************** Phase 2: create class-set map *****************************/
/**
 * Find which classes have atomic sets, the next phase will add lock fields
 * based on this information.
 */
@checker("class-sets-mapping") class ClassSetsMapping {


  rightAfter("check-annotation-targets")

  plugin AtomicScala 

  /**
   * TODO this looks horrible, beautify, consider getOrElse. I am also
   * inconsistent whether to test this condition here or by the caller.
   * 
   */
  private def ownerClassSymbol(t: Tree) = {
    if (hasSymbol(t))
      t.symbol.owner match {
        case s: ClassSymbol => Some(s)
        case s: MethodSymbol if (s.isClassConstructor) => Some(s.owner)
        case _ => None
      }
    else None
  }

  /**
   * This is Symbol to be compatible with what I get when I try to get
   * the class symbol while transformation. In practice this should always
   * be a class Symbol. The purpose is to relate classes to their atomic
   * sets.
   */
  private def addClassSetMapping(cs: Symbol, s: String) {
    if (classSetsMap.contains(cs)) classSetsMap(cs) = s :: classSetsMap(cs)
    else classSetsMap(cs) = s :: Nil
  }

  /**
   * Might need this if I decide to use java annotataions.
   *
   * TODO find a way to get the string value without using toString
   * to make it nicer and get rid of the quotations.
   * TODO after changing my way of getting the methods, I need to change this 
   * too.
   */
  private def j_atomicSetName(anno: AnnotationInfo) =
    anno.javaArgs.head._2.toString()


  /**
   * Returns the name of the atomic set.
   */
  private def atomicSetName(annoOpt: Option[AnnotationInfo]) = {
    annoOpt match {
      case Some(anno) if anno.args != Nil =>
        anno.args.head match {
          case Apply(_, Literal(value) :: Nil) => value.stringValue
          case _ => throw new Exception("Error getting atomic set name.")
        }
      case _ => throw new Exception("Error getting atomic set name.")
    }
  }


  /**
   *  TODO I think I am mixing type checking with other things here,
   *  thus extra checks to see if the target is valid, this is not needed
   *  will move them to the first type-checking phase.
   *
   *  If the tree is a ValDef tree which is a var (:|) and is contained
   *  in a class, and has an annotation @atomic(set_name), add to the
   *  class-sets map a mapping of the owner class and the set name.
   */
  private def addMapping(t: Tree) {  
    t match {
      case vt: ValDef if isVar(vt) &&
                         ownerClassSymbol(t) != None && //TODO: Wat?
                         hasAnnotation(t, atomic_anno) =>
        addClassSetMapping(
          ownerClassSymbol(t).get,
          atomicSetName(getAnnotationInfo(t, atomic_anno)))
      case _ => None
    }
  }

  /**
   * Initiate the work here.
   */
  def check(unit: CompilationUnit) {
    unit.body.foreach(addMapping)
    classSetsMap.foreach(println)
  }
}

/************ Phase 3: add mapping of class to ancestors' sets ****************/
/**
 * Find which classes have atomic sets, the next phase will add lock fields
 * based on this information.
 */
@checker("class-sets-mapping") class ClassAncestorsSetsMapping {

  rightAfter("class-sets-mapping")

  plugin AtomicScala 
  /**
   * This is Symbol to be compatible with what I get when I try to get
   * the class symbol while transformation. In practice this should always
   * be a class Symbol. The purpose is to relate classes to their atomic
   * sets.
   */
  private def addClassSetMapping(cs: Symbol, s: String) {
    if (classSetsMap.contains(cs)) classSetsMap(cs) = s :: classSetsMap(cs)
    else classSetsMap(cs) = s :: Nil
  }

  /**
   *  TODO I think I am mixing type checking with other things here,
   *  thus extra checks to see if the target is valid, this is not needed
   *  will move them to the first type-checking phase.
   *
   *  If the tree is a ValDef tree which is a var (:|) and is contained
   *  in a class, and has an annotation @atomic(set_name), add to the
   *  class-sets map a mapping of the owner class and the set name.
   */
  
  
  /**
   * class A {
   * lock_1 // these are not inserted now, I have a map of class names to
   * atomic sets, whcih is filled in the previous stage, so I have no problem
   * 
   * 
   * I know that, but during the parent check, you should first check the first parent
   * here A,
   * 
   * I think yes, we talk about this online :D
   * } //visited last (in your new phase), and has
   * 
   *  class B extends A {
   *  	//visited before last
   *    lock_2
   *   } 
   *   
   *   
   *   class C extends B {
   *   	//visited first, sees lock_2 but not lock_1
   *   }
   */
  private def addMapping(t: Tree) {
    t match {
      case cl: ClassDef if (!cl.symbol.parentSymbols.isEmpty) =>
        val parentSyms = cl.symbol.parentSymbols
        val parentSets = parentSyms.foldLeft(List[String]())((c, r) => {
          val setsOfClass = classSetsMap.get(r)
          if (setsOfClass != None) c ++ setsOfClass.get else c
        })
        if (!parentSets.isEmpty)
          parentSets.foreach(x => addClassSetMapping(cl.symbol, x))
      case _ => ()
    }
  }

  /**
   * Initiate the work here.
   */
  def check(unit: CompilationUnit) {
    unit.body.foreach(addMapping)
    classSetsMap.foreach(println)
  }
}

/************************ Phase 4: add lock fields ****************************/
/**
 * Creates lock objects that correspond to each atomic set declaration in
 * a class. Uses information collected during the previous phase.
 */
@phase("add-lock-fields") class AddLockFields {

  rightAfter("class-ancestor-sets-mapping")

  plugin AtomicScala 
  /**
   * Returns true if any ancestor of a class defines an atomic set.
   * @param list of parent symbols
   */
  private def hasLockFieldInParents(sl: List[Symbol]): Boolean = {
    val lockField = sl.foldLeft(List[Symbol]())((c,r) => {
      val lckSym = r.info.findMember(newTermName(as_lock), 0, 0, false)
      if(lckSym == NoSymbol || lckSym == null) c
      else if (lckSym.isOverloaded)
        throw new Exception("Duplicate lock field in one class!")
      else lckSym :: c
    })
    
    if (lockField.size == 1) true
    else if (lockField.size == 0) return false
    
    //TODO when I support a list of locks, this message should change.
    else throw new Exception("Duplicate lock fields in parents!")
  }

  /**
   * Returns a copy of the argument constructor plus the lock param.
   */
  private def getNewConstructor(old_construtcor: DefDef, lockType: Type) = {
    // The field was not linked properly until this was added
    // this was inspired from:
    // Typers.scala line 1529
    // It is crazy that now I comment those and it still works!
    // talk about mind fuck. Actually it was wrong to enter it because then
    // we cannot have a parameter of the same name in other constructors.
    //        localTyper.namer.enterValueParams(
    //            List(List(param)) map (_.map(_.duplicate)))
    // Notice how we ask the class Symbol to generate the new param 
    // symbol, NOT the method symbol!
    val parentClasses = old_construtcor.symbol.owner.ancestors;
//        println("CLASS " + old_construtcor.symbol.owner)
//        println("PARENTS: " + parentClasses)
    
    val hasLocksInSupers = hasLockFieldInParents(parentClasses)

    // I think the flags are not as important as I though, most 
    // important is to create the ValDef using the cass symbol
    // but will need to check that more.
    val lock_sym = if (hasLocksInSupers) {println("HAS A SUPER " + old_construtcor.symbol)
      old_construtcor.symbol.newValueParameter(
        newTermName(as_lock), old_construtcor.symbol.pos.focus)
        .setInfo(lockType)
        .setFlag(PARAMACCESSOR | PARAM | SYNTHETIC)
    }
    else
      old_construtcor.symbol.owner.newValueParameter(
        newTermName(as_lock), old_construtcor.symbol.pos.focus)
        .setInfo(lockType)
        .setFlag(PARAM | PARAMACCESSOR | SYNTHETIC)
        
    val param = localTyper.typed(ValDef(lock_sym).setType(lockType)).asInstanceOf[ValDef]
    val newparamss = param :: old_construtcor.vparamss.head
    val pList = newparamss :: old_construtcor.vparamss.drop(1)

    val methDef = localTyper.typed(DefDef(old_construtcor.mods, old_construtcor.name, 
        old_construtcor.tparams, pList, old_construtcor.tpt.duplicate, 
        old_construtcor.rhs).setSymbol(old_construtcor.symbol))

    val methodInfo = methDef.symbol.info.asInstanceOf[MethodType]

    methDef.symbol.updateInfo(MethodType(
      lock_sym :: methodInfo.params, methodInfo.resultType))
      
    // it is annoying that trees that are not linked correctly to owner
    // symbols still generate bytecode, that is not usable.
    // submit example with the proxy issue to Amanj
    if(!hasLocksInSupers) methDef.symbol.owner.info.decls.enter(lock_sym)

    methDef
  }

  def transform(tree: Tree): Tree = {
    tree match {
      case constructor: DefDef if (hasSymbol(constructor) &&
        constructor.symbol.isConstructor &&
        !constructor.symbol.owner.isModule &&
        classSetsMap.contains(constructor.symbol.owner)) =>

        val newConstructor =
          getNewConstructor(constructor, getClassByName(lockClass).tpe)
        super.transform(newConstructor)

      case _ => super.transform(tree)
    }
  }
}


/********************** Phase 5: modify 'new' statements **********************/
/**
 * After adding the new constructor parameter, we need to update all 
 * constructor calls to include the new lock argument, and this is what we
 * do here. We also update 'super' calls here. 
 */
@phase("modify-new-stmts") class ModifyNewStmts {


  rightAfter("add-lock-fields")

  plugin AtomicScala 
   
  private def passNewLock(oldArgs: List[Tree], ownerClassTpt: Tree) = {
    /*
     * TODO create a special phase for this work here.
     * 
     * Remember that the modified constructor type are available and so
     * there is no need to change the associated symbol here, to prove
     * that to yourself: println(newStmt.symbol.infosString)
     * 
     * Also, arguements do not have symbols. No need to create a symbol
     * for the new argument. 
     * 
     * The newArg can be introduced like this too: 
     * val lockModuleSym = getLockSym(lockClass).companionModule
     * val newArg = Apply(
     *   Select(Ident(lockModuleSym), newTermName("apply")), Nil)
     */

    val newArg = reify { ch.usi.inf.l3.as.plugin.OrderedLock() }.tree
    val newArgs = newArg :: oldArgs
    val newStmtM = localTyper.typed(
        Apply(Select(New(ownerClassTpt), nme.CONSTRUCTOR), newArgs))
    newStmtM
  }

  def transform(tree: Tree): Tree = {
    tree match {
//          case s @DefDef(_,_,_,_,_,_) =>
//            println("METHOD HERE in " + s.symbol.enclClass)
//            println(s)
//            println(showRaw(s))
//            super.transform(tree)
        
      case valNewRHS @ ValDef(mods, name, tpt,
        Apply(Select(New(ntpt), nme.CONSTRUCTOR), args)) 
        if (classSetsMap.contains(ntpt.symbol)) =>

        val aliasA = getAnnotationInfo(valNewRHS, alias_anno) 
          
        aliasA match {
          case Some(_) =>
            val ownerClass = valNewRHS.symbol.enclClass
            val lock_f = 
              ownerClass.info.findMember(newTermName(as_lock), 0, 0, false)

            // TODO this should move to a special typechecking phase.
            if (lock_f == NoSymbol || lock_f == null)
              throw new Exception("Enclosing class does not have atomic sets")
            
            val newArg = Select(This(ownerClass), newTermName(as_lock))

            val newArgs = newArg :: args

            val newRHS = localTyper.typed(
              Apply(Select(New(ntpt), nme.CONSTRUCTOR), newArgs))
            val newvalDef = treeCopy.ValDef(valNewRHS, mods, name, tpt, newRHS)
            
            super.transform(localTyper.typed(newvalDef))

          case None =>
            val newRHS = passNewLock(args, ntpt)
            val newvalDef = treeCopy.ValDef(valNewRHS, mods, name, tpt, newRHS)
            super.transform(localTyper.typed(newvalDef))
        }

      case sc @ Apply(fn @ (Select(spr @ Super(ths @ This(klass), 
            m), nme.CONSTRUCTOR)), args) 
            // TODO this is fragile here without nullness checks, so do it later.
            if (classSetsMap.contains(spr.symbol.enclClass.parentSymbols.head)) =>
          //TODO I am not sure of it is fine taking the param of primary 
          // constructor here, or i need to select the exact surrounding 
          // consructor
          val lockValSym = ths.symbol.primaryConstructor.
            asInstanceOf[MethodSymbol].paramss.head.head
          
          val newArg = Ident(lockValSym) 
          val newArgs =  newArg :: args
          
          val newSelect = (Select(Super(This(klass), m), nme.CONSTRUCTOR))
         
          val newApply = localTyper.typed(Apply(newSelect, newArgs).setSymbol(sc.symbol))

          super.transform(newApply)
           
       case cnst @ DefDef(_, nme.CONSTRUCTOR, _, _, _, Block( lst @ List( 
           sc @ Apply( 
               fn @ Select( 
                   ths @ This(klass), nme.CONSTRUCTOR), args), _*), c)) 
//           TODO this is fragile here without nullness checks, so do it later.
      if (classSetsMap.contains(sc.symbol.enclClass.parentSymbols.head)) =>
            val lockValSym = cnst.vparamss.head.head;
            val newArg = Ident(lockValSym.symbol) //args.drop(1).head 
            val newArgs =  newArg :: args
            
            val newSelect = (Select(This(klass), nme.CONSTRUCTOR))
           
            val newApply = localTyper.typed(Apply(newSelect, newArgs).setSymbol(sc.symbol))
            
            val newBlock = Block(newApply::lst.drop(1), c)
            
            val newCnst = treeCopy.DefDef(
                cnst, cnst.mods, cnst.name, cnst.tparams, 
                cnst.vparamss, cnst.tpt, newBlock)
                
            super.transform(newCnst)
      case _ => super.transform(tree)
    }
  }
}




/*********************** Phase 6: add synchronization *************************/
/**
 * add synchronization blocks to each top-level public method in a class
 * that has one or more atomic sets.
 *
 * TODO currently no lock ordering or support for more than one atomic set.
 */
@phase("add-sync") class AddSync {

  rightAfter("modify-new-stmts")

  plugin AtomicScala 
  // Amanj: This method filters all parameters that don't have a certain annotation?
  private def paramsWithUnitForSyms(mParamsSyms: List[Symbol]): List[Symbol] = {
    mParamsSyms.foldLeft(List[Symbol]())(
      (c, r) =>
        {
          val annos = r.annotations
          val uforAnno = getAnnotationInfo(r, unitfor_anno)
          if (uforAnno != None) r :: c else c
        })
  }

  /**
   * Given another rhs implementation and a new name, returns a new
   * method ready to be plugged into the same class containing the original
   * prototype method.
   *
   * mt: Original method to be used as a prototype
   * nb: new rhs (body)
   * nName: the new Name
   */
  private def getMethodWithSyncBody(mt: DefDef, nName: String) = { 
    val encClassSym = mt.symbol.enclClass

    // TODO: remove this Amanj
    val newMethodSym = encClassSym.newMethodSymbol(
          newTermName(nName),
          encClassSym.pos.focus,
          mt.symbol.flags)

    // TODO: remove this Amanj
    val newParams = mt.vparamss.map(_.map( x => {
         val pSymbolType = x.symbol.tpe;
         val newParamSym = newMethodSym.newSyntheticValueParam(pSymbolType, x.name)
         localTyper.typed(ValDef(newParamSym, x.tpt.duplicate)).asInstanceOf[ValDef]
        })) 

    val syncBody = addSyncBlock( mt.rhs.duplicate: Tree, newParams, 
            newMethodSym: Symbol, mt.tpt.duplicate)

    mkDefDef(mt.symbol.flags, newTermName(nName), mt.tparams,
        mt.vparamss, mt.tpt.tpe, syncBody, encClassSym)
  }

  // private def nonNullParamList(pListSym: Symbol) = {
  //   Amanj
  //   ValDef(Modifiers(), newTermName("__params_non_null"), TypeTree(),
  //     Apply(Select(Ident(pListSym),
  //       newTermName("filter")), List(Function(
  //       List(ValDef(Modifiers(PARAM), newTermName("x"), TypeTree(), 
  //           EmptyTree)),
  //       Apply(Select(Ident(newTermName("x")), newTermName("$bang$eq")),
  //         List(Literal(Constant(null))))))))
  // }
    

  // private def paramLocks(pListSym: Symbol) = {
  // Amanj
  //   val slct = mkSelect(pListSym, "map")

  //   val arg = mkFunction(List(mkParam("x", TypeTree())), apply)

  //     
  //     Function(
  //       List(ValDef(Modifiers(PARAM), newTermName("x"), TypeTree(), 
  //           EmptyTree)),
  //       Apply(Select(Ident(newTermName("x")), newTermName(as_lock)),
  //         List(Literal(Constant(null)))))
  //   val rhs = mkApply
  //   val rhs = Apply(slct, List(arg))
  //   ValDef(Modifiers(), newTermName("__param_locks"), TypeTree(), rhs)
  // }
  
  private def paramLocks(pl: List[Symbol]) = {
    val p_ufor =  paramsWithUnitForSyms(pl)
    p_ufor.map(x => mkSelect(x, as_lock))
  }

  private def composeSync(locksList_sym: Symbol, n_locks: Int, 
      tpt: Tree, body: Tree): Tree = {
    // Neve
    if (n_locks > 0) {
      val listApplyInt = mkSelect(locksList_sym, "apply")
      val applyCall = mkApply(listApplyInt, List(mkLiteral(n_locks - 1)))

      val syncStmt = mkSynchronized(body, applyCall, tpt)
      composeSync(locksList_sym, n_locks - 1, tpt, syncStmt)
    } else {
      body
    }
  }

  /**
   * given a method, takes the body and surrounds it with a synch block on
   * the lock object of the enclosing class.
   *
   * TODO think what to do when the method body is already synced on another
   * lock object. Probably the same should be done as a normal method but
   * ask Nate
   */
  private def addSyncBlock(mbody: Tree, mParams:List[List[ValDef]], 
            msymbol: Symbol, mtpt: Tree) = {
    val mthdEncClass = msymbol.owner
    val lockModuleSym = getClassByName(lockClass).companionModule
    
    /*it is VERY important to type the tree right here because we are 
     * using its type to construct a type tree an the enclosing tree.
     * It does not work to type the overall tree later.
     * */
    val classLockField = mkSelect(This(mthdEncClass), as_lock)
    
    val mthdParams = mParams.flatten.map(_.symbol)
    val mthdParamsIdent = mthdParams.map(x => Ident(x))
      
    // TODO: What about easier use of definitions?
    val paramListRHS = mkApply(mkSelect(definitions.ListModule, "apply"),
                                List(TypeTree(definitions.AnyTpe)),
                                mthdParamsIdent)

    val paramList = mkVal(msymbol, "__m_params", paramListRHS)
      
          
    // val nonNullParams = nonNullParamList(paramList_sym)
    
    val p_locks = paramLocks(mthdParams)
    val all_locks = classLockField :: p_locks
    // The call to Apply method of a List used to create a new list
    val lockList = mkApply(mkSelect(definitions.ListModule, "apply"),
              List(TypeTree(classLockField.tpe)), all_locks) 
    
    /* the compiler is smarter than I thought. It found the method and the
     * local typer bound it correctly! */
    val sortedLockList = mkSelect(lockList, "sorted")
    
    val locksList = mkVal(mbody.symbol, "locks", sortedLockList)
    
        // It seems that calling duplicate is needed, otherwise I get an
    // exception.
    val nestedSync = 
      composeSync(locksList.symbol, all_locks.size, 
          mtpt.duplicate , mbody.duplicate)
    
    val blockToInsert = Block(List(paramList, locksList), nestedSync)
    blockToInsert
  }

  def transform(tree: Tree): Tree = {

    tree match {
      case cl: ClassDef =>
        val newClass = localTyper.typed {
          treeCopy.ClassDef(cl, cl.mods,
            cl.symbol.name, cl.tparams,
            treeCopy.Template(cl.impl,
              cl.impl.parents,
              cl.impl.self,
              cl.impl.body.foldLeft(cl.impl.body)((c, r) =>
                methodTransform(r) match {
                  case nm @ Some(_) => nm.get :: c
                  case None => c
                })))
        }.asInstanceOf[ClassDef]
        super.transform(newClass)
            
      case _ => super.transform(tree)
    }
  }

  private def methodTransform(tree: Tree): Option[Tree] = {
    //TODO think what other kinds of methods need or need not be synced
    tree match {
      case md: DefDef if hasSymbol(md) &&
        md.symbol.isMethod &&
        !md.symbol.isConstructor &&
        md.mods.isPublic &&
        classSetsMap.contains(md.symbol.owner) &&
        !md.symbol.isSynthetic &&
        !md.symbol.isGetter =>
          
        Some(getMethodWithSyncBody(
          md, md.name + "__w_lock"))
      case _ => None
    }
  }
}
