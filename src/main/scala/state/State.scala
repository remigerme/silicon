// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2019 ETH Zurich.

package viper.silicon.state

import viper.silicon.Config.JoinMode
import viper.silicon.Config.JoinMode.JoinMode
import viper.silver.ast
import viper.silver.cfg.silver.SilverCfg
import viper.silicon.common.Mergeable
import viper.silicon.common.collections.immutable.InsertionOrderedSet
import viper.silicon.decider.RecordedPathConditions
import viper.silicon.interfaces.state.GeneralChunk
import viper.silicon.state.State.OldHeaps
import viper.silicon.state.terms.{Term, Var}
import viper.silicon.interfaces.state.Chunk
import viper.silicon.state.terms.predef.`?r`
import viper.silicon.state.terms.{And, Ite}
import viper.silicon.supporters.PredicateData
import viper.silicon.supporters.functions.{FunctionData, FunctionRecorder, NoopFunctionRecorder}
import viper.silicon.utils.ast.BigAnd
import viper.silicon.verifier.Verifier
import viper.silicon.{Map, Stack}
import viper.silver.utility.Sanitizer

final case class State(g: Store = Store(),
                       h: Heap = Heap(),
                       program: ast.Program,
                       currentMember: Option[ast.Member],
                       predicateData: Map[String, PredicateData],
                       functionData: Map[String, FunctionData],
                       oldHeaps: OldHeaps = Map.empty,

                       parallelizeBranches: Boolean = false,

                       recordVisited: Boolean = false,
                       visited: List[ast.Predicate] = Nil, /* TODO: Use a multiset instead of a list */

                       methodCfg: SilverCfg = null,
                       invariantContexts: Stack[Heap] = Stack.empty,

                       constrainableARPs: InsertionOrderedSet[Var] = InsertionOrderedSet.empty,
                       quantifiedVariables: Stack[(Var, Option[ast.AbstractLocalVar])] = Nil,
                       retrying: Boolean = false,
                       underJoin: Boolean = false,
                       functionRecorder: FunctionRecorder = NoopFunctionRecorder,
                       conservingSnapshotGeneration: Boolean = false,
                       recordPossibleTriggers: Boolean = false,
                       possibleTriggers: Map[ast.Exp, Term] = Map(),

                       triggerExp: Boolean = false,

                       partiallyConsumedHeap: Option[Heap] = None,
                       permissionScalingFactor: Term = terms.FullPerm,
                       permissionScalingFactorExp: Option[ast.Exp] = if (Verifier.config.enableDebugging()) Some(ast.FullPerm()()) else None,
                       isEvalInOld: Boolean = false,

                       reserveHeaps: Stack[Heap] = Nil,
                       reserveCfgs: Stack[SilverCfg] = Stack(),
                       conservedPcs: Stack[Vector[RecordedPathConditions]] = Stack(),
                       recordPcs: Boolean = false,
                       exhaleExt: Boolean = false,
                       isInPackage: Boolean = false,
                       equatedSnapshots: Vector[Term] = Vector(),

                       ssCache: SsCache = Map.empty,
                       assertReadAccessOnly: Boolean = false,

                       qpFields: InsertionOrderedSet[ast.Field] = InsertionOrderedSet.empty,
                       qpPredicates: InsertionOrderedSet[ast.Predicate] = InsertionOrderedSet.empty,
                       qpMagicWands: InsertionOrderedSet[MagicWandIdentifier] = InsertionOrderedSet.empty,
                       permLocations: InsertionOrderedSet[ast.Location] = InsertionOrderedSet.empty,
                       smCache: SnapshotMapCache = SnapshotMapCache.empty,
                       pmCache: PmCache = Map.empty,
                       smDomainNeeded: Boolean = false,
                       /* TODO: Isn't this data stable, i.e. fully known after a preprocessing step? If so, move it to the appropriate supporter. */
                       predicateSnapMap: Map[String, terms.Sort] = Map.empty,
                       predicateFormalVarMap: Map[String, Seq[terms.Var]] = Map.empty,
                       retryLevel: Int = 0,
                       /* ast.Field, ast.Predicate, or MagicWandIdentifier */
                       heapDependentTriggers: InsertionOrderedSet[Any] = InsertionOrderedSet.empty,
                       moreCompleteExhale: Boolean = false,
                       moreJoins: JoinMode = JoinMode.Off)
    extends Mergeable[State] {

  val isMethodVerification: Boolean = {
    // currentMember being None means we're verifying a CFG; this should behave like verifying a method.
    currentMember.isEmpty || currentMember.get.isInstanceOf[ast.Method]
  }

  def isUsedAsTrigger(res: ast.Resource): Boolean = {
    val identifier = res match {
      case mw: ast.MagicWand => MagicWandIdentifier(mw, program)
      case _ => res
    }
    heapDependentTriggers.contains(identifier)
  }

  def isQuantifiedResource(res: ast.Resource): Boolean = {
    res match {
      case f: ast.Field => qpFields.contains(f)
      case p: ast.Predicate => qpPredicates.contains(p)
      case mw: ast.MagicWand => qpMagicWands.contains(MagicWandIdentifier(mw, program))
    }
  }

  def getFormalArgVars(res: ast.Resource, v: Verifier): Seq[Var] = {
    res match {
      case _: ast.Field => Seq(`?r`)
      case p: ast.Predicate => predicateFormalVarMap(p.name)
      case w: ast.MagicWand =>
        val bodyVars = w.subexpressionsToEvaluate(program)
        bodyVars.indices.toList.map(i => Var(Identifier(s"x$i"), v.symbolConverter.toSort(bodyVars(i).typ), false))
    }
  }

  def getFormalArgDecls(res: ast.Resource): Seq[ast.LocalVarDecl] = {
    res match {
      case _: ast.Field => Seq(ast.LocalVarDecl("r", ast.Ref)())
      case p: ast.Predicate => p.formalArgs
      case w: ast.MagicWand =>
        val bodyVars = w.subexpressionsToEvaluate(program)
        bodyVars.indices.toList.map(i => ast.LocalVarDecl(s"x$i", bodyVars(i).typ)())
    }
  }

  val mayAssumeUpperBounds: Boolean = {
    currentMember.isEmpty || !currentMember.get.isInstanceOf[ast.Function] || Verifier.config.respectFunctionPrePermAmounts()
  }

  val isLastRetry: Boolean = retryLevel == 0

  def incCycleCounter(m: ast.Predicate) =
    if (recordVisited) copy(visited = m :: visited)
    else this

  def decCycleCounter(m: ast.Member) =
    if (recordVisited) {
      require(visited.contains(m))

      val (ms, others) = visited.partition(_ == m)
      copy(visited = ms.tail ::: others)
    }
  else
    this

  def cycles(m: ast.Member) = visited.count(_ == m)

  def setConstrainable(arps: Iterable[Var], constrainable: Boolean) = {
    val newConstrainableARPs =
      if (constrainable) constrainableARPs ++ arps
      else constrainableARPs -- arps

    copy(constrainableARPs = newConstrainableARPs)
  }

  def scalePermissionFactor(p: Term, exp: Option[ast.Exp]) =
    copy(permissionScalingFactor = terms.PermTimes(p, permissionScalingFactor),
      permissionScalingFactorExp = permissionScalingFactorExp.map(psf => ast.PermMul(exp.get, psf)(exp.get.pos, exp.get.info, exp.get.errT)))

  def merge(other: State): State =
    State.merge(this, other)

  def preserveAfterLocalEvaluation(post: State): State =
    State.preserveAfterLocalEvaluation(this, post)

  def functionRecorderQuantifiedVariables(): Seq[(Var, Option[ast.AbstractLocalVar])] =
    functionRecorder.arguments.fold(Seq.empty[(Var, Option[ast.AbstractLocalVar])])(d => d)

  def relevantQuantifiedVariables(filterPredicate: Var => Boolean): Seq[(Var, Option[ast.AbstractLocalVar])] = (
       functionRecorderQuantifiedVariables()
    ++ quantifiedVariables.filter(x => filterPredicate(x._1))
  )

  def relevantQuantifiedVariables(occurringIn: Seq[Term]): Seq[(Var, Option[ast.AbstractLocalVar])] =
    relevantQuantifiedVariables(x => occurringIn.exists(_.contains(x)))


  def substituteVarsInExp(e : ast.Exp): ast.Exp = {
    val varMapping = g.expValues.map { case (localVar, finalExp) => localVar.name -> finalExp}
    Sanitizer.replaceFreeVariablesInExpression(e, varMapping.map(vm => vm._1 -> vm._2.get), Set())
  }

  lazy val relevantQuantifiedVariables: Seq[(Var, Option[ast.AbstractLocalVar])] =
    relevantQuantifiedVariables(_ => true)

  override val toString = s"${this.getClass.getSimpleName}(...)"
}

object State {
  type OldHeaps = Map[String, Heap]
  val OldHeaps = Map

  def merge(s1: State, s2: State): State = {
    s1 match {
      /* Decompose state s1 */
      case State(g1, h1, program, member,
                 predicateData,
                 functionData,
                 oldHeaps1,
                 parallelizeBranches1,
                 recordVisited1, visited1,
                 methodCfg1, invariantContexts1,
                 constrainableARPs1,
                 quantifiedVariables1,
                 retrying1,
                 underJoin1,
                 functionRecorder1,
                 conservingSnapshotGeneration1,
                 recordPossibleTriggers1, possibleTriggers1,
                 triggerExp1,
                 partiallyConsumedHeap1,
                 permissionScalingFactor1, permissionScalingFactorExp1, isEvalInOld,
                 reserveHeaps1, reserveCfgs1, conservedPcs1, recordPcs1, exhaleExt1, isInPackage1, equatedSnapshots1,
                 ssCache1, assertReadAccessOnly1,
                 qpFields1, qpPredicates1, qpMagicWands1, permResources1, smCache1, pmCache1, smDomainNeeded1,
                 predicateSnapMap1, predicateFormalVarMap1, retryLevel, useHeapTriggers,
                 moreCompleteExhale, moreJoins) =>

        /* Decompose state s2: most values must match those of s1 */
        s2 match {
          case State(`g1`, `h1`,
                     `program`, `member`,
                     `predicateData`, `functionData`,
                     oldHeaps2,
                     `parallelizeBranches1`,
                     `recordVisited1`, `visited1`,
                     `methodCfg1`, `invariantContexts1`,
                     constrainableARPs2,
                     quantifiedVariables2,
                     `retrying1`,
                     `underJoin1`,
                     functionRecorder2,
                     `conservingSnapshotGeneration1`,
                     `recordPossibleTriggers1`, possibleTriggers2,
                     triggerExp2,
                     `partiallyConsumedHeap1`,
                     `permissionScalingFactor1`, `permissionScalingFactorExp1`, `isEvalInOld`,
                     `reserveHeaps1`, `reserveCfgs1`, conservedPcs2, `recordPcs1`, `exhaleExt1`, `isInPackage1`, `equatedSnapshots1`,
                     ssCache2, `assertReadAccessOnly1`,
                     `qpFields1`, `qpPredicates1`, `qpMagicWands1`, `permResources1`, smCache2, pmCache2, `smDomainNeeded1`,
                     `predicateSnapMap1`, `predicateFormalVarMap1`, `retryLevel`, `useHeapTriggers`,
                     moreCompleteExhale2, `moreJoins`) =>

            val oldHeaps3 = oldHeaps1 ++ oldHeaps2
            val functionRecorder3 = functionRecorder1.merge(functionRecorder2)
            val triggerExp3 = triggerExp1 && triggerExp2
            val possibleTriggers3 = possibleTriggers1 ++ possibleTriggers2
            val constrainableARPs3 = constrainableARPs1 ++ constrainableARPs2
            val quantifiedVariables3 = (quantifiedVariables1 ++ quantifiedVariables2).distinct

            val smCache3 = smCache1.union(smCache2)
            val pmCache3 = pmCache1 ++ pmCache2

            val ssCache3 = ssCache1 ++ ssCache2
            val moreCompleteExhale3 = moreCompleteExhale || moreCompleteExhale2

            assert(conservedPcs1.length == conservedPcs2.length)
            val conservedPcs3 = conservedPcs1
              .zip(conservedPcs2)
              .map({ case (pcs1, pcs2) => (pcs1 ++ pcs2).distinct })

            s1.copy(oldHeaps = oldHeaps3,
                    functionRecorder = functionRecorder3,
                    possibleTriggers = possibleTriggers3,
                    triggerExp = triggerExp3,
                    constrainableARPs = constrainableARPs3,
                    quantifiedVariables = quantifiedVariables3,
                    ssCache = ssCache3,
                    smCache = smCache3,
                    pmCache = pmCache3,
                    moreCompleteExhale = moreCompleteExhale3,
                    conservedPcs = conservedPcs3)

          case _ =>
            val err = new StringBuilder()
            for (ix <- 0 until s1.productArity) {
              val e1 = s1.productElement(ix)
              val e2 = s2.productElement(ix)
              if (e1 != e2) {
                err ++= s"\n- Field index ${s1.productElementName(ix)} not equal."
              }
            }
            sys.error(s"State merging failed: unexpected mismatch between symbolic states: $err")
      }
    }
  }

  // Lists all fields which do not match in several states.
  // Not fully sure this is super relevant but this was there before so.
  private def generateStateMismatchErrorMessage(states: Seq[State]): Nothing = {
    require(states.nonEmpty, "Cannot generate mismatch error message for an empty collection of states.")

    val err = new StringBuilder()
    for (ix <- 0 until states.head.productArity) yield {
      val expected = states.head.productElement(ix)
      val e = states.map(_.productElement(ix))
      if (e.exists(_ != expected)) {
        err ++= s"\n\tField index ${states.head.productElementName(ix)} not equal"
        e.zipWithIndex.foreach({case (v, i) => {
          err ++= s"\n\t\t state$i: $v"
        }})
      }
    }

    sys.error(s"State merging failed: unexpected mismatch between symbolic states: $err")
  }

  // Merge several maps at once.
  // If a key is missing in at least one map, the entry will be discarded in the resulting map.
  private def mergeMaps[K, V](mapsAndBranchConditions: Seq[(Map[K, V], Term, Option[ast.Exp])])
                             (mergeEntries: Seq[(V, Term, Option[ast.Exp])] => V)
                             : Map[K, V] = {
    val maps = mapsAndBranchConditions.map(_._1)
    val universalKeys = maps.head.keys.filter(k => maps.tail.forall(_.contains(k)))

    Map.from(universalKeys.map(k => {
      val entriesAndBranchConditions = mapsAndBranchConditions.map({ case (m, bc, bcExp) => (m(k), bc, bcExp)})
      (k, mergeEntries(entriesAndBranchConditions))
    }))
  }

  def mergeBindings(bindingsAndBranchConditions: Seq[(Map[ast.AbstractLocalVar, (Term, Option[ast.Exp])], Term, Option[ast.Exp])])
                   : Map[ast.AbstractLocalVar, (Term, Option[ast.Exp])] = {
    mergeMaps(bindingsAndBranchConditions)(localsAndBranchConditions => {
      // Checking if all entries are the same
      val locals = localsAndBranchConditions.map(_._1)
      if (locals.forall(_._1 == locals.head._1)) {
        locals.head
      } else {
        assert(locals.forall(_._1.sort == locals.head._1.sort))
        // Naively cascading Ite terms
        localsAndBranchConditions.tail.foldLeft(locals.head)({
          case (acc, (local, bc, bcExp)) =>
            (Ite(bc, local._1, acc._1), bcExp.map(cond => ast.CondExp(cond, local._2.get, acc._2.get)()))
        })
      }
    })
  }

  // Puts a collection of chunks under a condition.
  private def conditionalizeChunks(h: Iterable[Chunk], cond: Term, condExp: Option[ast.Exp]): Iterable[Chunk] = {
    h map (c => {
      c match {
        case c: GeneralChunk =>
          c.applyCondition(cond, condExp)
        case _ => sys.error("Chunk type not conditionalizable.")
      }
    })
  }

  // Merges two heaps together, by putting h1 under condition cond1,
  // and h2 under cond2.
  // Assumes that cond1 is the negation of cond2.
  def mergeHeap(h1: Heap, cond1: Term, cond1Exp: Option[ast.Exp], h2: Heap, cond2: Term, cond2Exp: Option[ast.Exp]): Heap = {
    val (unconditionalHeapChunks, h1HeapChunksToConditionalize) = h1.values.partition(c1 => h2.values.exists(_ == c1))
    val h2HeapChunksToConditionalize = h2.values.filter(c2 => !unconditionalHeapChunks.exists(_ == c2))
    val h1ConditionalizedHeapChunks = conditionalizeChunks(h1HeapChunksToConditionalize, cond1, cond1Exp)
    val h2ConditionalizedHeapChunks = conditionalizeChunks(h2HeapChunksToConditionalize, cond2, cond2Exp)
    Heap(unconditionalHeapChunks) + Heap(h1ConditionalizedHeapChunks) + Heap(h2ConditionalizedHeapChunks)
  }

  def mergeHeaps(heapsAndBranchConditions: Seq[(Heap, Term, Option[ast.Exp])], conditionalizeSingleHeap: Boolean): Heap = {
    require(heapsAndBranchConditions.nonEmpty, "Cannot merge an empty collection of heaps")
    val heaps = heapsAndBranchConditions.map(_._1)

    // First we determine the unconditional chunks present in every heap.
    // Note that if we try to merge only a single heap, it will not be conditionalized
    // unless `conditionalizeSingleHeap` is set to true. This is to replicate the
    // `partiallyConsumedHeap` behavior of the former implementation.
    val atLeastTwoHeaps = heaps.length >= 2
    val unconditionalChunks = heaps.head.values.filter(ch =>
      (atLeastTwoHeaps || !conditionalizeSingleHeap) && heaps.tail.forall(h => h.values.exists(_ == ch)))

    // And all other chunks must be conditionalized
    val conditionalizedChunks = heapsAndBranchConditions.map({case (h, bc, bcExp) =>
      val chunksToConditionalize = h.values.filter(ch => !unconditionalChunks.exists(_ == ch))
      conditionalizeChunks(chunksToConditionalize, bc, bcExp)}).flatten

    Heap(unconditionalChunks) + Heap(conditionalizedChunks)
  }

  def merge(s1: State, pc1: RecordedPathConditions, s2: State, pc2: RecordedPathConditions): State = {
    val bc1 = And(pc1.branchConditions)
    val withExp = Verifier.config.enableDebugging()
    val bc1Exp = if (withExp) Some(BigAnd(pc1.branchConditionExps.map(_._2.get))) else None
    val bc2 = And(pc2.branchConditions)
    val bc2Exp = if (withExp) Some(BigAnd(pc2.branchConditionExps.map(_._2.get))) else None
    merge(s1, bc1, bc1Exp, s2, bc2, bc2Exp)
  }

  def merge(s1: State, bc1: Term, bc1Exp: Option[ast.Exp], s2: State, bc2: Term, bc2Exp: Option[ast.Exp]): State = {
    merge(Seq((s1, bc1, bc1Exp), (s2, bc2, bc2Exp)))
  }

  private def mergeCheckInputs(states: Seq[State]): Unit = {
    require(states.nonEmpty, "Cannot merge an empty collection of states")

    def allEqualOne[P](extractField: State => P): Boolean = {
      val expectedVal = extractField(states.head)
      states.tail.forall(extractField(_) == expectedVal)
    }

    def allEqual(extractFields: Seq[State => Any]): Boolean = {
      extractFields.forall(allEqualOne)
    }

    val equalFields: Seq[State => Any] = Seq(
      _.program, _.currentMember,
      _.predicateData, _.functionData,
      _.parallelizeBranches,
      _.recordVisited, _.visited,
      _.methodCfg,
      _.quantifiedVariables,
      _.retrying,
      _.underJoin,
      _.conservingSnapshotGeneration,
      _.recordPossibleTriggers,
      _.permissionScalingFactor, _.permissionScalingFactorExp, _.isEvalInOld,
      _.reserveCfgs, _.recordPcs, _.exhaleExt, _.isInPackage,
      _.assertReadAccessOnly,
      _.qpFields, _.qpPredicates, _.qpMagicWands, _.permLocations,
      _.predicateSnapMap, _.predicateFormalVarMap, _.retryLevel, _.heapDependentTriggers,
      _.moreJoins
      )

    if (!allEqual(equalFields)) {
      generateStateMismatchErrorMessage(states)
    }
  }

  def merge(statesAndBranchConditions: Seq[(State, Term, Option[ast.Exp])]): State = {
    require(statesAndBranchConditions.nonEmpty, "Cannot merge an empty collection of states")

    // Merging a single state is a no-op and won't conditionalize the store/heaps.

    val states = statesAndBranchConditions.map(_._1)
    mergeCheckInputs(states)

    val functionRecorder = states.map(_.functionRecorder).reduce(_.merge(_))
    val triggerExp = states.map(_.triggerExp).reduce(_ && _)
    val possibleTriggers = states.map(_.possibleTriggers).reduce(_ ++ _)
    val constrainableARPs = states.map(_.constrainableARPs).reduce(_ ++ _)
    val smDomainNeeded = states.map(_.smDomainNeeded).reduce(_ || _)
    val moreCompleteExhale = states.map(_.moreCompleteExhale).reduce(_ || _)

    val g = Store(mergeBindings(statesAndBranchConditions.map({case (s, bc, bcExp) => (s.g.values, bc, bcExp)})))    
    val h = mergeHeaps(statesAndBranchConditions.map({case (s, bc, bcExp) => (s.h, bc, bcExp)}), false)

    val partiallyConsumedHeap = statesAndBranchConditions.map({case (s, bc, bcExp) => 
      s.partiallyConsumedHeap match {
        case None => None
        case Some(pch) => Some(pch, bc, bcExp)
      }}).flatten match {
        case Seq() => None
        case heapsAndBranchConditions => Some(mergeHeaps(heapsAndBranchConditions, true))
      }

    val oldHeaps = Map.from(mergeMaps(statesAndBranchConditions.map({case (s, bc, bcExp) => (s.oldHeaps, bc, bcExp)}))
      (heapsAndBranchConditions => mergeHeaps(heapsAndBranchConditions, false)))
  
    val expectedInvariantContextsLength = states.head.invariantContexts.length
    assert(states.tail.forall(_.invariantContexts.length == expectedInvariantContextsLength))
    val invariantContexts = (0 until expectedInvariantContextsLength).map(i =>
      mergeHeaps(statesAndBranchConditions.map({case (s, bc, bcExp) => (s.invariantContexts(i), bc, bcExp)}), false))

    val expectedReserveHeapsLength = states.head.reserveHeaps.length
    assert(states.tail.forall(_.reserveHeaps.length == expectedReserveHeapsLength))
    val reserveHeaps = (0 until expectedReserveHeapsLength).map(i =>
      mergeHeaps(statesAndBranchConditions.map({case (s, bc, bcExp) => (s.reserveHeaps(i), bc, bcExp)}), false))

    val expectedConservedPcsLength = states.head.conservedPcs.length
    assert(states.tail.forall(_.conservedPcs.length == expectedConservedPcsLength))
    val conservedPcs = (0 until expectedConservedPcsLength).map(i =>
      states.map(_.conservedPcs(i)).reduce((pcs1, pcs2) => (pcs1 ++ pcs2).distinct))

    val ssCache = states.map(_.ssCache).reduce(_ ++ _)
    val smCache = states.map(_.smCache).reduce(_.union(_))
    val pmCache = states.map(_.pmCache).reduce(_ ++ _)

    val equatedSnapshots = statesAndBranchConditions.foldLeft(Vector[Term]())({case (eqs, cur) => {
      val (s, bc, _) = cur
      val eqsCur = s.equatedSnapshots.map(t => Ite((bc, t, terms.True)))
      eqs ++ eqsCur
    }})

    val stateMerged = states.head.copy(functionRecorder = functionRecorder,
                                       possibleTriggers = possibleTriggers,
                                       triggerExp = triggerExp,
                                       constrainableARPs = constrainableARPs,
                                       moreCompleteExhale = moreCompleteExhale,
                                       ssCache = ssCache,
                                       smCache = smCache,
                                       pmCache = pmCache,
                                       g = g,
                                       h = h,
                                       oldHeaps = oldHeaps,
                                       partiallyConsumedHeap = partiallyConsumedHeap,
                                       smDomainNeeded = smDomainNeeded,
                                       invariantContexts = invariantContexts,
                                       reserveHeaps = reserveHeaps,
                                       conservedPcs = conservedPcs,
                                       equatedSnapshots = equatedSnapshots)

    // Optionally, we could also do a state consolidation after each
    // state merging, but this has shown to decrease performance a bit.
    //val stateRet = verifier.stateConsolidator.consolidate(stateMerged, verifier)
    //stateRet

    stateMerged
  }

  def preserveAfterLocalEvaluation(pre: State, post: State): State = {
    pre.copy(functionRecorder = post.functionRecorder,
             possibleTriggers = post.possibleTriggers,
             smCache = post.smCache,
             constrainableARPs = post.constrainableARPs)
  }

  def conflictFreeUnionOrAbort[K, V](m1: Map[K, V], m2: Map[K, V]): Map[K,V] =
    viper.silicon.utils.conflictFreeUnion(m1, m2) match {
      case (m3, conflicts) if conflicts.isEmpty => m3
      case _ => sys.error("Unexpected mismatch between contexts")
    }

  def merge[M <: Mergeable[M]](candidate1: Option[M], candidate2: Option[M]): Option[M] =
    (candidate1, candidate2) match {
      case (Some(m1), Some(m2)) => Some(m1.merge(m2))
      case (None, None) => None
      case _ => sys.error("Unexpected mismatch between contexts")
    }
}
