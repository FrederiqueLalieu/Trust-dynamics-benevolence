package asl
 import _root_.scala.collection.mutable.HashMap

 import _root_.akka.actor.typed.{ActorRef, Behavior, SupervisorStrategy}
 import _root_.akka.actor.typed.scaladsl.{ActorContext, Behaviors, Routers}
 import java.util.logging.Logger
 import _root_.scala.util.Failure
 import _root_.scala.util.Success
 import _root_.akka.util.Timeout
 import _root_.scala.concurrent.duration._
 import _root_.akka.actor.typed.scaladsl.AskPattern._
 import _root_.scala.language.implicitConversions
 import _root_.scala.concurrent.{Await, Future}
 import _root_.scala.jdk.CollectionConverters._
 import std.converters._

 import scala.util.Random
 import bb._
 import infrastructure._
 import bb.expstyla.exp._
 import std.{AgentCommunicationLayer, DefaultCommunications}

 class reasoning  (coms: AgentCommunicationLayer = new  DefaultCommunications,
                                     beliefBaseFactory: IBeliefBaseFactory = new StylaBeliefBaseFactory)
                      extends IntentionalAgentFactory {


 object Intention {

       def apply(p_executionContext: ExecutionContext): Behavior[ISubGoalMessage] = Behaviors.setup { context =>

         Behaviors.receive {
         (context, message) =>

          implicit val executionContext = p_executionContext.copy(intention = context, src = message.source)

         message match {
            case SubGoalMessage(_,_,r) =>
               message.goal match {

                   case reasoning.this.adopt_achievement_is_contradiction_3 =>
                     reasoning.this.adopt_achievement_is_contradiction_3.execute(message.params.asInstanceOf[Parameters])

                   case reasoning.this.adopt_achievement_find_contradiction_2 =>
                     reasoning.this.adopt_achievement_find_contradiction_2.execute(message.params.asInstanceOf[Parameters])

                   case reasoning.this.adopt_achievement_resolve_contradiction_2 =>
                     reasoning.this.adopt_achievement_resolve_contradiction_2.execute(message.params.asInstanceOf[Parameters])


           case _ =>
             context.log.error("This actor can not handle goal of type {}", message.goal)
         }
           r match {
                 case a : AkkaMessageSource => 
                   a.src ! IntentionDoneMessage(AkkaMessageSource(executionContext.agent.self))
                 case DummyMessageSource(_) => 
                   context.log.error("Intention Done!")
               }

               Behaviors.same
             case InitEndMessage(r) =>
               Behaviors.stopped
       }
      }
     }
     }

 override def agentBuilder: Agent = new Agent()
 class Agent extends IAgent {

         override def agent_type: String = "reasoning"

         var vars = VarMap()

         def initGoals()(implicit executionContext: ExecutionContext) = List[StructTerm](
         )

         def initBeliefs()(implicit executionContext: ExecutionContext) = List[StructTerm](
                     StructTerm("rumour",Seq[GenericTerm](StructTerm("a",Seq[GenericTerm]()),StructTerm("doctor",Seq[GenericTerm](StructTerm("john",Seq[GenericTerm]())))))
           ,
            StructTerm("rumour",Seq[GenericTerm](StructTerm("b",Seq[GenericTerm]()),StructTerm("evil",Seq[GenericTerm](StructTerm("john",Seq[GenericTerm]())))))
           ,
            StructTerm("rumour",Seq[GenericTerm](StructTerm("a",Seq[GenericTerm]()),StructTerm("reject",Seq[GenericTerm](StructTerm("offer",Seq[GenericTerm]()),StructTerm("fun",Seq[GenericTerm]())))))
           ,
            StructTerm("trust",Seq[GenericTerm](StructTerm("a",Seq[GenericTerm]()),DoubleTerm(0.6)))
           ,
            StructTerm("trust",Seq[GenericTerm](StructTerm("b",Seq[GenericTerm]()),DoubleTerm(0.7)))


         )

         def planApplicabilities()(implicit executionContext: ExecutionContext) = List[StructTerm](

                 )



      def apply(name: String, yellowPages: IYellowPages, MAS: ActorRef[IMessage], parent: IMessageSource): Behavior[IMessage] = {
           Behaviors.setup { context =>
             val yp: IYellowPages = yellowPages
             val bb: IBeliefBase[GenericTerm] = beliefBaseFactory()
             implicit val executionContext: ExecutionContext = ExecutionContext(
                            name = name,
                            agentType = agent_type,
                            agent = context,
                            yellowPages = yp,
                            beliefBase = bb,
                            logger = context.log,
                            goalParser = GoalParser,
                            parent = parent
                          )
             bb.assert(initBeliefs)
             bb.assert(planApplicabilities)

         val initiator = context.spawn(Intention(executionContext), "initiator")

         MAS ! ReadyMessage(context.self)
         Behaviors.receive {
           (context, message) =>
             message match {
               case StartMessage() =>


                 implicit val timeout: Timeout = 99999.seconds
                 implicit val ec = context.executionContext
                 implicit val scheduler = context.system.scheduler


                 //              initGoals.foreach( tuple => initiator ! SubGoalMessage(tuple._1,tuple._2,context.self))
                 initGoals.foreach(struct => {


                   val result: Future[IMessage] = initiator.ask[IMessage](ref => {
                     val subGoal = GoalParser.create_goal_message(struct, AkkaMessageSource(ref))
                     if (subGoal.isDefined)
                       subGoal.get
                     else
                       throw new RuntimeException(s"No plan for initial goal $struct")
                     }
                   )


                   //result.onComplete {
                   //  case Success(IntentionDoneMessage(r)) => IntentionDoneMessage(r)
                   //  case Failure(_) => IntentionErrorMessage(src = null)
                   //}

                   //Await.result(result, timeout.duration)

                   val res = Await.result(result, timeout.duration)

                   if(!res.isInstanceOf[IntentionDoneMessage]) {
                     throw new RuntimeException(s"Issue with initial goal $struct")
                     context.system.terminate()
                   }

                   //                context.ask[ISubGoalMessage, IMessage](initiator, ref => SubGoalMessage(tuple._1, tuple._2,name, ref)) {
                   //                  case Success(IntentionDoneMessage(_, _)) => IntentionDoneMessage()
                   //                  case Failure(_) => IntentionErrorMessage()
                   //                }
                 }
                 )

                 initiator ! InitEndMessage(context.self)
                 normal_behavior(MAS)

               //            case InitEndMessage(_) =>
               //              context.log.debug(f"$name: I have started, switching behavior")
               //              normal_behavior()
             }

         }
       }
     }

     def normal_behavior(MAS: ActorRef[IMessage])(implicit executionContext: ExecutionContext): Behavior[IMessage] = {
       Behaviors.setup { context =>

         val pool = Routers.pool(poolSize = 8)(
           Behaviors.supervise(Intention(executionContext)).onFailure[Exception](SupervisorStrategy.restart))

         val router = context.spawn(pool, "intention-pool")
         //MAS ! ReadyMessage(context.self)
         Behaviors.receive {
           (context, message) =>
             message match {
               case IntentionDoneMessage(s) =>
                 context.log.debug(f"${executionContext.name}: an intention was done by $s")
               case IntentionErrorMessage(c,s) =>
                 context.log.debug(f"${executionContext.name}: an intention was done by $s: $c")
               case SubGoalMessage(_, _, _) =>
                 router ! message.asInstanceOf[SubGoalMessage]
               case GoalMessage(m, ref) =>
                 m match {
                   case t: StructTerm =>
                     val subGoal = GoalParser.create_goal_message(t, ref)

                     if (subGoal.isDefined)
                       context.self ! subGoal.get
                     else
                       ref.asInstanceOf[AkkaMessageSource].src ! IntentionErrorMessage(NoPlanMessage(),AkkaMessageSource(executionContext.agent.self))


                 }

                case AskMessage(m, ref) =>
                                m match {
                                  case t: StructTerm =>
                                    val subGoal = GoalParser.create_test_message(t, ref)

                                    if (subGoal.isDefined)
                                      context.self ! subGoal.get
                                    else
                                      ref.asInstanceOf[AkkaMessageSource].src ! IntentionErrorMessage(NoPlanMessage(),AkkaMessageSource(executionContext.agent.self))
                                }
             case BeliefMessage(m, ref) =>
                  m match {
                    case t: StructTerm =>
                    if(executionContext.beliefBase.assertOne(t)) {
                      val subGoal = GoalParser.create_belief_message(t, ref)

                      if (subGoal.isDefined)
                        context.self ! subGoal.get
                    }
                  }

              case UnBeliefMessage(m, ref) =>
                   m match {
                     case t: StructTerm =>
                     if(executionContext.beliefBase.retractOne(t)) {
                       val subGoal = GoalParser.create_unbelief_message(t, ref)

                       if (subGoal.isDefined)
                         context.self ! subGoal.get
                     }
                   }
             }
             Behaviors.same
         }
       }
     }
   }



   object GoalParser extends IAgentGoalParser {
        override def create_goal_message(t: StructTerm, ref: IMessageSource) (implicit executionContext: ExecutionContext): Option[SubGoalMessage] = {
                                   if(t.matchOnlyFunctorAndArity("is_contradiction",3)) {
                                     val args: Parameters = Parameters(t.terms.toList)
                                     Option(SubGoalMessage(adopt_achievement_is_contradiction_3, args, ref))
                                   } else  
                                   if(t.matchOnlyFunctorAndArity("find_contradiction",2)) {
                                     val args: Parameters = Parameters(t.terms.toList)
                                     Option(SubGoalMessage(adopt_achievement_find_contradiction_2, args, ref))
                                   } else  
                                   if(t.matchOnlyFunctorAndArity("resolve_contradiction",2)) {
                                     val args: Parameters = Parameters(t.terms.toList)
                                     Option(SubGoalMessage(adopt_achievement_resolve_contradiction_2, args, ref))
                                   } else   {
                    Option.empty[SubGoalMessage]
                    }

                }

        override def create_belief_message(t: StructTerm, ref: IMessageSource) (implicit executionContext: ExecutionContext): Option[SubGoalMessage] = {
                      {
                    Option.empty[SubGoalMessage]
                    }

                }

         override def create_test_message(t: StructTerm, ref: IMessageSource) (implicit executionContext: ExecutionContext): Option[SubGoalMessage] = {
                              {
                            Option.empty[SubGoalMessage]
                            }
                        }
          override def create_unbelief_message(t: StructTerm, ref: IMessageSource) (implicit executionContext: ExecutionContext): Option[SubGoalMessage] = {
                                       {
                                     Option.empty[SubGoalMessage]
                                     }
                                 }



        }


      object adopt_achievement_is_contradiction_3 extends IGoal {

        def execute(params: Parameters) (implicit executionContext: ExecutionContext) : Unit = {
         var vars = VarMap()

         vars("Self").bind_to(StringTerm(executionContext.name))
         vars("Source").bind_to(StringTerm(executionContext.src.name))
         vars("Parent").bind_to(StringTerm(executionContext.parent.name))






                 //plan 0 start

                         vars.clear()
                         vars("Self").bind_to(StringTerm(executionContext.name))
                         vars("Source").bind_to(StringTerm(executionContext.src.name))
                         vars("Parent").bind_to(StringTerm(executionContext.parent.name))
                         vars +=(   "A" -> params.l_params(0))
                          vars +=(   "Rumour1" -> params.l_params(1))
                          vars +=(   "Rumour2" -> params.l_params(2))

                         val r0 = executionContext.beliefBase.query(StructTerm(",",Seq[GenericTerm](StructTerm("=",Seq[GenericTerm](vars("Rumour1"),StructTerm("accept",Seq[GenericTerm](vars("Offer"))))),StructTerm("=",Seq[GenericTerm](vars("Rumour2"),StructTerm("reject",Seq[GenericTerm](vars("Offer"),vars("_"))))))))

                         if (r0.result) {
                             r0.bindings foreach { case (k, v) =>
                            // vars += (k -> v.asInstanceOf[GenericTerm])
                                      vars(k).bind_to(v)
                             }
                             plan0(vars)
                             return
                          }

                          // plan 0 end



                 //plan 1 start

                         vars.clear()
                         vars("Self").bind_to(StringTerm(executionContext.name))
                         vars("Source").bind_to(StringTerm(executionContext.src.name))
                         vars("Parent").bind_to(StringTerm(executionContext.parent.name))
                         vars +=(   "A" -> params.l_params(0))
                          vars +=(   "Rumour1" -> params.l_params(1))
                          vars +=(   "Rumour2" -> params.l_params(2))

                         val r1 = executionContext.beliefBase.query(StructTerm(",",Seq[GenericTerm](StructTerm("=",Seq[GenericTerm](vars("Rumour1"),StructTerm("failure",Seq[GenericTerm](vars("A"),vars("Plan"))))),StructTerm("=",Seq[GenericTerm](vars("Rumour2"),StructTerm("succes",Seq[GenericTerm](vars("A"),vars("Plan"),vars("_"))))))))

                         if (r1.result) {
                             r1.bindings foreach { case (k, v) =>
                            // vars += (k -> v.asInstanceOf[GenericTerm])
                                      vars(k).bind_to(v)
                             }
                             plan1(vars)
                             return
                          }

                          // plan 1 end



                 //plan 2 start

                         vars.clear()
                         vars("Self").bind_to(StringTerm(executionContext.name))
                         vars("Source").bind_to(StringTerm(executionContext.src.name))
                         vars("Parent").bind_to(StringTerm(executionContext.parent.name))
                         vars +=(   "A" -> params.l_params(0))
                          vars +=(   "Rumour1" -> params.l_params(1))
                          vars +=(   "Rumour2" -> params.l_params(2))

                         val r2 = executionContext.beliefBase.query(StructTerm(",",Seq[GenericTerm](StructTerm("=",Seq[GenericTerm](vars("Rumour1"),StructTerm("mighthave",Seq[GenericTerm](vars("A"),vars("B"))))),StructTerm(";",Seq[GenericTerm](StructTerm("=",Seq[GenericTerm](vars("Rumour2"),StructTerm("hasnot",Seq[GenericTerm](vars("A"),vars("B"))))),StructTerm("=",Seq[GenericTerm](vars("Rumour2"),StructTerm("has",Seq[GenericTerm](vars("A"),vars("B"))))))))))

                         if (r2.result) {
                             r2.bindings foreach { case (k, v) =>
                            // vars += (k -> v.asInstanceOf[GenericTerm])
                                      vars(k).bind_to(v)
                             }
                             plan2(vars)
                             return
                          }

                          // plan 2 end



                 //plan 3 start

                         vars.clear()
                         vars("Self").bind_to(StringTerm(executionContext.name))
                         vars("Source").bind_to(StringTerm(executionContext.src.name))
                         vars("Parent").bind_to(StringTerm(executionContext.parent.name))
                         vars +=(   "A" -> params.l_params(0))
                          vars +=(   "Rumour1" -> params.l_params(1))
                          vars +=(   "Rumour2" -> params.l_params(2))

                         val r3 = executionContext.beliefBase.query(StructTerm(",",Seq[GenericTerm](StructTerm("=",Seq[GenericTerm](vars("Rumour1"),StructTerm("competent",Seq[GenericTerm](vars("A"),vars("B"))))),StructTerm("=",Seq[GenericTerm](vars("Rumour2"),StructTerm("incompetent",Seq[GenericTerm](vars("A"),vars("B"))))))))

                         if (r3.result) {
                             r3.bindings foreach { case (k, v) =>
                            // vars += (k -> v.asInstanceOf[GenericTerm])
                                      vars(k).bind_to(v)
                             }
                             plan3(vars)
                             return
                          }

                          // plan 3 end



                 //plan 4 start

                         vars.clear()
                         vars("Self").bind_to(StringTerm(executionContext.name))
                         vars("Source").bind_to(StringTerm(executionContext.src.name))
                         vars("Parent").bind_to(StringTerm(executionContext.parent.name))
                         vars +=(   "A" -> params.l_params(0))
                          vars +=(   "Rumour1" -> params.l_params(1))
                          vars +=(   "Rumour2" -> params.l_params(2))

                         val r4 = executionContext.beliefBase.query(StructTerm(",",Seq[GenericTerm](StructTerm("=",Seq[GenericTerm](vars("Rumour1"),StructTerm("unknown",Seq[GenericTerm](vars("A"),vars("B"))))),StructTerm(";",Seq[GenericTerm](StructTerm("=",Seq[GenericTerm](vars("Rumour2"),StructTerm("incompetent",Seq[GenericTerm](vars("A"),vars("B"))))),StructTerm("=",Seq[GenericTerm](vars("Rumour2"),StructTerm("competent",Seq[GenericTerm](vars("A"),vars("B"))))))))))

                         if (r4.result) {
                             r4.bindings foreach { case (k, v) =>
                            // vars += (k -> v.asInstanceOf[GenericTerm])
                                      vars(k).bind_to(v)
                             }
                             plan4(vars)
                             return
                          }

                          // plan 4 end



                 //plan 5 start

                         vars.clear()
                         vars("Self").bind_to(StringTerm(executionContext.name))
                         vars("Source").bind_to(StringTerm(executionContext.src.name))
                         vars("Parent").bind_to(StringTerm(executionContext.parent.name))
                         vars +=(   "A" -> params.l_params(0))
                          vars +=(   "Rumour1" -> params.l_params(1))
                          vars +=(   "Rumour2" -> params.l_params(2))

                         val r5 = executionContext.beliefBase.query(StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm("=",Seq[GenericTerm](vars("Rumour1"),StructTerm("value",Seq[GenericTerm](vars("A"),vars("B"),vars("C"),vars("D"))))),StructTerm("=",Seq[GenericTerm](vars("Rumour2"),StructTerm("value",Seq[GenericTerm](vars("A"),vars("B"),vars("C"),vars("E"))))))),StructTerm("rumour",Seq[GenericTerm](vars("Source1"),vars("Rumour1"))))),StructTerm("rumour",Seq[GenericTerm](vars("Source2"),vars("Rumour2"))))),StructTerm("trust",Seq[GenericTerm](vars("Source1"),vars("X1"))))),StructTerm("trust",Seq[GenericTerm](vars("Source2"),vars("X2"))))),StructTerm("is",Seq[GenericTerm](vars("F"),StructTerm("/",Seq[GenericTerm](StructTerm("+",Seq[GenericTerm](StructTerm("*",Seq[GenericTerm](vars("X1"),vars("D"))),StructTerm("*",Seq[GenericTerm](vars("X2"),vars("E"))))),StructTerm("+",Seq[GenericTerm](vars("X1"),vars("X2"))))))))))

                         if (r5.result) {
                             r5.bindings foreach { case (k, v) =>
                            // vars += (k -> v.asInstanceOf[GenericTerm])
                                      vars(k).bind_to(v)
                             }
                             plan5(vars)
                             return
                          }

                          // plan 5 end



                 //plan 6 start

                         vars.clear()
                         vars("Self").bind_to(StringTerm(executionContext.name))
                         vars("Source").bind_to(StringTerm(executionContext.src.name))
                         vars("Parent").bind_to(StringTerm(executionContext.parent.name))
                         vars +=(   "A" -> params.l_params(0))
                          vars +=(   "Rumour1" -> params.l_params(1))
                          vars +=(   "Rumour2" -> params.l_params(2))

                         val r6 = executionContext.beliefBase.query(StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm("=",Seq[GenericTerm](vars("Rumour1"),StructTerm("principle",Seq[GenericTerm](vars("A"),vars("B"),vars("C"))))),StructTerm("=",Seq[GenericTerm](vars("Rumour2"),StructTerm("principle",Seq[GenericTerm](vars("A"),vars("B"),vars("D"))))))),StructTerm("rumour",Seq[GenericTerm](vars("Source1"),vars("Rumour1"))))),StructTerm("rumour",Seq[GenericTerm](vars("Source2"),vars("Rumour2"))))),StructTerm("trust",Seq[GenericTerm](vars("Source1"),vars("X1"))))),StructTerm("trust",Seq[GenericTerm](vars("Source2"),vars("X2"))))),StructTerm("is",Seq[GenericTerm](vars("E"),StructTerm("/",Seq[GenericTerm](StructTerm("+",Seq[GenericTerm](StructTerm("*",Seq[GenericTerm](vars("X1"),vars("C"))),StructTerm("*",Seq[GenericTerm](vars("X2"),vars("D"))))),StructTerm("+",Seq[GenericTerm](vars("X1"),vars("X2"))))))))))

                         if (r6.result) {
                             r6.bindings foreach { case (k, v) =>
                            // vars += (k -> v.asInstanceOf[GenericTerm])
                                      vars(k).bind_to(v)
                             }
                             plan6(vars)
                             return
                          }

                          // plan 6 end


             executionContext.src.asInstanceOf[AkkaMessageSource].address() ! IntentionErrorMessage(NoApplicablePlanMessage(),AkkaMessageSource(executionContext.agent.self))

        }


                      def plan0(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( ( (StringTerm("There was a contradiction between ") + vars("Rumour1"))  + StringTerm(" and "))  + vars("Rumour2"))  + StringTerm(".")) )))
                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("rumour",Seq[GenericTerm](vars("A"),vars("Rumour1")))),GoalParser)
                                          adopt_achievement_resolve_contradiction_2.execute(Parameters(List( vars("Rumour1") , vars("Rumour2")  )))


                     }
                      def plan1(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( ( (StringTerm("There was a contradiction between ") + vars("Rumour1"))  + StringTerm(" and "))  + vars("Rumour2"))  + StringTerm(".")) )))
                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("rumour",Seq[GenericTerm](vars("A"),vars("Rumour1")))),GoalParser)
                                          adopt_achievement_resolve_contradiction_2.execute(Parameters(List( vars("Rumour1") , vars("Rumour2")  )))


                     }
                      def plan2(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( ( (StringTerm("There was a contradiction between ") + vars("Rumour1"))  + StringTerm(" and "))  + vars("Rumour2"))  + StringTerm(".")) )))
                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("rumour",Seq[GenericTerm](vars("A"),vars("Rumour1")))),GoalParser)
                                          adopt_achievement_resolve_contradiction_2.execute(Parameters(List( vars("Rumour1") , vars("Rumour2")  )))


                     }
                      def plan3(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( ( (StringTerm("There was a contradiction between ") + vars("Rumour1"))  + StringTerm(" and "))  + vars("Rumour2"))  + StringTerm(".")) )))
                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("rumour",Seq[GenericTerm](vars("A"),vars("Rumour1")))),GoalParser)
                                          adopt_achievement_resolve_contradiction_2.execute(Parameters(List( vars("Rumour1") , vars("Rumour2")  )))


                     }
                      def plan4(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( ( (StringTerm("There was a contradiction between ") + vars("Rumour1"))  + StringTerm(" and "))  + vars("Rumour2"))  + StringTerm(".")) )))
                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("rumour",Seq[GenericTerm](vars("A"),vars("Rumour1")))),GoalParser)
                                          adopt_achievement_resolve_contradiction_2.execute(Parameters(List( vars("Rumour1") , vars("Rumour2")  )))


                     }
                      def plan5(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( ( (StringTerm("There was a contradiction between ") + vars("Rumour1"))  + StringTerm(" and "))  + vars("Rumour2"))  + StringTerm(".")) )))
                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("-", StructTerm("value",Seq[GenericTerm](vars("A"),vars("B"),vars("C"),vars("E")))),GoalParser)
                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("value",Seq[GenericTerm](vars("A"),vars("B"),vars("C"),vars("F")))),GoalParser)


                     }
                      def plan6(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( ( (StringTerm("There was a contradiction between ") + vars("Rumour1"))  + StringTerm(" and "))  + vars("Rumour2"))  + StringTerm(".")) )))
                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("-", StructTerm("principle",Seq[GenericTerm](vars("A"),vars("B"),vars("D")))),GoalParser)
                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("value",Seq[GenericTerm](vars("A"),vars("B"),vars("E")))),GoalParser)


                     }


      }

      object adopt_achievement_find_contradiction_2 extends IGoal {

        def execute(params: Parameters) (implicit executionContext: ExecutionContext) : Unit = {
         var vars = VarMap()

         vars("Self").bind_to(StringTerm(executionContext.name))
         vars("Source").bind_to(StringTerm(executionContext.src.name))
         vars("Parent").bind_to(StringTerm(executionContext.parent.name))






                 //plan 0 start

                         vars.clear()
                         vars("Self").bind_to(StringTerm(executionContext.name))
                         vars("Source").bind_to(StringTerm(executionContext.src.name))
                         vars("Parent").bind_to(StringTerm(executionContext.parent.name))
                         vars +=(   "A" -> params.l_params(0))
                          vars +=(   "Rumour1" -> params.l_params(1))

                             plan0(vars)
                             return
                          // plan 0 end


             executionContext.src.asInstanceOf[AkkaMessageSource].address() ! IntentionErrorMessage(NoApplicablePlanMessage(),AkkaMessageSource(executionContext.agent.self))

        }


                      def plan0(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                               val ex_L69578 = executionContext.beliefBase.bufferedQuery( StructTerm("rumour",Seq[GenericTerm](vars("B"),vars("L69578"))) )
                                               while (ex_L69578.hasNext) {
                                                   val sol_L69578 = ex_L69578.next
                                                   if(sol_L69578.result) {
                                                   vars += ("Rumour2" -> sol_L69578.bindings("L69578").asInstanceOf[GenericTerm])
                                                                       adopt_achievement_is_contradiction_3.execute(Parameters(List( vars("A") , vars("Rumour1") , vars("Rumour2")  )))

                                                   }
                                               }
                                           vars -= ("Rumour2")


                     }


      }

      object adopt_achievement_resolve_contradiction_2 extends IGoal {

        def execute(params: Parameters) (implicit executionContext: ExecutionContext) : Unit = {
         var vars = VarMap()

         vars("Self").bind_to(StringTerm(executionContext.name))
         vars("Source").bind_to(StringTerm(executionContext.src.name))
         vars("Parent").bind_to(StringTerm(executionContext.parent.name))






                 //plan 0 start

                         vars.clear()
                         vars("Self").bind_to(StringTerm(executionContext.name))
                         vars("Source").bind_to(StringTerm(executionContext.src.name))
                         vars("Parent").bind_to(StringTerm(executionContext.parent.name))
                         vars +=(   "Rumour1" -> params.l_params(0))
                          vars +=(   "Rumour2" -> params.l_params(1))

                         val r0 = executionContext.beliefBase.query(StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm("rumour",Seq[GenericTerm](vars("Source1"),vars("Rumour1"))),StructTerm("rumour",Seq[GenericTerm](vars("Source2"),vars("Rumour2"))))),StructTerm("trust",Seq[GenericTerm](vars("Source1"),vars("X1"))))),StructTerm("trust",Seq[GenericTerm](vars("Source2"),vars("X2"))))))

                         if (r0.result) {
                             r0.bindings foreach { case (k, v) =>
                            // vars += (k -> v.asInstanceOf[GenericTerm])
                                      vars(k).bind_to(v)
                             }
                             plan0(vars)
                             return
                          }

                          // plan 0 end


             executionContext.src.asInstanceOf[AkkaMessageSource].address() ! IntentionErrorMessage(NoApplicablePlanMessage(),AkkaMessageSource(executionContext.agent.self))

        }


                      def plan0(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( (StringTerm("There is a contradiction between ") + vars("Rumour1"))  + StringTerm(" and "))  + vars("Rumour2")) )))
                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => println(StringTerm("I will now decide which rumour to believe."))))
                                          if(( (vars("X1") > vars("X2")) ).holds) {
                                                                   BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("-", StructTerm("rumour",Seq[GenericTerm](vars("Source2"),vars("Rumour2")))),GoalParser)
                                                                  PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( (StringTerm("I believe ") + vars("Rumour1"))  + StringTerm(".")) )))

                                          }
                                                               else if(( (vars("X2") > vars("X1")) ).holds) {
                                                                                          BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("-", StructTerm("rumour",Seq[GenericTerm](vars("Source1"),vars("Rumour1")))),GoalParser)
                                                                                         PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( (StringTerm("I believe ") + vars("Rumour2"))  + StringTerm(".")) )))

                                                               }

                                           else {
                                                                  PrimitiveAction.execute(PrimitiveAction.Parameters(() => println(StringTerm("I dont know what to believe. Help!!"))))

                                           }


                     }


      }





 }
object reasoning_companion { 
   def create() = new reasoning().agentBuilder 
   def create(in_coms : AgentCommunicationLayer) = new reasoning(coms = in_coms).agentBuilder 
   def create(in_beliefBaseFactory: IBeliefBaseFactory) = new reasoning(beliefBaseFactory = in_beliefBaseFactory).agentBuilder 
   def create(in_coms : AgentCommunicationLayer, in_beliefBaseFactory: IBeliefBaseFactory) = new reasoning(coms = in_coms, beliefBaseFactory = in_beliefBaseFactory).agentBuilder 
} 
