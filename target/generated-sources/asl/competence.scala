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

 class competence  (coms: AgentCommunicationLayer = new  DefaultCommunications,
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

                   case competence.this.adopt_achievement_update_competence_1 =>
                     competence.this.adopt_achievement_update_competence_1.execute(message.params.asInstanceOf[Parameters])

                   case competence.this.adopt_achievement_add_info_3 =>
                     competence.this.adopt_achievement_add_info_3.execute(message.params.asInstanceOf[Parameters])

                   case competence.this.adopt_achievement_update_competence_succes_2 =>
                     competence.this.adopt_achievement_update_competence_succes_2.execute(message.params.asInstanceOf[Parameters])

                   case competence.this.adopt_achievement_update_competence_failures_2 =>
                     competence.this.adopt_achievement_update_competence_failures_2.execute(message.params.asInstanceOf[Parameters])

                   case competence.this.adopt_achievement_update_competence_failure_3 =>
                     competence.this.adopt_achievement_update_competence_failure_3.execute(message.params.asInstanceOf[Parameters])


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

         override def agent_type: String = "competence"

         var vars = VarMap()

         def initGoals()(implicit executionContext: ExecutionContext) = List[StructTerm](
                     StructTerm("update_competence",Seq[GenericTerm]( StructTerm("a",Seq[GenericTerm]())  ))


         )

         def initBeliefs()(implicit executionContext: ExecutionContext) = List[StructTerm](
                     StructTerm("intention",Seq[GenericTerm](StructTerm("a",Seq[GenericTerm]()),StructTerm("gardening",Seq[GenericTerm]())))
           ,
            StructTerm("succes",Seq[GenericTerm](StructTerm("a",Seq[GenericTerm]()),StructTerm("gardening",Seq[GenericTerm]())))
           ,
            StructTerm("intention",Seq[GenericTerm](StructTerm("a",Seq[GenericTerm]()),StructTerm("shopping",Seq[GenericTerm]())))
           ,
            StructTerm("succes",Seq[GenericTerm](StructTerm("a",Seq[GenericTerm]()),StructTerm("shopping",Seq[GenericTerm]())))
           ,
            StructTerm("intention",Seq[GenericTerm](StructTerm("a",Seq[GenericTerm]()),StructTerm("swimming",Seq[GenericTerm]())))
           ,
            StructTerm("failure",Seq[GenericTerm](StructTerm("a",Seq[GenericTerm]()),StructTerm("swimming",Seq[GenericTerm]()),StructTerm("time",Seq[GenericTerm]())))
           ,
            StructTerm("subplan",Seq[GenericTerm](StructTerm("swimming",Seq[GenericTerm]()),StructTerm("driving",Seq[GenericTerm]())))
           ,
            StructTerm("subplan",Seq[GenericTerm](StructTerm("gardening",Seq[GenericTerm]()),StructTerm("weeding",Seq[GenericTerm]())))
           ,
            StructTerm("knowledge",Seq[GenericTerm](StructTerm("driving",Seq[GenericTerm]()),StructTerm("knowledge",Seq[GenericTerm]()),DoubleTerm(0.7)))
           ,
            StructTerm("resource",Seq[GenericTerm](StructTerm("weeding",Seq[GenericTerm]()),StructTerm("tools",Seq[GenericTerm]()),DoubleTerm(0.6)))
           ,
            StructTerm("resource",Seq[GenericTerm](StructTerm("shopping",Seq[GenericTerm]()),StructTerm("money",Seq[GenericTerm]()),DoubleTerm(0.5)))
           ,
            StructTerm("resource",Seq[GenericTerm](StructTerm("shopping",Seq[GenericTerm]()),StructTerm("bag",Seq[GenericTerm]()),DoubleTerm(0.4)))
           ,
            StructTerm("skill",Seq[GenericTerm](StructTerm("shopping",Seq[GenericTerm]()),StructTerm("preference",Seq[GenericTerm]()),DoubleTerm(0.3)))
           ,
            StructTerm("resource",Seq[GenericTerm](StructTerm("shopping",Seq[GenericTerm]()),StructTerm("time",Seq[GenericTerm]()),DoubleTerm(0.6)))
           ,
            StructTerm("resource",Seq[GenericTerm](StructTerm("gardening",Seq[GenericTerm]()),StructTerm("time",Seq[GenericTerm]()),DoubleTerm(0.7)))
           ,
            StructTerm("knowledge",Seq[GenericTerm](StructTerm("gardening",Seq[GenericTerm]()),StructTerm("edible",Seq[GenericTerm]()),DoubleTerm(0.8)))
           ,
            StructTerm("resource",Seq[GenericTerm](StructTerm("gardening",Seq[GenericTerm]()),StructTerm("tools",Seq[GenericTerm]()),DoubleTerm(0.2)))
           ,
            StructTerm("resource",Seq[GenericTerm](StructTerm("swimming",Seq[GenericTerm]()),StructTerm("time",Seq[GenericTerm]()),DoubleTerm(0.6)))
           ,
            StructTerm("skill",Seq[GenericTerm](StructTerm("swimming",Seq[GenericTerm]()),StructTerm("floating",Seq[GenericTerm]()),DoubleTerm(0.5)))
           ,
            StructTerm("knowledge",Seq[GenericTerm](StructTerm("swimming",Seq[GenericTerm]()),StructTerm("technique",Seq[GenericTerm]()),DoubleTerm(0.5)))


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
                                   if(t.matchOnlyFunctorAndArity("update_competence",1)) {
                                     val args: Parameters = Parameters(t.terms.toList)
                                     Option(SubGoalMessage(adopt_achievement_update_competence_1, args, ref))
                                   } else  
                                   if(t.matchOnlyFunctorAndArity("add_info",3)) {
                                     val args: Parameters = Parameters(t.terms.toList)
                                     Option(SubGoalMessage(adopt_achievement_add_info_3, args, ref))
                                   } else  
                                   if(t.matchOnlyFunctorAndArity("update_competence_succes",2)) {
                                     val args: Parameters = Parameters(t.terms.toList)
                                     Option(SubGoalMessage(adopt_achievement_update_competence_succes_2, args, ref))
                                   } else  
                                   if(t.matchOnlyFunctorAndArity("update_competence_failures",2)) {
                                     val args: Parameters = Parameters(t.terms.toList)
                                     Option(SubGoalMessage(adopt_achievement_update_competence_failures_2, args, ref))
                                   } else  
                                   if(t.matchOnlyFunctorAndArity("update_competence_failure",3)) {
                                     val args: Parameters = Parameters(t.terms.toList)
                                     Option(SubGoalMessage(adopt_achievement_update_competence_failure_3, args, ref))
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


      object adopt_achievement_update_competence_1 extends IGoal {

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

                             plan0(vars)
                             return
                          // plan 0 end


             executionContext.src.asInstanceOf[AkkaMessageSource].address() ! IntentionErrorMessage(NoApplicablePlanMessage(),AkkaMessageSource(executionContext.agent.self))

        }


                      def plan0(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                               val ex_L66534 = executionContext.beliefBase.bufferedQuery( StructTerm("succes",Seq[GenericTerm](vars("A"),vars("L66534"))) )
                                               while (ex_L66534.hasNext) {
                                                   val sol_L66534 = ex_L66534.next
                                                   if(sol_L66534.result) {
                                                   vars += ("Action" -> sol_L66534.bindings("L66534").asInstanceOf[GenericTerm])
                                                                       PrimitiveAction.execute(PrimitiveAction.Parameters(() => println(vars("Action"))))
                                                                       adopt_achievement_update_competence_succes_2.execute(Parameters(List( vars("A") , vars("Action")  )))

                                                   }
                                               }
                                           vars -= ("Action")
                                               val ex_L75018 = executionContext.beliefBase.bufferedQuery( StructTerm("failure",Seq[GenericTerm](vars("A"),vars("L75018"),vars("_"))) )
                                               while (ex_L75018.hasNext) {
                                                   val sol_L75018 = ex_L75018.next
                                                   if(sol_L75018.result) {
                                                   vars += ("Action" -> sol_L75018.bindings("L75018").asInstanceOf[GenericTerm])
                                                                       PrimitiveAction.execute(PrimitiveAction.Parameters(() => println(vars("Action"))))
                                                                       adopt_achievement_update_competence_failures_2.execute(Parameters(List( vars("A") , vars("Action")  )))

                                                   }
                                               }
                                           vars -= ("Action")


                     }


      }

      object adopt_achievement_add_info_3 extends IGoal {

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
                          vars +=(   "Action" -> params.l_params(1))
                          vars +=(   "Condition" -> params.l_params(2))

                         val r0 = executionContext.beliefBase.query(StructTerm(",",Seq[GenericTerm](StructTerm("knowledge",Seq[GenericTerm](vars("Action"),vars("Condition"),vars("X"))),StructTerm("knowledge",Seq[GenericTerm](vars("A"),vars("Condition"),vars("Low"),vars("High"))))))

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
                          vars +=(   "Action" -> params.l_params(1))
                          vars +=(   "Condition" -> params.l_params(2))

                         val r1 = executionContext.beliefBase.query(StructTerm("knowledge",Seq[GenericTerm](vars("Action"),vars("Condition"),vars("X"))))

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
                          vars +=(   "Action" -> params.l_params(1))
                          vars +=(   "Condition" -> params.l_params(2))

                         val r2 = executionContext.beliefBase.query(StructTerm(",",Seq[GenericTerm](StructTerm("resource",Seq[GenericTerm](vars("Action"),vars("Condition"),vars("X"))),StructTerm("resource",Seq[GenericTerm](vars("A"),vars("Condition"),vars("Low"),vars("High"))))))

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
                          vars +=(   "Action" -> params.l_params(1))
                          vars +=(   "Condition" -> params.l_params(2))

                         val r3 = executionContext.beliefBase.query(StructTerm("resource",Seq[GenericTerm](vars("Action"),vars("Condition"),vars("X"))))

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
                          vars +=(   "Action" -> params.l_params(1))
                          vars +=(   "Condition" -> params.l_params(2))

                         val r4 = executionContext.beliefBase.query(StructTerm(",",Seq[GenericTerm](StructTerm("skill",Seq[GenericTerm](vars("Action"),vars("Condition"),vars("X"))),StructTerm("skill",Seq[GenericTerm](vars("A"),vars("Condition"),vars("Low"),vars("High"))))))

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
                          vars +=(   "Action" -> params.l_params(1))
                          vars +=(   "Condition" -> params.l_params(2))

                         val r5 = executionContext.beliefBase.query(StructTerm("skill",Seq[GenericTerm](vars("Action"),vars("Condition"),vars("X"))))

                         if (r5.result) {
                             r5.bindings foreach { case (k, v) =>
                            // vars += (k -> v.asInstanceOf[GenericTerm])
                                      vars(k).bind_to(v)
                             }
                             plan5(vars)
                             return
                          }

                          // plan 5 end


             executionContext.src.asInstanceOf[AkkaMessageSource].address() ! IntentionErrorMessage(NoApplicablePlanMessage(),AkkaMessageSource(executionContext.agent.self))

        }


                      def plan0(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => println(vars("A"),vars("Condition"),vars("X"),IntTerm(1))))
                                          if(( ( (vars("X") > vars("Low"))  &&  (vars("X") <= vars("High")) ) ).holds) {
                                                                   BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("knowledge",Seq[GenericTerm](vars("A"),vars("Condition"),vars("X"),vars("High")))),GoalParser)

                                          }


                     }
                      def plan1(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => println(vars("A"),vars("Condition"),vars("X"),IntTerm(1))))
                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("knowledge",Seq[GenericTerm](vars("A"),vars("Condition"),vars("X"),IntTerm(1)))),GoalParser)


                     }
                      def plan2(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => println(vars("A"),vars("Condition"),vars("X"),vars("High"))))
                                          if(( ( (vars("X") > vars("Low"))  &&  (vars("X") <= vars("High")) ) ).holds) {
                                                                   BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("resource",Seq[GenericTerm](vars("A"),vars("Condition"),vars("X"),vars("High")))),GoalParser)

                                          }


                     }
                      def plan3(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => println(vars("A"),vars("Condition"),vars("X"),IntTerm(1))))
                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("resource",Seq[GenericTerm](vars("A"),vars("Condition"),vars("X"),IntTerm(1)))),GoalParser)


                     }
                      def plan4(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => println(vars("A"),vars("Condition"),vars("X"),vars("High"))))
                                          if(( ( (vars("X") > vars("Low"))  &&  (vars("X") <= vars("High")) ) ).holds) {
                                                                   BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("skill",Seq[GenericTerm](vars("A"),vars("Condition"),vars("X"),vars("High")))),GoalParser)

                                          }


                     }
                      def plan5(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => println(vars("A"),vars("Condition"),vars("X"),IntTerm(1))))
                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("skill",Seq[GenericTerm](vars("A"),vars("Condition"),vars("X"),IntTerm(1)))),GoalParser)


                     }


      }

      object adopt_achievement_update_competence_succes_2 extends IGoal {

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
                          vars +=(   "Action" -> params.l_params(1))

                         val r0 = executionContext.beliefBase.query(StructTerm("intention",Seq[GenericTerm](vars("A"),vars("Action"))))

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

                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("competent",Seq[GenericTerm](vars("A"),vars("Action")))),GoalParser)
                                               val ex_L48588 = executionContext.beliefBase.bufferedQuery( StructTerm("knowledge",Seq[GenericTerm](vars("Action"),vars("L48588"),vars("X"))) )
                                               while (ex_L48588.hasNext) {
                                                   val sol_L48588 = ex_L48588.next
                                                   if(sol_L48588.result) {
                                                   vars += ("Knowledge" -> sol_L48588.bindings("L48588").asInstanceOf[GenericTerm])
                                                                       adopt_achievement_add_info_3.execute(Parameters(List( vars("A") , vars("Action") , vars("Knowledge")  )))

                                                   }
                                               }
                                           vars -= ("Knowledge")
                                               val ex_L52191 = executionContext.beliefBase.bufferedQuery( StructTerm("resource",Seq[GenericTerm](vars("Action"),vars("L52191"),vars("X"))) )
                                               while (ex_L52191.hasNext) {
                                                   val sol_L52191 = ex_L52191.next
                                                   if(sol_L52191.result) {
                                                   vars += ("Resource" -> sol_L52191.bindings("L52191").asInstanceOf[GenericTerm])
                                                                       adopt_achievement_add_info_3.execute(Parameters(List( vars("A") , vars("Action") , vars("Resource")  )))

                                                   }
                                               }
                                           vars -= ("Resource")
                                               val ex_L37446 = executionContext.beliefBase.bufferedQuery( StructTerm("skill",Seq[GenericTerm](vars("Action"),vars("L37446"),vars("X"))) )
                                               while (ex_L37446.hasNext) {
                                                   val sol_L37446 = ex_L37446.next
                                                   if(sol_L37446.result) {
                                                   vars += ("Skill" -> sol_L37446.bindings("L37446").asInstanceOf[GenericTerm])
                                                                       adopt_achievement_add_info_3.execute(Parameters(List( vars("A") , vars("Action") , vars("Skill")  )))

                                                   }
                                               }
                                           vars -= ("Skill")
                                               val ex_L27980 = executionContext.beliefBase.bufferedQuery( StructTerm("subplan",Seq[GenericTerm](vars("Action"),vars("L27980"))) )
                                               while (ex_L27980.hasNext) {
                                                   val sol_L27980 = ex_L27980.next
                                                   if(sol_L27980.result) {
                                                   vars += ("Subaction" -> sol_L27980.bindings("L27980").asInstanceOf[GenericTerm])
                                                                       PrimitiveAction.execute(PrimitiveAction.Parameters(() => println(vars("Subaction"))))
                                                                        BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("competent",Seq[GenericTerm](vars("A"),vars("Subaction")))),GoalParser)
                                                                            val ex_L25087 = executionContext.beliefBase.bufferedQuery( StructTerm("knowledge",Seq[GenericTerm](StructTerm("subaction",Seq[GenericTerm]()),vars("L25087"),vars("X"))) )
                                                                            while (ex_L25087.hasNext) {
                                                                                val sol_L25087 = ex_L25087.next
                                                                                if(sol_L25087.result) {
                                                                                vars += ("Knowledge" -> sol_L25087.bindings("L25087").asInstanceOf[GenericTerm])
                                                                                                    PrimitiveAction.execute(PrimitiveAction.Parameters(() => println(vars("Knowledge"),vars("X"))))
                                                                                                     BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("knowledge",Seq[GenericTerm](vars("A"),vars("Knowledge"),vars("X")))),GoalParser)

                                                                                }
                                                                            }
                                                                        vars -= ("Knowledge")
                                                                            val ex_L11182 = executionContext.beliefBase.bufferedQuery( StructTerm("resource",Seq[GenericTerm](vars("Subaction"),vars("L11182"))) )
                                                                            while (ex_L11182.hasNext) {
                                                                                val sol_L11182 = ex_L11182.next
                                                                                if(sol_L11182.result) {
                                                                                vars += ("Resource" -> sol_L11182.bindings("L11182").asInstanceOf[GenericTerm])
                                                                                                    PrimitiveAction.execute(PrimitiveAction.Parameters(() => println(vars("Resource"))))
                                                                                                     BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("resource",Seq[GenericTerm](vars("A"),vars("Resource"),vars("X")))),GoalParser)

                                                                                }
                                                                            }
                                                                        vars -= ("Resource")
                                                                            val ex_L99724 = executionContext.beliefBase.bufferedQuery( StructTerm("skill",Seq[GenericTerm](vars("Subaction"),vars("L99724"),vars("X"))) )
                                                                            while (ex_L99724.hasNext) {
                                                                                val sol_L99724 = ex_L99724.next
                                                                                if(sol_L99724.result) {
                                                                                vars += ("Skill" -> sol_L99724.bindings("L99724").asInstanceOf[GenericTerm])
                                                                                                    PrimitiveAction.execute(PrimitiveAction.Parameters(() => println(vars("Skill"))))
                                                                                                     BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("skill",Seq[GenericTerm](vars("A"),vars("Skill"),vars("X")))),GoalParser)

                                                                                }
                                                                            }
                                                                        vars -= ("Skill")

                                                   }
                                               }
                                           vars -= ("Subaction")


                     }


      }

      object adopt_achievement_update_competence_failures_2 extends IGoal {

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
                          vars +=(   "Action" -> params.l_params(1))

                         val r0 = executionContext.beliefBase.query(StructTerm("intention",Seq[GenericTerm](vars("A"),vars("Action"))))

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

                                               val ex_L37365 = executionContext.beliefBase.bufferedQuery( StructTerm("failure",Seq[GenericTerm](vars("A"),vars("Action"),vars("L37365"))) )
                                               while (ex_L37365.hasNext) {
                                                   val sol_L37365 = ex_L37365.next
                                                   if(sol_L37365.result) {
                                                   vars += ("Reason" -> sol_L37365.bindings("L37365").asInstanceOf[GenericTerm])
                                                                       adopt_achievement_update_competence_failure_3.execute(Parameters(List( vars("A") , vars("Action") , vars("Reason")  )))

                                                   }
                                               }
                                           vars -= ("Reason")


                     }


      }

      object adopt_achievement_update_competence_failure_3 extends IGoal {

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
                          vars +=(   "Action" -> params.l_params(1))
                          vars +=(   "Condition" -> params.l_params(2))

                         val r0 = executionContext.beliefBase.query(StructTerm(",",Seq[GenericTerm](StructTerm("resource",Seq[GenericTerm](vars("Action"),vars("Condition"),vars("X"))),StructTerm("resource",Seq[GenericTerm](vars("A"),vars("Condition"),vars("Low"),vars("High"))))))

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
                          vars +=(   "Action" -> params.l_params(1))
                          vars +=(   "Condition" -> params.l_params(2))

                         val r1 = executionContext.beliefBase.query(StructTerm("resource",Seq[GenericTerm](vars("Action"),vars("Condition"),vars("X"))))

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
                          vars +=(   "Action" -> params.l_params(1))
                          vars +=(   "Condition" -> params.l_params(2))

                         val r2 = executionContext.beliefBase.query(StructTerm(",",Seq[GenericTerm](StructTerm("knowledge",Seq[GenericTerm](vars("Action"),vars("Condition"),vars("X"))),StructTerm("knowledge",Seq[GenericTerm](vars("A"),vars("Condition"),vars("Low"),vars("High"))))))

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
                          vars +=(   "Action" -> params.l_params(1))
                          vars +=(   "Condition" -> params.l_params(2))

                         val r3 = executionContext.beliefBase.query(StructTerm("knowledge",Seq[GenericTerm](vars("Action"),vars("Condition"),vars("X"))))

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
                          vars +=(   "Action" -> params.l_params(1))
                          vars +=(   "Condition" -> params.l_params(2))

                         val r4 = executionContext.beliefBase.query(StructTerm(",",Seq[GenericTerm](StructTerm("skill",Seq[GenericTerm](vars("Action"),vars("Condition"),vars("X"))),StructTerm("skill",Seq[GenericTerm](vars("A"),vars("Condition"),vars("Low"),vars("High"))))))

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
                          vars +=(   "Action" -> params.l_params(1))
                          vars +=(   "Condition" -> params.l_params(2))

                         val r5 = executionContext.beliefBase.query(StructTerm("skill",Seq[GenericTerm](vars("Action"),vars("Condition"),vars("X"))))

                         if (r5.result) {
                             r5.bindings foreach { case (k, v) =>
                            // vars += (k -> v.asInstanceOf[GenericTerm])
                                      vars(k).bind_to(v)
                             }
                             plan5(vars)
                             return
                          }

                          // plan 5 end


             executionContext.src.asInstanceOf[AkkaMessageSource].address() ! IntentionErrorMessage(NoApplicablePlanMessage(),AkkaMessageSource(executionContext.agent.self))

        }


                      def plan0(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => println(vars("A"),vars("Condition"),vars("Low"),vars("X"))))
                                          if(( ( (vars("X") < vars("High"))  &&  (vars("X") < vars("Low")) ) ).holds) {
                                                                   BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("resource",Seq[GenericTerm](IntTerm(0),vars("X")))),GoalParser)

                                          }
                                                               else if(( (vars("X") < vars("High")) ).holds) {
                                                                                          BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("resource",Seq[GenericTerm](vars("A"),vars("Condition"),vars("Low"),vars("X")))),GoalParser)

                                                               }



                     }
                      def plan1(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => println(vars("A"),vars("Condition"),IntTerm(0),vars("X"))))
                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("resource",Seq[GenericTerm](vars("A"),vars("Condition"),IntTerm(0),vars("X")))),GoalParser)


                     }
                      def plan2(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => println(vars("A"),vars("Condition"),vars("Low"),vars("X"))))
                                          if(( ( (vars("X") < vars("High"))  &&  (vars("X") < vars("Low")) ) ).holds) {
                                                                   BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("knowledge",Seq[GenericTerm](IntTerm(0),vars("X")))),GoalParser)

                                          }
                                                               else if(( (vars("X") < vars("High")) ).holds) {
                                                                                          BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("knowledge",Seq[GenericTerm](vars("A"),vars("Condition"),vars("Low"),vars("X")))),GoalParser)

                                                               }



                     }
                      def plan3(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => println(vars("A"),vars("Condition"),IntTerm(0),vars("X"))))
                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("knowledge",Seq[GenericTerm](vars("A"),vars("Condition"),IntTerm(0),vars("X")))),GoalParser)


                     }
                      def plan4(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => println(vars("A"),vars("Condition"),vars("Low"),vars("X"))))
                                          if(( ( (vars("X") < vars("High"))  &&  (vars("X") < vars("Low")) ) ).holds) {
                                                                   BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("resource",Seq[GenericTerm](IntTerm(0),vars("X")))),GoalParser)

                                          }
                                                               else if(( (vars("X") < vars("High")) ).holds) {
                                                                                          BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("skill",Seq[GenericTerm](vars("A"),vars("Condition"),vars("Low"),vars("X")))),GoalParser)

                                                               }



                     }
                      def plan5(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => println(vars("A"),vars("Condition"),IntTerm(0),vars("X"))))
                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("skill",Seq[GenericTerm](vars("A"),vars("Condition"),IntTerm(0),vars("X")))),GoalParser)


                     }


      }





 }
object competence_companion { 
   def create() = new competence().agentBuilder 
   def create(in_coms : AgentCommunicationLayer) = new competence(coms = in_coms).agentBuilder 
   def create(in_beliefBaseFactory: IBeliefBaseFactory) = new competence(beliefBaseFactory = in_beliefBaseFactory).agentBuilder 
   def create(in_coms : AgentCommunicationLayer, in_beliefBaseFactory: IBeliefBaseFactory) = new competence(coms = in_coms, beliefBaseFactory = in_beliefBaseFactory).agentBuilder 
} 
