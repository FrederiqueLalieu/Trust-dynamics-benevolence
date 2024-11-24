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

 class tom  (coms: AgentCommunicationLayer = new  DefaultCommunications,
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

                   case tom.this.adopt_achievement_update_benevolence_1 =>
                     tom.this.adopt_achievement_update_benevolence_1.execute(message.params.asInstanceOf[Parameters])

                   case tom.this.adopt_achievement_evaluate_offer_2 =>
                     tom.this.adopt_achievement_evaluate_offer_2.execute(message.params.asInstanceOf[Parameters])

                   case tom.this.adopt_achievement_update_benevolence_accept_4 =>
                     tom.this.adopt_achievement_update_benevolence_accept_4.execute(message.params.asInstanceOf[Parameters])

                   case tom.this.adopt_achievement_update_benevolence_reject_3 =>
                     tom.this.adopt_achievement_update_benevolence_reject_3.execute(message.params.asInstanceOf[Parameters])


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

         override def agent_type: String = "tom"

         var vars = VarMap()

         def initGoals()(implicit executionContext: ExecutionContext) = List[StructTerm](
                     StructTerm("update_benevolence",Seq[GenericTerm]( StructTerm("a",Seq[GenericTerm]())  ))


         )

         def initBeliefs()(implicit executionContext: ExecutionContext) = List[StructTerm](
                     StructTerm("offer",Seq[GenericTerm](StructTerm("damian",Seq[GenericTerm]()),StructTerm("gardening",Seq[GenericTerm]()),StructTerm("repairing",Seq[GenericTerm]()),StructTerm("offer1",Seq[GenericTerm]())))
           ,
            StructTerm("accept",Seq[GenericTerm](StructTerm("damian",Seq[GenericTerm]()),StructTerm("offer1",Seq[GenericTerm]())))
           ,
            StructTerm("phi",Seq[GenericTerm](StructTerm("damian",Seq[GenericTerm]()),StructTerm("fun",Seq[GenericTerm]()),StructTerm("gardening",Seq[GenericTerm]()),DoubleTerm(0.9)))
           ,
            StructTerm("phi",Seq[GenericTerm](StructTerm("damian",Seq[GenericTerm]()),StructTerm("health",Seq[GenericTerm]()),StructTerm("gardening",Seq[GenericTerm]()),IntTerm(1)))
           ,
            StructTerm("phi",Seq[GenericTerm](StructTerm("damian",Seq[GenericTerm]()),StructTerm("fun",Seq[GenericTerm]()),StructTerm("repairing",Seq[GenericTerm]()),DoubleTerm(0.6)))
           ,
            StructTerm("phi",Seq[GenericTerm](StructTerm("damian",Seq[GenericTerm]()),StructTerm("health",Seq[GenericTerm]()),StructTerm("repairing",Seq[GenericTerm]()),DoubleTerm(0.6)))
           ,
            StructTerm("benevolence",Seq[GenericTerm](StructTerm("damian",Seq[GenericTerm]()),StructTerm("fun",Seq[GenericTerm]()),IntTerm(0),IntTerm(1)))
           ,
            StructTerm("benevolence",Seq[GenericTerm](StructTerm("damian",Seq[GenericTerm]()),StructTerm("health",Seq[GenericTerm]()),IntTerm(0),IntTerm(1)))
           ,
            StructTerm("offer",Seq[GenericTerm](StructTerm("damian",Seq[GenericTerm]()),StructTerm("gardening",Seq[GenericTerm]()),StructTerm("cleaning",Seq[GenericTerm]()),StructTerm("offer2",Seq[GenericTerm]())))
           ,
            StructTerm("reject",Seq[GenericTerm](StructTerm("damian",Seq[GenericTerm]()),StructTerm("offer2",Seq[GenericTerm]()),StructTerm("fun",Seq[GenericTerm]())))
           ,
            StructTerm("reject",Seq[GenericTerm](StructTerm("damian",Seq[GenericTerm]()),StructTerm("offer2",Seq[GenericTerm]()),StructTerm("health",Seq[GenericTerm]())))
           ,
            StructTerm("phi",Seq[GenericTerm](StructTerm("damian",Seq[GenericTerm]()),StructTerm("fun",Seq[GenericTerm]()),StructTerm("cleaning",Seq[GenericTerm]()),DoubleTerm(0.1)))
           ,
            StructTerm("phi",Seq[GenericTerm](StructTerm("damian",Seq[GenericTerm]()),StructTerm("health",Seq[GenericTerm]()),StructTerm("cleaning",Seq[GenericTerm]()),DoubleTerm(0.1)))


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
                                   if(t.matchOnlyFunctorAndArity("update_benevolence",1)) {
                                     val args: Parameters = Parameters(t.terms.toList)
                                     Option(SubGoalMessage(adopt_achievement_update_benevolence_1, args, ref))
                                   } else  
                                   if(t.matchOnlyFunctorAndArity("evaluate_offer",2)) {
                                     val args: Parameters = Parameters(t.terms.toList)
                                     Option(SubGoalMessage(adopt_achievement_evaluate_offer_2, args, ref))
                                   } else  
                                   if(t.matchOnlyFunctorAndArity("update_benevolence_accept",4)) {
                                     val args: Parameters = Parameters(t.terms.toList)
                                     Option(SubGoalMessage(adopt_achievement_update_benevolence_accept_4, args, ref))
                                   } else  
                                   if(t.matchOnlyFunctorAndArity("update_benevolence_reject",3)) {
                                     val args: Parameters = Parameters(t.terms.toList)
                                     Option(SubGoalMessage(adopt_achievement_update_benevolence_reject_3, args, ref))
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


      object adopt_achievement_update_benevolence_1 extends IGoal {

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

                                               val ex_L16532 = executionContext.beliefBase.bufferedQuery( StructTerm("offer",Seq[GenericTerm](vars("A"),vars("NewPlan"),vars("OldPlan"),vars("L16532"))) )
                                               while (ex_L16532.hasNext) {
                                                   val sol_L16532 = ex_L16532.next
                                                   if(sol_L16532.result) {
                                                   vars += ("Offer" -> sol_L16532.bindings("L16532").asInstanceOf[GenericTerm])
                                                                       PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( (StringTerm("We are now updating according to ") + vars("Offer"))  + StringTerm(".")) )))
                                                                       PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( (StringTerm("We dont know if ") + vars("Offer"))  + StringTerm(" is accepted. We will try to find out now.")) )))
                                                                       adopt_achievement_evaluate_offer_2.execute(Parameters(List( vars("A") , vars("Offer")  )))

                                                   }
                                               }
                                           vars -= ("Offer")


                     }


      }

      object adopt_achievement_evaluate_offer_2 extends IGoal {

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
                          vars +=(   "Offer" -> params.l_params(1))

                         val r0 = executionContext.beliefBase.query(StructTerm(",",Seq[GenericTerm](StructTerm("offer",Seq[GenericTerm](vars("A"),vars("NewPlan"),vars("OldPlan"),vars("Offer"))),StructTerm("accept",Seq[GenericTerm](vars("A"),vars("Offer"))))))

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
                          vars +=(   "Offer" -> params.l_params(1))

                         val r1 = executionContext.beliefBase.query(StructTerm("reject",Seq[GenericTerm](vars("A"),vars("Offer"),vars("_"))))

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
                          vars +=(   "Offer" -> params.l_params(1))

                             plan2(vars)
                             return
                          // plan 2 end


             executionContext.src.asInstanceOf[AkkaMessageSource].address() ! IntentionErrorMessage(NoApplicablePlanMessage(),AkkaMessageSource(executionContext.agent.self))

        }


                      def plan0(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => println(StringTerm("The offer was accepted."))))
                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("intention",Seq[GenericTerm](vars("A"),vars("NewPlan")))),GoalParser)
                                               val ex_L61350 = executionContext.beliefBase.bufferedQuery( StructTerm("value",Seq[GenericTerm](vars("A"),vars("L61350"),vars("NewPlan"),vars("X1"))) )
                                               while (ex_L61350.hasNext) {
                                                   val sol_L61350 = ex_L61350.next
                                                   if(sol_L61350.result) {
                                                   vars += ("Value" -> sol_L61350.bindings("L61350").asInstanceOf[GenericTerm])
                                                                       PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( (StringTerm("We will check if ") + vars("Value"))  + StringTerm(" is relevant.")) )))
                                                                       adopt_achievement_update_benevolence_accept_4.execute(Parameters(List( vars("A") , vars("Value") , vars("NewPlan") , vars("OldPlan")  )))

                                                   }
                                               }
                                           vars -= ("Value")


                     }
                      def plan1(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => println(StringTerm("The offer was rejected."))))
                                               val ex_L46296 = executionContext.beliefBase.bufferedQuery( StructTerm("reject",Seq[GenericTerm](vars("Offer"),vars("L46296"))) )
                                               while (ex_L46296.hasNext) {
                                                   val sol_L46296 = ex_L46296.next
                                                   if(sol_L46296.result) {
                                                   vars += ("Value" -> sol_L46296.bindings("L46296").asInstanceOf[GenericTerm])
                                                                       PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( (StringTerm("It was rejected on the basis of ") + vars("Value"))  + StringTerm(".")) )))
                                                                       adopt_achievement_update_benevolence_reject_3.execute(Parameters(List( vars("A") , vars("Offer") , vars("Value")  )))

                                                   }
                                               }
                                           vars -= ("Value")


                     }
                      def plan2(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( (StringTerm("It is not known yet whether ") + vars("A"))  + StringTerm(" accepted or rejected the offer.")) )))


                     }


      }

      object adopt_achievement_update_benevolence_accept_4 extends IGoal {

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
                          vars +=(   "Value" -> params.l_params(1))
                          vars +=(   "NewPlan" -> params.l_params(2))
                          vars +=(   "OldPlan" -> params.l_params(3))

                         val r0 = executionContext.beliefBase.query(StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm("value",Seq[GenericTerm](vars("A"),vars("Value"),vars("NewPlan"),vars("X1"))),StructTerm("value",Seq[GenericTerm](vars("A"),vars("Value"),vars("OldPlan"),vars("X2"))))),StructTerm("benevolence",Seq[GenericTerm](vars("A"),vars("Value"),vars("Bmin"),vars("Bmax"))))))

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

                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( (StringTerm("One of the relevant values was ") + vars("Value"))  + StringTerm(".")) )))
                                           vars += ("B" ->  (vars("X2") - vars("X1")) )
                                          if(( (vars("B") < IntTerm(0)) ).holds) {
                                                                  PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( ( (StringTerm("It seems ") + vars("A"))  + StringTerm(" did not need to sacrifice "))  + vars("Value"))  + StringTerm(" to accept this plan.")) )))
                                                                   BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("-", StructTerm("benevolence",Seq[GenericTerm](vars("A"),vars("Value"),vars("Bmin"),vars("Bmax")))),GoalParser)
                                                                   BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("benevolence",Seq[GenericTerm](vars("A"),vars("Value"),IntTerm(0),vars("Bmax")))),GoalParser)
                                                                  PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( (StringTerm("The previous interval was ") + vars("Bmin"))  + StringTerm(", "))  + vars("Bmax")) )))
                                                                  PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( ( (StringTerm("The benefits of the old versus the new plan are ") + vars("X2"))  + StringTerm(" versus "))  + vars("X1"))  + StringTerm(".")) )))
                                                                  PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( ( (StringTerm("The new benevolence interval is ") + vars("B"))  + StringTerm(", "))  + vars("Bmax"))  + StringTerm(".")) )))

                                          }
                                                               else if(( (vars("B") > vars("Bmax")) ).holds) {
                                                                                         PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( (StringTerm("It seems ") + vars("A"))  + StringTerm(" is even more benevolent than you thought was possible.")) )))
                                                                                          BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("-", StructTerm("benevolence",Seq[GenericTerm](vars("A"),vars("Value"),vars("Bmin"),vars("Bmax")))),GoalParser)
                                                                                          BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("benevolence",Seq[GenericTerm](vars("A"),vars("Value"),vars("B"),IntTerm(1)))),GoalParser)
                                                                                         PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( ( (StringTerm("The previous interval was ") + vars("Bmin"))  + StringTerm(", "))  + vars("Bmax"))  + StringTerm(".")) )))
                                                                                         PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( ( (StringTerm("The benefits of the old versus the new plan are ") + vars("X2"))  + StringTerm(" versus "))  + vars("X1"))  + StringTerm(".")) )))
                                                                                         PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( ( (StringTerm("The new benevolence interval is ") + vars("B"))  + StringTerm(", "))  + IntTerm(1))  + StringTerm(".")) )))

                                                               }

                                           else {
                                                                   BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("-", StructTerm("benevolence",Seq[GenericTerm](vars("A"),vars("Value"),vars("Bmin"),vars("Bmax")))),GoalParser)
                                                                   BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("benevolence",Seq[GenericTerm](vars("A"),vars("Value"),vars("B"),vars("Bmax")))),GoalParser)
                                                                  PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( ( (StringTerm("The previous interval was ") + vars("Bmin"))  + StringTerm(", "))  + vars("Bmax"))  + StringTerm(".")) )))
                                                                  PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( ( (StringTerm("The benefits of the old versus the new plan are ") + vars("X2"))  + StringTerm(" versus "))  + vars("X1"))  + StringTerm(".")) )))
                                                                  PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( ( (StringTerm("The new benevolence interval is ") + vars("B"))  + StringTerm(", "))  + vars("Bmax"))  + StringTerm(".")) )))

                                           }


                     }


      }

      object adopt_achievement_update_benevolence_reject_3 extends IGoal {

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
                          vars +=(   "Offer" -> params.l_params(1))
                          vars +=(   "Value" -> params.l_params(2))

                         val r0 = executionContext.beliefBase.query(StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm("offer",Seq[GenericTerm](vars("A"),vars("NewPlan"),vars("OldPlan"),vars("Offer"))),StructTerm("value",Seq[GenericTerm](vars("A"),vars("Value"),vars("NewPlan"),vars("X1"))))),StructTerm("value",Seq[GenericTerm](vars("A"),vars("Value"),vars("OldPlan"),vars("X2"))))),StructTerm("benevolence",Seq[GenericTerm](vars("A"),vars("Value"),vars("Bmin"),vars("Bmax"))))))

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

                                           vars += ("B" ->  (vars("X2") - vars("X1")) )
                                          if(( (vars("B") < IntTerm(0)) ).holds) {
                                                                  PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( ( (StringTerm("Either ") + vars("A"))  + StringTerm(" did not actually reject the offer on the basis of "))  + vars("Value"))  + StringTerm(" or your assesment of his enjoyment of the plans is incorrect.")) )))
                                                                  PrimitiveAction.execute(PrimitiveAction.Parameters(() => println(StringTerm("His benevolence will therefore not be updated according to this information."))))

                                          }
                                                               else if(( (vars("B") < vars("Bmin")) ).holds) {
                                                                                         PrimitiveAction.execute(PrimitiveAction.Parameters(() => println(StringTerm("Your previous assesment of his benevolence seems to have been too high."))))
                                                                                          BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("-", StructTerm("benevolence",Seq[GenericTerm](vars("A"),vars("Value"),vars("Bmin"),vars("Bmax")))),GoalParser)
                                                                                          BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("benevolence",Seq[GenericTerm](vars("A"),vars("Value"),IntTerm(0),vars("B")))),GoalParser)
                                                                                         PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( (StringTerm("The offer was rejected on the basis of ") + vars("Value"))  + StringTerm(".")) )))
                                                                                         PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( ( (StringTerm("The previous interval was ") + vars("Bmin"))  + StringTerm(", "))  + vars("Bmax"))  + StringTerm(".")) )))
                                                                                         PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( ( (StringTerm("The benefits of the old versus the new are ") + vars("X2"))  + StringTerm(" versus "))  + vars("X1"))  + StringTerm(".")) )))
                                                                                         PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( ( (StringTerm("The new benevolence interval is ") + vars("Bmin"))  + StringTerm(", "))  + vars("B"))  + StringTerm(".")) )))

                                                               }

                                           else {
                                                                   BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("-", StructTerm("benevolence",Seq[GenericTerm](vars("A"),vars("Value"),vars("Bmin"),vars("Bmax")))),GoalParser)
                                                                   BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("benevolence",Seq[GenericTerm](vars("A"),vars("Value"),vars("Bmin"),vars("B")))),GoalParser)
                                                                  PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( (StringTerm("The offer was rejected on the basis of ") + vars("Value"))  + StringTerm(".")) )))
                                                                  PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( (StringTerm("The previous interval was ") + vars("Bmin"))  + StringTerm(", "))  + vars("Bmax")) )))
                                                                  PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( ( (StringTerm("The benefits of the old versus the new are ") + vars("X2"))  + StringTerm(" versus "))  + vars("X1"))  + StringTerm(".")) )))
                                                                  PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( ( (StringTerm("The new benevolence interval is ") + vars("Bmin"))  + StringTerm(", "))  + vars("B"))  + StringTerm(".")) )))

                                           }


                     }


      }





 }
object tom_companion { 
   def create() = new tom().agentBuilder 
   def create(in_coms : AgentCommunicationLayer) = new tom(coms = in_coms).agentBuilder 
   def create(in_beliefBaseFactory: IBeliefBaseFactory) = new tom(beliefBaseFactory = in_beliefBaseFactory).agentBuilder 
   def create(in_coms : AgentCommunicationLayer, in_beliefBaseFactory: IBeliefBaseFactory) = new tom(coms = in_coms, beliefBaseFactory = in_beliefBaseFactory).agentBuilder 
} 
