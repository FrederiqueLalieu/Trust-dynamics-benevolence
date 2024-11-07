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

 class predictability  (coms: AgentCommunicationLayer = new  DefaultCommunications,
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

                   case predictability.this.adopt_achievement_update_predictability_1 =>
                     predictability.this.adopt_achievement_update_predictability_1.execute(message.params.asInstanceOf[Parameters])

                   case predictability.this.adopt_achievement_get_principle_intention_2 =>
                     predictability.this.adopt_achievement_get_principle_intention_2.execute(message.params.asInstanceOf[Parameters])

                   case predictability.this.adopt_achievement_get_weight_3 =>
                     predictability.this.adopt_achievement_get_weight_3.execute(message.params.asInstanceOf[Parameters])

                   case predictability.this.adopt_achievement_distanceSum_2 =>
                     predictability.this.adopt_achievement_distanceSum_2.execute(message.params.asInstanceOf[Parameters])

                   case predictability.this.adopt_achievement_distanceWeight_2 =>
                     predictability.this.adopt_achievement_distanceWeight_2.execute(message.params.asInstanceOf[Parameters])

                   case predictability.this.adopt_achievement_normalizedDistance_1 =>
                     predictability.this.adopt_achievement_normalizedDistance_1.execute(message.params.asInstanceOf[Parameters])

                   case predictability.this.adopt_achievement_renew_info_2 =>
                     predictability.this.adopt_achievement_renew_info_2.execute(message.params.asInstanceOf[Parameters])


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

         override def agent_type: String = "predictability"

         var vars = VarMap()

         def initGoals()(implicit executionContext: ExecutionContext) = List[StructTerm](
                     StructTerm("update_predictability",Seq[GenericTerm]( StructTerm("tom",Seq[GenericTerm]())  ))


         )

         def initBeliefs()(implicit executionContext: ExecutionContext) = List[StructTerm](
                     StructTerm("agent",Seq[GenericTerm](StructTerm("tom",Seq[GenericTerm]())))
           ,
            StructTerm("agent",Seq[GenericTerm](StructTerm("paula",Seq[GenericTerm]())))
           ,
            StructTerm("principle",Seq[GenericTerm](StructTerm("paula",Seq[GenericTerm]()),StructTerm("p1",Seq[GenericTerm]()),IntTerm(0)))
           ,
            StructTerm("principle",Seq[GenericTerm](StructTerm("paula",Seq[GenericTerm]()),StructTerm("p2",Seq[GenericTerm]()),DoubleTerm(0.2)))
           ,
            StructTerm("principle",Seq[GenericTerm](StructTerm("paula",Seq[GenericTerm]()),StructTerm("p3",Seq[GenericTerm]()),IntTerm(0)))
           ,
            StructTerm("principle",Seq[GenericTerm](StructTerm("paula",Seq[GenericTerm]()),StructTerm("p4",Seq[GenericTerm]()),IntTerm(0)))
           ,
            StructTerm("principle",Seq[GenericTerm](StructTerm("paula",Seq[GenericTerm]()),StructTerm("p5",Seq[GenericTerm]()),DoubleTerm(0.2)))
           ,
            StructTerm("intention",Seq[GenericTerm](StructTerm("paula",Seq[GenericTerm]()),StructTerm("plan1",Seq[GenericTerm]())))
           ,
            StructTerm("intention",Seq[GenericTerm](StructTerm("paula",Seq[GenericTerm]()),StructTerm("plan2",Seq[GenericTerm]())))
           ,
            StructTerm("value",Seq[GenericTerm](StructTerm("plan1",Seq[GenericTerm]()),StructTerm("p1",Seq[GenericTerm]()),DoubleTerm(0.5)))
           ,
            StructTerm("value",Seq[GenericTerm](StructTerm("plan1",Seq[GenericTerm]()),StructTerm("p2",Seq[GenericTerm]()),DoubleTerm(0.6)))
           ,
            StructTerm("value",Seq[GenericTerm](StructTerm("plan1",Seq[GenericTerm]()),StructTerm("p4",Seq[GenericTerm]()),DoubleTerm(0.8)))
           ,
            StructTerm("value",Seq[GenericTerm](StructTerm("plan1",Seq[GenericTerm]()),StructTerm("p5",Seq[GenericTerm]()),DoubleTerm(0.9)))
           ,
            StructTerm("value",Seq[GenericTerm](StructTerm("plan2",Seq[GenericTerm]()),StructTerm("p2",Seq[GenericTerm]()),DoubleTerm(0.8)))
           ,
            StructTerm("value",Seq[GenericTerm](StructTerm("plan2",Seq[GenericTerm]()),StructTerm("p3",Seq[GenericTerm]()),DoubleTerm(0.7)))
           ,
            StructTerm("value",Seq[GenericTerm](StructTerm("plan2",Seq[GenericTerm]()),StructTerm("p4",Seq[GenericTerm]()),DoubleTerm(0.6)))
           ,
            StructTerm("value",Seq[GenericTerm](StructTerm("plan2",Seq[GenericTerm]()),StructTerm("p5",Seq[GenericTerm]()),DoubleTerm(0.5)))
           ,
            StructTerm("intention",Seq[GenericTerm](StructTerm("tom",Seq[GenericTerm]()),StructTerm("plan1",Seq[GenericTerm]())))
           ,
            StructTerm("intention",Seq[GenericTerm](StructTerm("tom",Seq[GenericTerm]()),StructTerm("plan2",Seq[GenericTerm]())))
           ,
            StructTerm("principle",Seq[GenericTerm](StructTerm("tom",Seq[GenericTerm]()),StructTerm("p1",Seq[GenericTerm]()),DoubleTerm(0.8)))
           ,
            StructTerm("principle",Seq[GenericTerm](StructTerm("tom",Seq[GenericTerm]()),StructTerm("p2",Seq[GenericTerm]()),DoubleTerm(0.2)))
           ,
            StructTerm("principle",Seq[GenericTerm](StructTerm("tom",Seq[GenericTerm]()),StructTerm("p3",Seq[GenericTerm]()),DoubleTerm(0.6)))
           ,
            StructTerm("principle",Seq[GenericTerm](StructTerm("tom",Seq[GenericTerm]()),StructTerm("p4",Seq[GenericTerm]()),DoubleTerm(0.4)))
           ,
            StructTerm("principle",Seq[GenericTerm](StructTerm("tom",Seq[GenericTerm]()),StructTerm("p5",Seq[GenericTerm]()),DoubleTerm(0.2)))
           ,
            StructTerm("weight",Seq[GenericTerm](StructTerm("p1",Seq[GenericTerm]()),DoubleTerm(0.7)))
           ,
            StructTerm("weight",Seq[GenericTerm](StructTerm("p2",Seq[GenericTerm]()),DoubleTerm(0.8)))
           ,
            StructTerm("weight",Seq[GenericTerm](StructTerm("p3",Seq[GenericTerm]()),DoubleTerm(0.4)))
           ,
            StructTerm("weight",Seq[GenericTerm](StructTerm("p4",Seq[GenericTerm]()),DoubleTerm(0.4)))
           ,
            StructTerm("weight",Seq[GenericTerm](StructTerm("p5",Seq[GenericTerm]()),DoubleTerm(0.4)))
           ,
            StructTerm("sum",Seq[GenericTerm](IntTerm(0)))
           ,
            StructTerm("weightSum",Seq[GenericTerm](IntTerm(0)))
           ,
            StructTerm("threshold",Seq[GenericTerm](DoubleTerm(0.7)))


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
                                   if(t.matchOnlyFunctorAndArity("update_predictability",1)) {
                                     val args: Parameters = Parameters(t.terms.toList)
                                     Option(SubGoalMessage(adopt_achievement_update_predictability_1, args, ref))
                                   } else  
                                   if(t.matchOnlyFunctorAndArity("get_principle_intention",2)) {
                                     val args: Parameters = Parameters(t.terms.toList)
                                     Option(SubGoalMessage(adopt_achievement_get_principle_intention_2, args, ref))
                                   } else  
                                   if(t.matchOnlyFunctorAndArity("get_weight",3)) {
                                     val args: Parameters = Parameters(t.terms.toList)
                                     Option(SubGoalMessage(adopt_achievement_get_weight_3, args, ref))
                                   } else  
                                   if(t.matchOnlyFunctorAndArity("distanceSum",2)) {
                                     val args: Parameters = Parameters(t.terms.toList)
                                     Option(SubGoalMessage(adopt_achievement_distanceSum_2, args, ref))
                                   } else  
                                   if(t.matchOnlyFunctorAndArity("distanceWeight",2)) {
                                     val args: Parameters = Parameters(t.terms.toList)
                                     Option(SubGoalMessage(adopt_achievement_distanceWeight_2, args, ref))
                                   } else  
                                   if(t.matchOnlyFunctorAndArity("normalizedDistance",1)) {
                                     val args: Parameters = Parameters(t.terms.toList)
                                     Option(SubGoalMessage(adopt_achievement_normalizedDistance_1, args, ref))
                                   } else  
                                   if(t.matchOnlyFunctorAndArity("renew_info",2)) {
                                     val args: Parameters = Parameters(t.terms.toList)
                                     Option(SubGoalMessage(adopt_achievement_renew_info_2, args, ref))
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


      object adopt_achievement_update_predictability_1 extends IGoal {

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
                         vars +=(   "Agent" -> params.l_params(0))

                             plan0(vars)
                             return
                          // plan 0 end


             executionContext.src.asInstanceOf[AkkaMessageSource].address() ! IntentionErrorMessage(NoApplicablePlanMessage(),AkkaMessageSource(executionContext.agent.self))

        }


                      def plan0(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                               val ex_L71068 = executionContext.beliefBase.bufferedQuery( StructTerm("principle",Seq[GenericTerm](vars("Agent"),vars("L71068"),vars("P"))) )
                                               while (ex_L71068.hasNext) {
                                                   val sol_L71068 = ex_L71068.next
                                                   if(sol_L71068.result) {
                                                   vars += ("X" -> sol_L71068.bindings("L71068").asInstanceOf[GenericTerm])
                                                                       adopt_achievement_get_principle_intention_2.execute(Parameters(List( vars("Agent") , vars("X")  )))

                                                   }
                                               }
                                           vars -= ("X")
                                               val ex_L10583 = executionContext.beliefBase.bufferedQuery( StructTerm("weight",Seq[GenericTerm](vars("X"),vars("L10583"))) )
                                               while (ex_L10583.hasNext) {
                                                   val sol_L10583 = ex_L10583.next
                                                   if(sol_L10583.result) {
                                                   vars += ("W" -> sol_L10583.bindings("L10583").asInstanceOf[GenericTerm])
                                                                       adopt_achievement_distanceWeight_2.execute(Parameters(List( vars("Agent") , vars("W")  )))

                                                   }
                                               }
                                           vars -= ("W")
                                          adopt_achievement_normalizedDistance_1.execute(Parameters(List( vars("Agent")  )))


                     }


      }

      object adopt_achievement_get_principle_intention_2 extends IGoal {

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
                         vars +=(   "Agent" -> params.l_params(0))
                          vars +=(   "X" -> params.l_params(1))

                             plan0(vars)
                             return
                          // plan 0 end


             executionContext.src.asInstanceOf[AkkaMessageSource].address() ! IntentionErrorMessage(NoApplicablePlanMessage(),AkkaMessageSource(executionContext.agent.self))

        }


                      def plan0(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                               val ex_L21117 = executionContext.beliefBase.bufferedQuery( StructTerm("intention",Seq[GenericTerm](vars("Agent"),vars("L21117"))) )
                                               while (ex_L21117.hasNext) {
                                                   val sol_L21117 = ex_L21117.next
                                                   if(sol_L21117.result) {
                                                   vars += ("Plan" -> sol_L21117.bindings("L21117").asInstanceOf[GenericTerm])
                                                                       PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( (vars("Agent") + StringTerm(" intends to do "))  + vars("Plan"))  + StringTerm(".")) )))
                                                                       adopt_achievement_get_weight_3.execute(Parameters(List( vars("Agent") , vars("X") , vars("Plan")  )))

                                                   }
                                               }
                                           vars -= ("Plan")
                                          adopt_achievement_distanceSum_2.execute(Parameters(List( vars("Agent") , vars("X")  )))


                     }


      }

      object adopt_achievement_get_weight_3 extends IGoal {

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
                         vars +=(   "Agent" -> params.l_params(0))
                          vars +=(   "X" -> params.l_params(1))
                          vars +=(   "Plan" -> params.l_params(2))

                         val r0 = executionContext.beliefBase.query(StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm("principle",Seq[GenericTerm](vars("Agent"),vars("X"),vars("P"))),StructTerm("value",Seq[GenericTerm](vars("Plan"),vars("X"),vars("Weight"))))),StructTerm("intention",Seq[GenericTerm](vars("Agent"),vars("X"),vars("Weight2"))))),StructTerm("is",Seq[GenericTerm](vars("D1"),StructTerm("**",Seq[GenericTerm](StructTerm("-",Seq[GenericTerm](vars("Weight"),vars("P"))),IntTerm(2))))))),StructTerm("is",Seq[GenericTerm](vars("D2"),StructTerm("**",Seq[GenericTerm](StructTerm("-",Seq[GenericTerm](vars("Weight2"),vars("P"))),IntTerm(2))))))),StructTerm("is",Seq[GenericTerm](vars("D"),StructTerm("-",Seq[GenericTerm](vars("Weight"),vars("P"))))))))

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
                         vars +=(   "Agent" -> params.l_params(0))
                          vars +=(   "X" -> params.l_params(1))
                          vars +=(   "Plan" -> params.l_params(2))

                         val r1 = executionContext.beliefBase.query(StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm("principle",Seq[GenericTerm](vars("Agent"),vars("X"),vars("P"))),StructTerm("value",Seq[GenericTerm](vars("Plan"),vars("X"),vars("Weight"))))),StructTerm("is",Seq[GenericTerm](vars("D"),StructTerm("-",Seq[GenericTerm](vars("Weight"),vars("P"))))))))

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
                         vars +=(   "Agent" -> params.l_params(0))
                          vars +=(   "X" -> params.l_params(1))
                          vars +=(   "Plan" -> params.l_params(2))

                         val r2 = executionContext.beliefBase.query(StructTerm(",",Seq[GenericTerm](StructTerm("principle",Seq[GenericTerm](vars("Agent"),vars("X"),vars("P"))),StructTerm("intention",Seq[GenericTerm](vars("Agent"),vars("X"),vars("Weight2"))))))

                         if (r2.result) {
                             r2.bindings foreach { case (k, v) =>
                            // vars += (k -> v.asInstanceOf[GenericTerm])
                                      vars(k).bind_to(v)
                             }
                             plan2(vars)
                             return
                          }

                          // plan 2 end


             executionContext.src.asInstanceOf[AkkaMessageSource].address() ! IntentionErrorMessage(NoApplicablePlanMessage(),AkkaMessageSource(executionContext.agent.self))

        }


                      def plan0(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                          if(( (vars("D1") > vars("D2")) ).holds) {
                                                                   BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("-", StructTerm("intention",Seq[GenericTerm](vars("Agent"),vars("X"),vars("Weight2")))),GoalParser)
                                                                   BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("intention",Seq[GenericTerm](vars("Agent"),vars("X"),vars("Weight")))),GoalParser)
                                                                  PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( ( ( ( (StringTerm("The intentions of ") + vars("Agent"))  + StringTerm(" show that they deviate "))  + vars("D"))  + StringTerm(" from what I believed about how they value "))  + vars("X"))  + StringTerm(".")) )))

                                          }


                     }
                      def plan1(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("intention",Seq[GenericTerm](vars("Agent"),vars("X"),vars("Weight")))),GoalParser)
                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( ( ( ( (StringTerm("The intentions of ") + vars("Agent"))  + StringTerm(" show that they deviate "))  + vars("D"))  + StringTerm(" from what I believed about how they value "))  + vars("X"))  + StringTerm(".")) )))


                     }
                      def plan2(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("intention",Seq[GenericTerm](vars("Agent"),vars("X"),vars("P")))),GoalParser)
                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( ( (StringTerm("The intentions of ") + vars("Agent"))  + StringTerm(" do not tell me anything about how they value principle "))  + vars("X"))  + StringTerm(". Their behaviour does not influence how I view their predictability.")) )))


                     }


      }

      object adopt_achievement_distanceSum_2 extends IGoal {

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
                         vars +=(   "Agent" -> params.l_params(0))
                          vars +=(   "X" -> params.l_params(1))

                         val r0 = executionContext.beliefBase.query(StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm("principle",Seq[GenericTerm](vars("Agent"),vars("X"),vars("P"))),StructTerm("intention",Seq[GenericTerm](vars("Agent"),vars("X"),vars("I"))))),StructTerm("weight",Seq[GenericTerm](vars("X"),vars("W"))))),StructTerm("sum",Seq[GenericTerm](vars("CurrentSum"))))),StructTerm("is",Seq[GenericTerm](vars("D"),StructTerm("*",Seq[GenericTerm](vars("W"),StructTerm("**",Seq[GenericTerm](StructTerm("-",Seq[GenericTerm](vars("P"),vars("I"))),IntTerm(2))))))))),StructTerm("is",Seq[GenericTerm](vars("NewSum"),StructTerm("+",Seq[GenericTerm](vars("CurrentSum"),vars("D"))))))))

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

                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("-", StructTerm("sum",Seq[GenericTerm](vars("CurrentSum")))),GoalParser)
                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("sum",Seq[GenericTerm](vars("NewSum")))),GoalParser)


                     }


      }

      object adopt_achievement_distanceWeight_2 extends IGoal {

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
                         vars +=(   "Agent" -> params.l_params(0))
                          vars +=(   "W" -> params.l_params(1))

                         val r0 = executionContext.beliefBase.query(StructTerm(",",Seq[GenericTerm](StructTerm("weightSum",Seq[GenericTerm](vars("CurrentSumWeight"))),StructTerm("is",Seq[GenericTerm](vars("NewSumWeight"),StructTerm("+",Seq[GenericTerm](vars("CurrentSumWeight"),vars("W"))))))))

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

                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("-", StructTerm("weightSum",Seq[GenericTerm](vars("CurrentSumWeight")))),GoalParser)
                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("weightSum",Seq[GenericTerm](vars("NewSumWeight")))),GoalParser)


                     }


      }

      object adopt_achievement_normalizedDistance_1 extends IGoal {

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
                         vars +=(   "Agent" -> params.l_params(0))

                         val r0 = executionContext.beliefBase.query(StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm("sum",Seq[GenericTerm](vars("Xtest"))),StructTerm("threshold",Seq[GenericTerm](vars("T"))))),StructTerm("weightSum",Seq[GenericTerm](vars("Ytest"))))),StructTerm("is",Seq[GenericTerm](vars("Predictability"),StructTerm("-",Seq[GenericTerm](IntTerm(1),StructTerm("/",Seq[GenericTerm](vars("Xtest"),vars("Ytest"))))))))))

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

                                          if(( ( (IntTerm(1) -  (vars("Xtest") / vars("Ytest")) )  > vars("T")) ).holds) {
                                                                  PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( ( ( ( (StringTerm("I think the predictability of ") + vars("Agent"))  + StringTerm(" is: "))  +  (IntTerm(1) -  (vars("Xtest") / vars("Ytest")) ) )  + StringTerm(". That is at least "))  + vars("T"))  + StringTerm(" so I consider them predictable.")) )))

                                          }
                                           else {
                                                                  PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( ( ( ( (StringTerm("I think the predictability of ") + vars("Agent"))  + StringTerm(" is: "))  +  (IntTerm(1) -  (vars("Xtest") / vars("Ytest")) ) )  + StringTerm(". That is lower than "))  + vars("T"))  + StringTerm(" so I consider them unpredictable.")) )))

                                           }
                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("-", StructTerm("sum",Seq[GenericTerm](vars("Xtest")))),GoalParser)
                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("sum",Seq[GenericTerm](IntTerm(0)))),GoalParser)
                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("-", StructTerm("weightSum",Seq[GenericTerm](vars("Ytest")))),GoalParser)
                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("weightSum",Seq[GenericTerm](IntTerm(0)))),GoalParser)
                                          adopt_achievement_renew_info_2.execute(Parameters(List( vars("Agent") , vars("Predictability")  )))


                     }


      }

      object adopt_achievement_renew_info_2 extends IGoal {

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
                         vars +=(   "Agent" -> params.l_params(0))
                          vars +=(   "Predictability" -> params.l_params(1))

                         val r0 = executionContext.beliefBase.query(StructTerm("predictability",Seq[GenericTerm](vars("Agent"),vars("OldPredictability"))))

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
                         vars +=(   "Agent" -> params.l_params(0))
                          vars +=(   "Predictability" -> params.l_params(1))

                             plan1(vars)
                             return
                          // plan 1 end


             executionContext.src.asInstanceOf[AkkaMessageSource].address() ! IntentionErrorMessage(NoApplicablePlanMessage(),AkkaMessageSource(executionContext.agent.self))

        }


                      def plan0(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("-", StructTerm("predictability",Seq[GenericTerm](vars("Agent"),vars("OldPredictability")))),GoalParser)
                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("predictability",Seq[GenericTerm](vars("Agent"),vars("Predictability")))),GoalParser)


                     }
                      def plan1(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("predictability",Seq[GenericTerm](vars("Agent"),vars("Predictability")))),GoalParser)


                     }


      }





 }
object predictability_companion { 
   def create() = new predictability().agentBuilder 
   def create(in_coms : AgentCommunicationLayer) = new predictability(coms = in_coms).agentBuilder 
   def create(in_beliefBaseFactory: IBeliefBaseFactory) = new predictability(beliefBaseFactory = in_beliefBaseFactory).agentBuilder 
   def create(in_coms : AgentCommunicationLayer, in_beliefBaseFactory: IBeliefBaseFactory) = new predictability(coms = in_coms, beliefBaseFactory = in_beliefBaseFactory).agentBuilder 
} 
