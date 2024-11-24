file:///C:/Users/frede/Documents/GitHub/Trust-dynamics/src/main/java/MainMaven.java
### java.util.NoSuchElementException: next on empty iterator

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
uri: file:///C:/Users/frede/Documents/GitHub/Trust-dynamics/src/main/java/MainMaven.java
text:
```scala
import infrastructure.AgentRequest;
import infrastructure.AgentRequestMessage;
import infrastructure.MAS;
import akka.actor.typed.ActorSystem;
import scala.collection.immutable.Seq;
import java.util.List;

public class MainMaven {
    public static void main(String[] args) {

        // Create the system
        MAS mas = MAS.build();
        var system = ActorSystem.create(mas.applyDefaults(), "MAS");

        // Tell the system how many of which agent to add
        // Starts as soon as all have been initialized
        system.tell(
                AgentRequestMessage.apply(
                        toSeq(List.of(
                                        new AgentRequest(asl.tom_compan.create(), "benevolence", 1),
                                        new AgentRequest(asl.competence_companion.create(), "competence", 1),
                                        new AgentRequest(asl.predictability_companion.create(), "predictability", 1),
                                        new AgentRequest(asl.reasoning_companion.create(), "reasoning", 1)
                                )
                        ),
                        system
                )
        );

    }

    private static Seq<AgentRequest> toSeq(List<AgentRequest> l) {
        return scala.jdk.CollectionConverters.ListHasAsScala(l).asScala().toSeq();
    }
}

```



#### Error stacktrace:

```
scala.collection.Iterator$$anon$19.next(Iterator.scala:973)
	scala.collection.Iterator$$anon$19.next(Iterator.scala:971)
	scala.collection.mutable.MutationTracker$CheckedIterator.next(MutationTracker.scala:76)
	scala.collection.IterableOps.head(Iterable.scala:222)
	scala.collection.IterableOps.head$(Iterable.scala:222)
	scala.collection.AbstractIterable.head(Iterable.scala:935)
	dotty.tools.dotc.interactive.InteractiveDriver.run(InteractiveDriver.scala:164)
	dotty.tools.pc.MetalsDriver.run(MetalsDriver.scala:45)
	dotty.tools.pc.WithCompilationUnit.<init>(WithCompilationUnit.scala:31)
	dotty.tools.pc.SimpleCollector.<init>(PcCollector.scala:345)
	dotty.tools.pc.PcSemanticTokensProvider$Collector$.<init>(PcSemanticTokensProvider.scala:63)
	dotty.tools.pc.PcSemanticTokensProvider.Collector$lzyINIT1(PcSemanticTokensProvider.scala:63)
	dotty.tools.pc.PcSemanticTokensProvider.Collector(PcSemanticTokensProvider.scala:63)
	dotty.tools.pc.PcSemanticTokensProvider.provide(PcSemanticTokensProvider.scala:88)
	dotty.tools.pc.ScalaPresentationCompiler.semanticTokens$$anonfun$1(ScalaPresentationCompiler.scala:109)
```
#### Short summary: 

java.util.NoSuchElementException: next on empty iterator