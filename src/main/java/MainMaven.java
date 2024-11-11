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
                                        new AgentRequest(asl.benevolence_companion.create(), "tom", 1)
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
