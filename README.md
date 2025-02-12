# Model of benevolence dynamics in Trust
## About
This repository contains the implementation of the model of trust dynamics that was introduced in the paper 'Model of the influence of external signals on the trust of the agent in Multi Agent System' by Lalieu, Zurek and Engers. With this code, the user can execute and modify an example of the updating of benevolence estimation. It is implemented using the Multi-Agent System framework created by Mohajeri. The main project source is available here: [https://github.com/mostafamohajeri/agentscript](https://github.com/mostafamohajeri/agentscript). The language of ASC2 is based on [AgentSpeak(L)/Jason](https://github.com/jason-lang/jason). The structure of the code, as well as instructions in this README are based on Mohajeri's repository:  [https://github.com/mostafamohajeri/asc2-java-examples](https://github.com/mostafamohajeri/asc2-java-examples).

## How to use

Requirements:
* Java 11+
* sbt or maven

The project can be compiled with `$ mvn clean generate-sources` which should result in creation of a package with two scala files in `target/generated-sources/` directory.

To compile the agent, the following command can be used:

```$ mvn clean generate-sources scala:compile```

##### Utilizing the agents
To run these agents we need to create a system (MAS). The following is an example of how this can be done:
```java
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
                                    new AgentRequest(asl.pinger_companion.create(), "pinger", 1),
                                    new AgentRequest(asl.ponger_companion.create(), "ponger", 1)
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

Now the whole project can be simply compiled with
```
$ mvn clean generate-sources scala:compile compile
```
and by running the main file, the following output should be visible in the terminal: 
```
$ mvn generate-sources scala:compile compile  exec:java -Dexec.mainClass="MainMaven"
...

...
```
