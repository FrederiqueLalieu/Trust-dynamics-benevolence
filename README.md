# Model of benevolence dynamics in Trust
## About
This repository contains the implementation of the model of trust dynamics that was introduced in the paper 'Model of the influence of external signals on the trust of the agent in Multi Agent System' by Lalieu, Zurek and Engers. With this code, the user can execute and modify an example of the updating of benevolence estimation. It is implemented using the Multi-Agent System framework created by Mohajeri. The main project source is available here: [https://github.com/mostafamohajeri/agentscript](https://github.com/mostafamohajeri/agentscript). The language of ASC2 is based on [AgentSpeak(L)/Jason](https://github.com/jason-lang/jason). The structure of the code, as well as instructions in this README are based on Mohajeri's repository:  [https://github.com/mostafamohajeri/asc2-java-examples](https://github.com/mostafamohajeri/asc2-java-examples).

## How to use

Requirements:
* Java 11+
* sbt or maven

The project can be compiled with `$ mvn clean generate-sources` which should result in creation of a package with one scala file in `target/generated-sources/` directory. This file is already present in the repository. If you want to modify or add the agents however, you should compile the project to create the necessary scala files.

To compile the agent, the following command can be used:

```$ mvn clean generate-sources scala:compile```

##### Utilizing the agents

Now the whole project can be simply compiled with
```
$ mvn clean generate-sources scala:compile compile
```
and by running the main file, the following output should be visible in the terminal: 
```
$ mvn generate-sources scala:compile compile  exec:java -Dexec.mainClass="MainMaven"
...
We are now updating according to offer1.
We dont know if offer1 is accepted. We will try to find out now.

....

The benefits of the old versus the new are 1 versus 0.1.
The new benevolence interval is [0.4, 0.9>.
...
```
