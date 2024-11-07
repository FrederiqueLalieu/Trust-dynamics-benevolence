// COMPLETE
// IDEA: is het nodig om dingen waar je niks over weet op te slaan als [0,1]?
// Betekent 'I knów that I know nothing about it'

// Initial beliefs
intention(a, gardening).
succes(a, gardening).

intention(a, shopping).
succes(a, shopping).

intention(a, swimming).
failure(a, swimming, time).

subplan(swimming, driving).
subplan(gardening, weeding).

knowledge(driving, knowledge, 0.7).

resource(weeding, tools, 0.6).

resource(shopping, money, 0.5).
resource(shopping, bag, 0.4).
skill(shopping, preference, 0.3).
resource(shopping, time, 0.6).

resource(gardening, time, 0.7).
knowledge(gardening, edible, 0.8).
resource(gardening, tools, 0.2).

resource(swimming, time, 0.6).
skill(swimming, floating, 0.5).
knowledge(swimming, technique, 0.5).

// Initial goals
!update_competence(a).

// Plans
+!update_competence(A) =>

for (Action in succes(A, Action)) {
    #println(Action);
    !update_competence_succes(A, Action);};

for (Action in failure(A, Action, _)) {
    #println(Action);
    !update_competence_failures(A, Action);
}.

// Could be more efficient, with only one if statement
+!add_info(A, Action, Condition) :
    knowledge(Action, Condition, X) &&
    knowledge(A, Condition, Low, High) =>
    #println(A, Condition, X, 1);
    if ((X > Low) && (X =< High)) {
        +knowledge(A, Condition, X, High);}.

+!add_info(A, Action, Condition) :
    knowledge(Action, Condition, X) =>
    #println(A, Condition, X, 1);
    +knowledge(A, Condition, X, 1).

+!add_info(A, Action, Condition) :
    resource(Action, Condition, X) &&
    resource(A, Condition, Low, High) =>
    #println(A, Condition, X, High);
    if ((X > Low) && (X =< High)) {
        +resource(A, Condition, X, High);}.

+!add_info(A, Action, Condition) :
    resource(Action, Condition, X) =>
    #println(A, Condition, X, 1);
    +resource(A, Condition, X, 1).

+!add_info(A, Action, Condition) :
    skill(Action, Condition, X) &&
    skill(A, Condition, Low, High) =>
    #println(A, Condition, X, High);
    if ((X > Low) && (X =< High)) {
        +skill(A, Condition, X, High);}.

+!add_info(A, Action, Condition) :
    skill(Action, Condition, X) =>
    #println(A, Condition, X, 1);
    +skill(A, Condition, X, 1).

+!update_competence_succes(A, Action) :
    intention(A, Action) =>
    +competent(A, Action);
    for (Knowledge in knowledge(Action, Knowledge, X)) {
        !add_info(A, Action, Knowledge);
    };
    for (Resource in resource(Action, Resource, X)) {
        !add_info(A, Action, Resource);
    };
    for (Skill in skill(Action, Skill, X)) {
        !add_info(A, Action, Skill);
    };
    for (Subaction in subplan(Action, Subaction)) {
        #println(Subaction);
        +competent(A, Subaction);
        for (Knowledge in knowledge(subaction, Knowledge, X)) {
            #println(Knowledge, X);
            +knowledge(A, Knowledge, X);
        };
        for (Resource in resource(Subaction, Resource)) {
            #println(Resource);
            +resource(A, Resource, X);
        };
        for (Skill in skill(Subaction, Skill, X)) {
            #println(Skill);
            +skill(A, Skill, X);
        };
        }.

+!update_competence_failures(A, Action) :
    intention(A, Action) =>
    for (Reason in failure(A, Action, Reason)) {
        !update_competence_failure(A, Action, Reason);
    }.

// Update with priory to new beliefs
+!update_competence_failure(A, Action, Condition) :
    resource(Action, Condition, X) &&
    resource(A, Condition, Low, High) =>
    #println(A, Condition, Low, X);
    if ((X < High) && (X < Low)) {
        +resource(0, X);}
    else if (X < High) {
        +resource(A, Condition, Low, X);}.

+!update_competence_failure(A, Action, Condition) :
    resource(Action, Condition, X)=>
    #println(A, Condition, 0, X);
    +resource(A, Condition, 0, X).

+!update_competence_failure(A, Action, Condition) :
    knowledge(Action, Condition, X) &&
    knowledge(A, Condition, Low, High) =>
    #println(A, Condition, Low, X);
    if ((X < High) && (X < Low)) {
        +knowledge(0, X);}
    else if (X < High) {
        +knowledge(A, Condition, Low, X);}.

+!update_competence_failure(A, Action, Condition) :
    knowledge(Action, Condition, X)=>
    #println(A, Condition, 0, X);
    +knowledge(A, Condition, 0, X).

+!update_competence_failure(A, Action, Condition) :
    skill(Action, Condition, X) &&
    skill(A, Condition, Low, High) =>
    #println(A, Condition, Low, X);
    if ((X < High) && (X < Low)) {
        +resource(0, X);}
    else if (X < High) {
        +skill(A, Condition, Low, X);}.

+!update_competence_failure(A, Action, Condition) :
    skill(Action, Condition, X) =>
    #println(A, Condition, 0, X);
    +skill(A, Condition, 0, X).