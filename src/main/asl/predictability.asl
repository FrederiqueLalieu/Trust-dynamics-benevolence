// COMPLETED
// Geen upper bound want is het maximum niet altijd 0? Je hebt nooit het bewijs dat iemand iets niet zal doen
// IDEA: handle the case dat niet voor elke value elk plan iets zegt

agent(tom).
agent(paula).

// Gebaseerd op vorige acties en eigen zeggen
principle(paula, p1, 0).
principle(paula, p2, 0.2).
principle(paula, p3, 0).
principle(paula, p4, 0).
principle(paula, p5, 0.2).

// So this should be based on plans
intention(paula, plan1).
intention(paula, plan2).

value(plan1, p1, 0.5).
value(plan1, p2, 0.6).
// value(plan1, p3, 0.7).
value(plan1, p4, 0.8).
value(plan1, p5, 0.9).

// value(plan2, p1, 0.9).
value(plan2, p2, 0.8).
value(plan2, p3, 0.7).
value(plan2, p4, 0.6).
value(plan2, p5, 0.5).

intention(tom, plan1).
intention(tom, plan2).

principle(tom, p1, 0.8).
principle(tom, p2, 0.2).
principle(tom, p3, 0.6).
principle(tom, p4, 0.4).
principle(tom, p5, 0.2).

weight(p1, 0.7).
weight(p2, 0.8).
weight(p3, 0.4).
weight(p4, 0.4).
weight(p5, 0.4).

sum(0).
weightSum(0).
threshold(0.7).

!update_predictability(tom).

+!update_predictability(Agent) =>
    for (X in principle(Agent, X, P)) {
        !get_principle_intention(Agent, X);
    };
    for (W in weight(X, W)) {
        !distanceWeight(Agent,W);
    };
    !normalizedDistance(Agent).

+!get_principle_intention(Agent, X) =>
for (Plan in intention(Agent, Plan)) {
    #println(Agent + " intends to do " + Plan + ".");
    !get_weight(Agent, X, Plan);
};
!distanceSum(Agent, X).

+!get_weight(Agent, X, Plan) :
    principle(Agent, X, P) &&
    value(Plan, X, Weight) &&
    intention(Agent, X, Weight2) &&
    D1 is (Weight - P)**2 &&
    D2 is (Weight2 - P) **2 &&
    D is (Weight - P) =>
    if (D1 > D2) {
        -intention(Agent, X, Weight2);
        +intention(Agent, X, Weight);
    #println("The intentions of " + Agent + " show that they deviate " + D + " from what I believed about how they value " + X + ".");
    }.

+!get_weight(Agent, X, Plan) :
    principle(Agent, X, P) &&
    value(Plan, X, Weight) &&
    D is (Weight - P) =>
    +intention(Agent, X, Weight);
    #println("The intentions of " + Agent + " show that they deviate " + D + " from what I believed about how they value " + X + ".").

// Als dat plan geen waarde voor dat principe heeft
+!get_weight(Agent, X, Plan) :
    principle(Agent, X, P) &&
    intention(Agent, X, Weight2) =>
    +intention(Agent, X, P);
    #println("The intentions of " + Agent + " do not tell me anything about how they value principle " + X + ". Their behaviour does not influence how I view their predictability.").

+!distanceSum(Agent, X) :
    principle(Agent, X, P) &&
    intention(Agent, X, I) &&
    weight(X, W) &&
    sum(CurrentSum) &&
    D is W*(((P-I))**2) &&
    NewSum is CurrentSum + D =>
    -sum(CurrentSum);
    +sum(NewSum).

+!distanceWeight(Agent, W):
    weightSum(CurrentSumWeight) &&
    NewSumWeight is CurrentSumWeight + W =>

    -weightSum(CurrentSumWeight);
    +weightSum(NewSumWeight).

+!normalizedDistance(Agent) :
    sum(Xtest) &&
    threshold(T) &&
    weightSum(Ytest) &&
    Predictability is (1 -(Xtest / Ytest)) => 
    if (1 -(Xtest / Ytest) > T) {
        #println("I think the predictability of " + Agent + " is: " + (1 -(Xtest / Ytest)) + ". That is at least " + T + " so I consider them predictable.");
    } else {
        #println("I think the predictability of " + Agent + " is: " + (1 -(Xtest / Ytest)) + ". That is lower than " + T + " so I consider them unpredictable.");
    };
    -sum(Xtest);
    +sum(0);
    -weightSum(Ytest);
    +weightSum(0);
    !renew_info(Agent, Predictability).

+!renew_info(Agent, Predictability) :
    predictability(Agent, OldPredictability)=>
    -predictability(Agent, OldPredictability);
    +predictability(Agent, Predictability).

+!renew_info(Agent, Predictability) =>
    +predictability(Agent, Predictability).