// IDEAS
// - I want to store the confidince in the other beliefs

rumour(a, doctor(john)).
rumour(b, evil(john)).
rumour(a, reject(offer, fun)).

trust(a, 0.6).
trust(b, 0.7).

// !resolve_contradiction(doctor(john), evil(john)).
// !find_contradiction(b, accept(offer)).

+!is_contradiction(A, Rumour1, Rumour2) :
    Rumour1 = accept(Offer) &&
    Rumour2 = reject(Offer, _) =>
    #println("There was a contradiction between " + Rumour1 + " and " + Rumour2 + ".");
    +rumour(A, Rumour1);
    !resolve_contradiction(Rumour1, Rumour2).

+!is_contradiction(A, Rumour1, Rumour2) :
    Rumour1 = failure(A, Plan) &&
    Rumour2 = succes(A, Plan, _) =>
    #println("There was a contradiction between " + Rumour1 + " and " + Rumour2 + ".");
    +rumour(A, Rumour1);
    !resolve_contradiction(Rumour1, Rumour2).

+!is_contradiction(A, Rumour1, Rumour2) :
    Rumour1 = mighthave(A, B) &&
    (Rumour2 = hasnot(A, B) || Rumour2 = has(A,B)) =>
    #println("There was a contradiction between " + Rumour1 + " and " + Rumour2 + ".");
    +rumour(A, Rumour1);
    !resolve_contradiction(Rumour1, Rumour2).

+!is_contradiction(A, Rumour1, Rumour2) :
    Rumour1 = competent(A, B) &&
    Rumour2 = incompetent(A, B) =>
    #println("There was a contradiction between " + Rumour1 + " and " + Rumour2 + ".");
    +rumour(A, Rumour1);
    !resolve_contradiction(Rumour1, Rumour2).

+!is_contradiction(A, Rumour1, Rumour2) :
    Rumour1 = unknown(A, B) &&
    (Rumour2 = incompetent(A, B) || Rumour2 = competent(A, B)) =>
    #println("There was a contradiction between " + Rumour1 + " and " + Rumour2 + ".");
    +rumour(A, Rumour1);
    !resolve_contradiction(Rumour1, Rumour2).

+!is_contradiction(A, Rumour1, Rumour2) :
    Rumour1 = value(A, B, C, D) &&
    Rumour2 = value(A, B, C, E) &&
    rumour(Source1, Rumour1) &&
    rumour(Source2, Rumour2) &&
    trust(Source1, X1) &&
    trust(Source2, X2) &&
    F is (X1 * D + X2 * E)/(X1+X2)=>
    #println("There was a contradiction between " + Rumour1 + " and " + Rumour2 + ".");
    -value(A, B, C, E);
    +value(A, B, C, F).

+!is_contradiction(A, Rumour1, Rumour2) :
    Rumour1 = principle(A, B, C) &&
    Rumour2 = principle(A, B, D) &&
    rumour(Source1, Rumour1) &&
    rumour(Source2, Rumour2) &&
    trust(Source1, X1) &&
    trust(Source2, X2) &&
    E is (X1 * C + X2 * D)/(X1+X2)=>
    #println("There was a contradiction between " + Rumour1 + " and " + Rumour2 + ".");
    -principle(A, B, D);
    +value(A, B, E).

+!find_contradiction(A, Rumour1) =>
    for (Rumour2 in rumour(B, Rumour2)) {
        !is_contradiction(A, Rumour1, Rumour2);
    }.

+!resolve_contradiction(Rumour1, Rumour2) :
    rumour(Source1, Rumour1) &&
    rumour(Source2, Rumour2) &&
    trust(Source1, X1) &&
    trust(Source2, X2) =>
    #println("There is a contradiction between " + Rumour1 + " and " + Rumour2);
    #println("I will now decide which rumour to believe.");
    if (X1 > X2) {
        -rumour(Source2, Rumour2);
        #println("I believe " + Rumour1 + ".");
    } else if (X2 > X1) {
        -rumour(Source1, Rumour1);
        #println("I believe " + Rumour2 + ".");
    } else {
        #println("I don't know what to believe. Help!!");
    }.