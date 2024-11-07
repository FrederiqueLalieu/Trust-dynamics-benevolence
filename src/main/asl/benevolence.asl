// COMPLETE
// IDEA: this program can run into a contradiction, resolve it with reasoning

// Initial beliefs
offer(a, gardening, chilling, offer1).
reject(a, offer1, comfort).
reject(a, offer1, fun).

offer(a, cooking, shopping, offer2).
accept(a, offer2).

value(a, fun, shopping, 0.7).
value(a, fun, cooking, 0.3).
value(a, fun, chilling, 0.4).
value(a, fun, gardening, 0.3).

value(a, comfort, gardening, 0.4).
value(a, comfort, chilling, 0.2).

benevolence(a, fun, 0, 0.5).
benevolence(a, comfort, 0.2, 0.6).

// Initial goals
// !update_benevolence(a).

// Plans
+!update_benevolence(A) =>
    for (Offer in offer(A, NewPlan, OldPlan, Offer)) {
        #println("We are now updating according to " + Offer + ".");
        #println("We don't know if " + Offer + " is accepted. We will try to find out now.");
        !evaluate_offer(A, Offer);}.


+!evaluate_offer(A, Offer) :
    offer(A, NewPlan, OldPlan, Offer) &&
    accept(A, Offer) =>
    #println("The offer was accepted.");
    +intention(A, NewPlan);
    for (Value in value(A, Value, NewPlan, X1)) {
        #println("We will check if " + Value + " is relevant.");
        !update_benevolence_accept(A, Value, NewPlan, OldPlan);
    }.

+!evaluate_offer(A, Offer) :
    reject(A, Offer, _) =>
    #println("The offer was rejected.");
    for (Value in reject(Offer, Value)) {
                #println("It was rejected on the basis of " + Value + ".");
                !update_benevolence_reject(A, Offer, Value);
            }.

+!evaluate_offer(A, Offer) =>
    #println("It is not known yet whether " + A + " accepted or rejected the offer.").

+!update_benevolence_accept(A, Value, NewPlan, OldPlan) :
    value(A, Value, NewPlan, X1) &&
    value(A, Value, OldPlan, X2) &&
    benevolence(A, Value, Bmin, Bmax) =>
    #println("One of the relevant values was " + Value + ".");
    B = X2 - X1;
    if (B < 0) {
        #println("It seems " + A + " did not need to sacrifice " + Value + " to accept this plan.");
        -benevolence(A, Value, Bmin, Bmax);
        +benevolence(A, Value, 0, Bmax);
        #println("The previous interval was " + Bmin + ", " + Bmax);
        #println("The benefits of the old versus the new plan are " + X2 + " versus " + X1 + ".");
        #println("The new benevolence interval is " + B + ", " + Bmax + ".");
    } else if (B > Bmax) {
        #println("It seems " + A + " is even more benevolent than you thought was possible.");
        -benevolence(A, Value, Bmin, Bmax);
        +benevolence(A, Value, B, 1);
        #println("The previous interval was " + Bmin + ", " + Bmax + ".");
        #println("The benefits of the old versus the new plan are " + X2 + " versus " + X1 + ".");
        #println("The new benevolence interval is " + B + ", " + 1 + ".");
    } else {
        -benevolence(A, Value, Bmin, Bmax);
        +benevolence(A, Value, B, Bmax);
        #println("The previous interval was " + Bmin + ", " + Bmax + ".");
        #println("The benefits of the old versus the new plan are " + X2 + " versus " + X1 + ".");
        #println("The new benevolence interval is " + B + ", " + Bmax + ".");
    }.

+!update_benevolence_reject(A, Offer, Value) :
    offer(A, NewPlan, OldPlan, Offer) &&
    value(A, Value, NewPlan, X1) &&
    value(A, Value, OldPlan, X2) &&
    benevolence(A, Value, Bmin, Bmax) =>
    B = X2 - X1;
    if (B < 0) {
        #println("Either " + A + " did not actually reject the offer on the basis of " + Value + " or your assesment of his enjoyment of the plans is incorrect.");
        #println("His benevolence will therefore not be updated according to this information.");
    } else if (B < Bmin ) {
        #println("Your previous assesment of his benevolence seems to have been too high.");
        -benevolence(A, Value, Bmin, Bmax);
        +benevolence(A, Value, 0, B);
        #println("The offer was rejected on the basis of " + Value + ".");
        #println("The previous interval was " + Bmin + ", " + Bmax + ".");
        #println("The benefits of the old versus the new are " + X2 + " versus " + X1 + ".");
        #println("The new benevolence interval is " + Bmin + ", " + B + ".");
    }
    else {
        -benevolence(A, Value, Bmin, Bmax);
        +benevolence(A, Value, Bmin, B);
        #println("The offer was rejected on the basis of " + Value + ".");
        #println("The previous interval was " + Bmin + ", " + Bmax);
        #println("The benefits of the old versus the new are " + X2 + " versus " + X1 + ".");
        #println("The new benevolence interval is " + Bmin + ", " + B + ".");
    }.