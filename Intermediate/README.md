A stack-based SKI evaluator slightly closer to the hardware truth.

We use explicit pointers and an explicit heap.

The next step is to move memory outside of the evaluator's internal state.

That is, we have to define explicit memory actions that happen *outside*
of the evaluator, at the evaluator's request.