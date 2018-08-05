# simmer.bricks 0.2.0

## New features

* Implement `delayed_release_selected()` (#9 addressing #4).

## Minor changes and fixes

* Fix `do_parallel()` to also accept lists of trajectories as input (#6).
* Simplify `delayed_release()` interface (as part of #9).
* Change `do_parallel()` interface: move the environment after the dots to force the user to name it (#10).
