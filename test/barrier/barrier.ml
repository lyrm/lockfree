type t = { waiters : int Atomic.t; size : int; passed : int Atomic.t }

let create n = { waiters = Atomic.make 0; size = n; passed = Atomic.make 0 }

let wait_until_full { waiters; size; passed } =
  (* waiting for the barrier to release the previous group *)
  while Atomic.get waiters = size do
    Domain.cpu_relax ()
  done;
  (* Put in the waiter group *)
  Atomic.incr waiters;
  (* wait for enough waiters to arrive *)
  while Atomic.get waiters < size do
    Domain.cpu_relax ()
  done;
  (* Have passed. Increased [passed]. If last one to pass, reset the barrier.  *)
  if Atomic.fetch_and_add passed 1 = size - 1 then (
    Atomic.set passed 0;
    Atomic.set waiters 0)
