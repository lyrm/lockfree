let ceil_pow_2_minus_1 n =
  let n = n lor (n lsr 1) in
  let n = n lor (n lsr 2) in
  let n = n lor (n lsr 4) in
  let n = n lor (n lsr 8) in
  let n = n lor (n lsr 16) in
  if Sys.int_size > 32 then n lor (n lsr 32) else n

let ceil_pow_2 n = ceil_pow_2_minus_1 (n - 1) + 1

let floor_pow_2 n =
  let m = ceil_pow_2_minus_1 n in
  m lxor (m lsr 1)
