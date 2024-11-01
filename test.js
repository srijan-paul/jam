// [  { x=1 }  ] = 1;
// ^  ^     ^    ^
// |  |     |    |
//can can  must  must

// ...; <- cannot be a MUST destruct.
// let x = (...) <- cannot be a MUST destruct.
// f(...) <- cannot be a MUST destruct.

[a, {b: {c = 1}}] = d

