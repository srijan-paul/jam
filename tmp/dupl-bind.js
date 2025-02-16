// ERROR: Variable 'x' has already been declared

let { y: x } = 456;
{
	var { x: x } = 123;
}

