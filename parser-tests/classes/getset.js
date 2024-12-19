class A extends B.C {
	get x() { return 1 }
	get ['x']() { return 1 }
	set ['y'](x) { }
	set x(x) { return 1 }
	static get static() { return 1 }
	static set static(x) { }
}
