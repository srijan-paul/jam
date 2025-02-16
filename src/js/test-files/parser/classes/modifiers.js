class A extends B.C {
	async static() { return await 0 };
	static async;
	static async = 2;
	static async ["f" + "gh"](){}
	static async f(){}
	static async *['f'](){}
	*fg(){}
	static async g(){} h() {}
	123(){}
	x = class {}
}

