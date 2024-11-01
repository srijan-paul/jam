const f = () => 1
const f2 = () => {
	const f3 = () => {
		return 456;
	}
	return f3;
}
