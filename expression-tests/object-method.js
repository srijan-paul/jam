//{"object_literal":[{"object_property":{"key":{"identifier":"a"},"value":{"identifier":"a"},"flags":{"is_method":false,"is_shorthand":true,"is_computed":false,"kind":"init"}}},{"object_property":{"key":{"literal":"\"b\""},"value":{"literal":"123"},"flags":{"is_method":false,"is_shorthand":false,"is_computed":true,"kind":"init"}}},{"object_property":{"key":{"identifier":"f"},"value":{"function_expression":{"parameters":{"parameters":[{"object_pattern":[{"object_property":{"key":{"identifier":"name"},"value":{"identifier":"name"},"flags":{"is_method":false,"is_shorthand":false,"is_computed":false,"kind":"init"}}}]}]},"body":{"block_statement":[{"expression_statement":{"call_expression":{"callee":{"member_expression":{"object":{"identifier":"console"},"property":"log"}},"arguments":{"arguments":[{"literal":"\"hi!\""},{"identifier":"name"}]}}}}]},"info":{"function":{"name":null,"flags":{"is_generator":false,"is_async":false,"is_arrow":false,"_":0}}}}},"flags":{"is_method":true,"is_shorthand":false,"is_computed":false,"kind":"init"}}}]}
({
	a,
	["b"]: 123,
	f({ name }) {
		console.log("hi!", name)
	}
})
