import { parse } from "@babel/parser"
import fs from "node:fs"
import path from "node:path"

const subdirs = [
	"core", "es2015", "es2016", "es2017", "es2018",
	"es2019", "es2020", "es2021", "es2022", "es2024",
	"estree", "esprima", "comments",
]

// taken and modified from https://github.com/babel/babel/issues/11239
function removeAstTrivia(ast) {
	if (Array.isArray(ast)) {
		ast.forEach(a => removeAstTrivia(a));
	} else if (typeof ast === 'object' && ast !== null) {
		delete ast['loc'];
		delete ast['extra']
		delete ast['directives']
		delete ast['directive']
		delete ast['range']
		delete ast['trailingComments']
		delete ast['leadingComments']

		// Jam does not parse regular expressions in source,
		// and doesn't plan to. They're only validated as per the
		// ECMAScript lexical grammar for `RegularExpression`.
		if (ast.type === "Literal")
			delete ast['regex']

		// "expression": false is redundant when
		// the type already says whether its a declaration or expression.
		if (ast.type === "FunctionDeclaration")
			delete ast['expression']

		if (ast.type === "FunctionExpression") {
			delete ast['expression']
			if (ast.id == null)
				delete ast['id']

			if (!Object.hasOwnProperty.call(ast, 'arrow')) {
				ast['arrow'] = false
			}
		}

		const values = Object.values(ast).filter(v => Array.isArray(v) || typeof v === 'object');
		removeAstTrivia(values);
	}
};

for (const subdir of subdirs) {
	const fullpath = path.join(process.cwd(), subdir)
	const jsFiles = await fs.promises.readdir(fullpath, { recursive: true, encoding: "utf8" })
	for (const jsFile_ of jsFiles) {
		const jsFile = path.join(fullpath, jsFile_)

		if (!fs.statSync(jsFile).isFile()) continue;

		const ext = path.extname(jsFile)
		if (ext !== ".js" && ext !== ".mjs") continue;

		const parentDir = path.dirname(jsFile)
		const optionsFile = path.join(parentDir, "options.json")
		let options = { plugins: ["estree"] };
		if (fs.existsSync(optionsFile)) {
			options = JSON.parse(fs.readFileSync(optionsFile, { encoding: "utf8" }))
			if (Array.isArray(options.plugins) && !options.plugins.includes("estree")) {
				options.plugins.push("estree")
			}
		}

		const code = fs.readFileSync(jsFile, { encoding: "utf8" })
		let ast;
		try {
			ast = parse(code, options)
			removeAstTrivia(ast)
		} catch (error) {
			ast = { error: error.toString() }
		}
		const astJson = JSON.stringify(ast.program, (_, v) => typeof v === 'bigint' ? v.toString() : v, 2)
		if (typeof astJson !== "string") continue

		const astFile = path.join(parentDir, "output.json")
		fs.writeFileSync(astFile, astJson, { encoding: "utf8" })
	}
}

