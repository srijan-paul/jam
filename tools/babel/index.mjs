import { parse } from "@babel/parser"
import fs from "node:fs"
import path from "node:path"

const subdirs = [
    "core", "es2015", "es2016", "es2017", "es2018",
    "es2019", "es2020", "es2021", "es2022", "es2024",
    "estree", "esprima", "comments", "jsx",
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
        delete ast['innerComments']

        // Jam does not parse regular expressions in source,
        // and doesn't plan to. They're only validated as per the
        // ECMAScript lexical grammar for `RegularExpression`.
        if (ast.type === "Literal")
            delete ast['regex']

        // "expression": false is redundant when
        // the type already says whether its a declaration or expression.
        if (ast.type === "FunctionDeclaration")
            delete ast['expression']

        if (ast.type == 'JSXExpressionContainer') {
           if (ast.expression.type == 'JSXEmptyExpression') {
                ast.expression = null;
           }
        }

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

/**
  * Recursively list all files in a directory and its subdirectories
  * @param {string} dirName - The directory to list files from
  */
function listFilesRecursive(dirName) {
    let files = [];
    function recurse(directory) {
        fs.readdirSync(directory).forEach(fileName => {
            const filePath = path.join(directory, fileName);
            if (fs.statSync(filePath).isDirectory()) {
                return recurse(filePath);
            }
            files.push(filePath);
        });
    }

    recurse(dirName);
    return files;
}


for (const subdir of subdirs) {
    const fullpath = path.join(process.cwd(), subdir)
    const jsFiles = listFilesRecursive(fullpath)
        .filter(file => {
            const ext = path.extname(file)
            return ext === ".js" || ext === ".jsx" || ext === ".mjs" || ext === ".cjs"
        })

    for (const jsFile of jsFiles) {
        if (!fs.statSync(jsFile).isFile()) continue;

        const parentDir = path.dirname(jsFile)
        const optionsFile = path.join(parentDir, "options.json")
        let options = { plugins: ["estree"] };
        if (subdir === "jsx") options.plugins.push("jsx")
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

        if (Array.isArray(ast.errors) && ast.errors.length > 0) {
            ast = { error: ast.errors.map(e => e.toString()).join("\n") }
        } else if (ast.error) {
            ast = { error: ast.error }
        }

        const astJson = JSON.stringify(ast.program ??  ast, (_, v) => typeof v === 'bigint' ? v.toString() : v, 2)
        const astFile = path.join(parentDir, "output.json")
        fs.writeFileSync(astFile, astJson, { encoding: "utf8" })
    }
}

