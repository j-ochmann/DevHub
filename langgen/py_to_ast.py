import ast
import json
import sys

# -------------------------
# Funkce pro převod Python AST → naše AST
# -------------------------
def convert(node):
    if isinstance(node, ast.Module):
        return {"type": "program", "body": [convert(n) for n in node.body]}
    
    elif isinstance(node, ast.Assign):
        if not isinstance(node.targets[0], ast.Name):
            raise NotImplementedError("Složené přiřazení není podporováno")
        target_name = node.targets[0].id

        return {
            "type": "var_decl",
            "name": target_name,
            "value": convert(node.value)
        }

    elif isinstance(node, ast.AugAssign):
        # x += 1 → x = x + 1
        return {
            "type": "assign",
            "target": node.target.id,
            "value": {
                "type": "binary",
                "op": type(node.op).__name__,
                "left": {"type": "identifier", "name": node.target.id},
                "right": convert(node.value)
            }
        }

    elif isinstance(node, ast.Constant):
        return {"type": "literal", "value": node.value}

    elif isinstance(node, ast.Name):
        return {"type": "identifier", "name": node.id}

    elif isinstance(node, ast.BinOp):
        op_map = {
            ast.Add: "+",
            ast.Sub: "-",
            ast.Mult: "*",
            ast.Div: "/",
            ast.Mod: "%",
        }
        op_type = type(node.op)
        op = op_map.get(op_type, "?")
        return {
            "type": "binary",
            "op": op,
            "left": convert(node.left),
            "right": convert(node.right)
        }

    elif isinstance(node, ast.Compare):
        if len(node.ops) != 1:
            raise NotImplementedError("Víceoperátorové porovnání není podporováno")
        op_map = {
            ast.Eq: "==",
            ast.NotEq: "!=",
            ast.Lt: "<",
            ast.LtE: "<=",
            ast.Gt: ">",
            ast.GtE: ">=",
        }
        op = op_map[type(node.ops[0])]
        return {
            "type": "binary",
            "op": op,
            "left": convert(node.left),
            "right": convert(node.comparators[0])
        }

    elif isinstance(node, ast.If):
        return {
            "type": "if",
            "condition": convert(node.test),
            "then": [convert(n) for n in node.body],
            "else": [convert(n) for n in node.orelse] if node.orelse else None
        }

    elif isinstance(node, ast.While):
        return {
            "type": "while",
            "condition": convert(node.test),
            "body": [convert(n) for n in node.body]
        }
    
    elif isinstance(node, ast.Expr):
        return convert(node.value)

    elif isinstance(node, ast.Call):
        if isinstance(node.func, ast.Name) and node.func.id == "print":
            if len(node.args) != 1:
                raise NotImplementedError("print() s více argumenty není podporován")
            return {"type": "print", "value": convert(node.args[0])}

    else:
        raise NotImplementedError(
            f"Nepodporovaný Python AST uzel: {type(node).__name__}"
        )

# -------------------------
# Hlavní část
# -------------------------
if len(sys.argv) != 2:
    print("Použití: python py_to_ast.py <soubor.py>")
    sys.exit(1)

filename = sys.argv[1]

with open(filename, "r", encoding="utf-8") as f:
    source = f.read()

py_ast = ast.parse(source)
our_ast = convert(py_ast)

# uložíme do JSON
out_file = filename.replace(".py", ".json")
with open(out_file, "w", encoding="utf-8") as f:
    json.dump(our_ast, f, indent=2)

print(f"AST vygenerováno: {out_file}")
