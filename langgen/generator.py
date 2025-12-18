import json
from emitters.python import PythonEmitter
from emitters.csharp import CSharpEmitter
from emitters.cpp import CppEmitter
from emitters.fortran77 import Fortran77Emitter
from emitters.markdown import MarkdownEmitter

# ---- NAČTENÍ AST ----
with open("ast/example.json", "r") as f:
    ast = json.load(f)

# ---- EMITERY ----
code_emitters = {
    "python": PythonEmitter(),
    "csharp": CSharpEmitter(),
    "cpp": CppEmitter(),
    "fortran77": Fortran77Emitter(),
}

# ---- GENERACE MD ----
md = MarkdownEmitter(code_emitters)
print(md.emit_program(ast))

