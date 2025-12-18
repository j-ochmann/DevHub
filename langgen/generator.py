import json
from emitters.abap import ABAPEmitter
from emitters.cobol85 import Cobol85Emitter
from emitters.cpp import CppEmitter
from emitters.csharp import CSharpEmitter
from emitters.fortran77 import Fortran77Emitter
from emitters.python import PythonEmitter
from emitters.markdown import MarkdownEmitter

# ---- NAČTENÍ AST ----
with open("ast/example.json", "r") as f:
    ast = json.load(f)

# ---- EMITERY ----
code_emitters = {
    "ABAP": ABAPEmitter(),
    "COBOL": Cobol85Emitter(),
    "C++": CppEmitter(),
    "C#": CSharpEmitter(),
    "Python": PythonEmitter(),
    "Fortran": Fortran77Emitter(),
}

# ---- GENERACE MD ----
md = MarkdownEmitter(code_emitters)
print(md.emit_program(ast))

