from emitters.base import Emitter

class ABAPEmitter(Emitter):
    INDENT = "  "

    def emit_program(self, node, level):
        lines = []
        lines.append("REPORT ZEXAMPLE.")
        lines.append("")
        # deklarace jednoduché proměnné
        lines.append(f"{self.indent(1)}DATA x TYPE i VALUE 0.")
        lines.append("")

        for stmt in node["body"]:
            lines.append(self.emit(stmt, 1))

        return "\n".join(lines)

    # -------- Statements --------
    def emit_var_decl(self, node, level):
        return f"{self.indent(level)}DATA {node['name']} TYPE i VALUE {self.emit(node['value'])}."

    def emit_assign(self, node, level):
        return f"{self.indent(level)}{node['target']} = {self.emit(node['value'])}."

    def emit_print(self, node, level):
        return f"{self.indent(level)}WRITE {self.emit(node['value'])}."

    def emit_if(self, node, level):
        lines = []
        cond = self.emit(node["condition"])
        lines.append(f"{self.indent(level)}IF {cond}.")

        for stmt in node["then"]:
            lines.append(self.emit(stmt, level + 1))

        lines.append(f"{self.indent(level)}ENDIF.")
        return "\n".join(lines)

    # -------- Expressions --------
    def emit_literal(self, node, level=0):
        return str(node["value"])

    def emit_identifier(self, node, level=0):
        return node["name"].upper()

    def emit_binary(self, node, level=0):
        left = self.emit(node["left"])
        right = self.emit(node["right"])
        op = node["op"]
        if op == "==":
            op = "="
        elif op == "!=":
            op = "<>"
        return f"{left} {op} {right}"
        
    def emit_while(self, node, level=0):
        indent = self.indent(level)
        lines = []

        cond = self.emit(node["condition"])
        lines.append(f"{indent}WHILE {cond}.")

        for stmt in node.get("body", []):
            lines.append(self.emit(stmt, level + 1))

        lines.append(f"{indent}ENDWHILE.")
        return "\n".join(lines)
