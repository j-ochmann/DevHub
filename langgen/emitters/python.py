from emitters.base import Emitter

class PythonEmitter(Emitter):

    def emit_program(self, node, level):
        return "\n".join(
            self.emit(stmt, level) for stmt in node["body"]
        )

    def emit_var_decl(self, node, level):
        return f"{self.i(level)}{node['name']} = {self.emit(node['value'])}"

    def emit_assign(self, node, level):
        return f"{self.i(level)}{node['target']} = {self.emit(node['value'])}"

    def emit_print(self, node, level):
        return f"{self.i(level)}print({self.emit(node['value'])})"

    def emit_literal(self, node, level=0):
        return str(node["value"])

    def emit_identifier(self, node, level=0):
        return node["name"]

    def emit_binary(self, node, level=0):
        return f"{self.emit(node['left'])} {node['op']} {self.emit(node['right'])}"

    def emit_if(self, node, level):
        lines = []
        cond = self.emit(node["condition"])
        lines.append(f"{self.i(level)}if {cond}:")

        for stmt in node["then"]:
            lines.append(self.emit(stmt, level + 1))

        if node.get("else"):
            lines.append(f"{self.i(level)}else:")
            for stmt in node["else"]:
                lines.append(self.emit(stmt, level + 1))

        return "\n".join(lines)
