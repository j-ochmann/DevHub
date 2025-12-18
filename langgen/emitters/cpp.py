from emitters.base import Emitter


class CppEmitter(Emitter):
    """
    C++ code emitter (C++17-style, ale bez moderních fíčur,
    aby byl srozumitelný v učebnici).
    """

    INDENT = " " * 4

    # -------- PROGRAM --------

    def emit_program(self, node, level):
        lines = []
        lines.append("#include <iostream>")
        lines.append("")
        lines.append("int main()")
        lines.append("{")

        for stmt in node["body"]:
            lines.append(self.emit(stmt, level + 1))

        lines.append("")
        lines.append(f"{self.i(level + 1)}return 0;")
        lines.append("}")
        return "\n".join(lines)

    # -------- STATEMENTS --------

    def emit_var_decl(self, node, level):
        # Zatím jednoduché: int
        return f"{self.i(level)}int {node['name']} = {self.emit(node['value'])};"

    def emit_assign(self, node, level):
        return f"{self.i(level)}{node['target']} = {self.emit(node['value'])};"

    def emit_print(self, node, level):
        return (
            f"{self.i(level)}std::cout << "
            f"{self.emit(node['value'])} << std::endl;"
        )

    def emit_if(self, node, level):
        lines = []
        cond = self.emit(node["condition"])

        lines.append(f"{self.i(level)}if ({cond})")
        lines.append(f"{self.i(level)}{{")

        for stmt in node["then"]:
            lines.append(self.emit(stmt, level + 1))

        lines.append(f"{self.i(level)}}}")

        if node.get("else"):
            lines.append(f"{self.i(level)}else")
            lines.append(f"{self.i(level)}{{")
            for stmt in node["else"]:
                lines.append(self.emit(stmt, level + 1))
            lines.append(f"{self.i(level)}}}")

        return "\n".join(lines)

    # -------- EXPRESSIONS --------

    def emit_literal(self, node, level=0):
        # Později můžeš řešit stringy, bool, float…
        return str(node["value"])

    def emit_identifier(self, node, level=0):
        return node["name"]

    def emit_binary(self, node, level=0):
        left = self.emit(node["left"])
        right = self.emit(node["right"])
        return f"{left} {node['op']} {right}"

