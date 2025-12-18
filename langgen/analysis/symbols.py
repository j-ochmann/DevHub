class SymbolCollector:
    def __init__(self):
        self.variables = set()

    def collect(self, node):
        if not isinstance(node, dict):
            return

        node_type = node.get("type")
        if not node_type:
            return

        method = f"collect_{node_type}"
        if hasattr(self, method):
            getattr(self, method)(node)
        else:
            self.generic(node)

    # -------------------------
    # Konkrétní uzly
    # -------------------------

    def collect_program(self, node):
        for stmt in node.get("body", []):
            self.collect(stmt)

    def collect_var_decl(self, node):
        self.variables.add(node["name"])
        self.collect(node.get("value"))

    def collect_assign(self, node):
        self.variables.add(node["target"])
        self.collect(node.get("value"))

    def collect_if(self, node):
        self.collect(node.get("condition"))
        for stmt in node.get("then", []):
            self.collect(stmt)
        for stmt in node.get("else") or []:
            self.collect(stmt)

    def collect_while(self, node):
        self.collect(node.get("condition"))
        for stmt in node.get("body", []):
            self.collect(stmt)

    def collect_binary(self, node):
        self.collect(node.get("left"))
        self.collect(node.get("right"))

    def collect_print(self, node):
        self.collect(node.get("value"))

    # -------------------------
    # FALLBACK – KRITICKÁ ČÁST
    # -------------------------

    def generic(self, node):
        """
        Bezpečný fallback:
        projde všechny vnořené dict/list a sbírá dál
        """
        for v in node.values():
            if isinstance(v, dict):
                self.collect(v)
            elif isinstance(v, list):
                for i in v:
                    if isinstance(i, dict):
                        self.collect(i)

