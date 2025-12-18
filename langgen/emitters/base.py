class Emitter:
    INDENT = " " * 4

    def emit(self, node, level=0):
        if isinstance(node, str):
            raise TypeError(f"AST error: string místo uzlu: {node}")

        method = f"emit_{node['type']}"
        if not hasattr(self, method):
            raise NotImplementedError(
                f"{self.__class__.__name__} neumí {node['type']}"
            )
        return getattr(self, method)(node, level)


    def i(self, level):
        return self.INDENT * level
