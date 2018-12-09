def mod_func(x1, x2, x3):
    print("Entering mymodule function with\n", x1, x2, x3);
    return x1

class SubModule:
    def forward(self, x1, x2):
        print("Entering mymodule SubModule with\n", x1, x2)
        return x2

class NNModule:
    def __init__(self):
        self.submodule = SubModule();

nn = NNModule()
