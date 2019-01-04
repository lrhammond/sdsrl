# Define a new class for an object
class Object:

    def __init__(self, id, x_pos=None, y_pos=None):
        self.id = id
        self.x_pos = x_pos
        self.y_pos = y_pos
        self.colour = None
        self.size = None
        self.shape = None
        self.exists = True
