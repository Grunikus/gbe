MEMORY_SIZE = 0x10000  # 64KB total memory

class Memory:

    def __init__(self):
        self.memory = [0x00] * MEMORY_SIZE

    def read_byte(self, address):
        return self.memory[address]

    def write_byte(self, address, value):
        self.memory[address] = value
